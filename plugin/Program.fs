// Learn more about F# at http://fsharp.org

open System
open Argu
open System.Net.WebSockets
open System.Threading
open System.Threading.Tasks
open FSharpx.Task
open FSharpx.Collections
open Chiron.Builder
open Chiron
open FSharpx
open Aether
open Aether.Operators

type Settings =
    { Minutes: Int16 option; CountdownText: string option; FinishedText: string option }
    static member FromJson(_: Settings): Json<Settings> = json {
        let settingsPrism =
            Json.Object_ >?> Map.key_ "settings"

        let minutePrism =
            settingsPrism >?> 
            Json.Object_ >?> Map.key_ "minutes" >?>
            Json.Number_

        let countdownPrism =
            settingsPrism >?>
            Json.Object_ >?> Map.key_ "countdownText" >?>
            Json.String_
            
        let finishedPrism =
            settingsPrism >?>
            Json.Object_ >?> Map.key_ "finishedText" >?>
            Json.String_

        let! minutes = Json.Optic.tryGet minutePrism
        let! countdownText = Json.Optic.tryGet countdownPrism
        let! finishedText = Json.Optic.tryGet finishedPrism
        return {
                Minutes = minutes |> Option.map (fun value -> int16 value)
                CountdownText = countdownText
                FinishedText = finishedText
            }
    }

type Event = 
    | DidReceiveSettings
    | KeyUp
    | WillAppear
    | Unknown of string

type Context = Context of string
type Device = Device of string
type Action = Action of string

type MailboxMessage =
    | PressedButton of Context * Settings
    | CountdownCompleted of Context

let websocket = new ClientWebSocket()
let task = TaskBuilder()
let payload (json: string) = ReadOnlyMemory((System.Text.UTF8Encoding()).GetBytes(json))

let sendToHost (websocket: ClientWebSocket) (json: string): Task<unit> =
    websocket.SendAsync(payload json, WebSocketMessageType.Text, true, CancellationToken.None).AsTask() |> ToTaskUnit

let writeFile (filename: string) (text: string): Task<unit> =
    System.IO.File.WriteAllTextAsync(filename, text) |> ToTaskUnit

let setTitle' (target: string) (Context context) (title: string): string =
    sprintf """
{
    "event": "setTitle",
    "context": "%s",
    "payload": {
        "title": "%s",
        "target": "%s"
    }
}""" context title target

let setTitle = setTitle' "both"

let log (message: string): unit =
    let json =
        sprintf """
        {
            "event": "logMessage",
            "payload": {
                "message": "[SDFS]: %s"
            }
        }""" (System.Web.HttpUtility.JavaScriptStringEncode message)
    try
        (sendToHost websocket json).Result
    with
        | _ -> ()

type Message =
    { Event: Event
      Context: Context
      Action: Action
      Device: Device
      Settings: Settings }
    static member FromJson (_:Message): Json<Message> = json {
        let! action = Json.readOrDefault "action" ""
        let! context = Json.readOrDefault "context" ""
        let! device = Json.read "device"
        let! eventName = Json.read "event"
        let! settings = Json.read "payload"
        let! event = 
            match eventName with
            | "didReceiveSettings" -> json.Return(DidReceiveSettings)
            | "keyUp" -> json.Return(KeyUp)
            | "willAppear" -> json.Return(WillAppear)
            | unknown -> json.Return(Unknown unknown)
        return {Action = Action action; Context = Context context; Device = Device device; Event = event; Settings = settings}
    }



let toMessage (rawJson: string) : Choice<Message, string> =
    match rawJson |> Json.tryParse with
    | Choice1Of2 parsed -> parsed |> Json.tryDeserialize
    | Choice2Of2 x -> Choice2Of2 x

let receiveString (websocket: ClientWebSocket) : Task<string> =
    let buffer = ResizeArray<byte>()
    let rec receiveImpl (buffer: ResizeArray<byte>) =
        task {
            let receiveBuffer = Array.create 255 0uy
            let receiveSegment = new ArraySegment<byte>(receiveBuffer)
            let! result = websocket.ReceiveAsync(receiveSegment, CancellationToken.None)
            let trimmed = receiveSegment.Array |> Array.take result.Count
            buffer.AddRange(trimmed)
            return! match result.EndOfMessage with
                        | true -> Task.FromResult(buffer.ToArray() |> (System.Text.UTF8Encoding()).GetString)
                        | false -> receiveImpl buffer
        }
    receiveImpl buffer

type CountdownState = Stopped | Running of CancellationTokenSource
type PluginState = Map<Context, CountdownState>

let startCountdownImpl (settings: Settings) (context: Context) (token: CancellationToken) (completed: Context -> Unit): unit =
    let writeTime = writeFile "C:/Snaz/TextFiles/ChronoDown.txt"
    let taskWithToken = TaskBuilderWithToken()
    let rec updateText (secondsRemaining: int): TokenToTask<unit> =
        if secondsRemaining > 0 then
            taskWithToken {
                let minutesRemaining = secondsRemaining / 60
                let secondsInMinute = secondsRemaining % 60
                let timeRemaining = (sprintf "%i:%02i" minutesRemaining secondsInMinute)
                let text = sprintf "%s %s" (Option.getOrElse "Back in" settings.CountdownText) timeRemaining
                do! writeTime text
                let json = setTitle context timeRemaining
                do! websocket.SendAsync(payload json, WebSocketMessageType.Text, true, CancellationToken.None).AsTask() |> ToTaskUnit
                do! Task.Delay(1000) |> ToTaskUnit
                return! updateText (secondsRemaining - 1)
            }
        else taskWithToken {
                completed context
                do! writeTime (Option.getOrElse "Back soon" settings.FinishedText)
            }
    
    updateText (int settings.Minutes.Value * 60) token |> ignore

type Countdown (startCountdown: Settings -> Context -> CancellationToken -> (Context -> unit) -> unit) =

    let handleCompletion (context: Context) (state: Task<PluginState>): Task<PluginState> = task {
        let request = setTitle context "Done"
        do! websocket.SendAsync(payload request, WebSocketMessageType.Text, true, CancellationToken.None).AsTask() |> ToTaskUnit
        let! state' = state
        return state' |> Map.add context Stopped
    }

    let rec mailbox = MailboxProcessor.Start(fun inbox -> 
        let rec loop (localState: Task<PluginState>) = async {
            log (sprintf "Loop State: %A" localState)
            match! inbox.Receive() with
            | PressedButton (context, minutes) ->
                let state' = handleButtonPress context minutes localState
                log (sprintf "After HBP State: %A" state')
                return! loop state'
            | CountdownCompleted context ->
                return! loop (handleCompletion context localState)
        }
        loop (Task.FromResult Map.empty)
        )
    and handleButtonPress (context: Context) (settings: Settings) (state: Task<PluginState>): Task<PluginState> = task {
        let! state' = state
        log (sprintf "HBP State: %A" state)
        match (state' |> Map.findOrDefault context Stopped) with
        | Running tokenSource ->
            tokenSource.Cancel ()
            let request = setTitle context "Stopped"
            do! websocket.SendAsync(payload request, WebSocketMessageType.Text, true, CancellationToken.None).AsTask() |> ToTaskUnit
            return state' |> Map.add context Stopped
        | Stopped ->
            let tokenSource = new CancellationTokenSource()
            startCountdown settings context tokenSource.Token (CountdownCompleted >> mailbox.Post)
            return state' |> Map.add context (Running tokenSource)
    }
    
    member _this.PressedButton (context : Context) (settings: Settings) =
        mailbox.Post <| PressedButton (context, settings)

let countdown = Countdown(startCountdownImpl)

let handleMessage (message: Message): unit =
    match message.Event with
    | KeyUp -> countdown.PressedButton message.Context message.Settings |> ignore
    | _ -> ()

type Arguments = 
    | [<AltCommandLine("-port")>] Port of int
    | [<AltCommandLine("-pluginUUID")>] PluginUUID of string
    | [<AltCommandLine("-registerEvent")>] RegisterEvent of string
    | [<AltCommandLine("-info")>] Info of String
with
    interface IArgParserTemplate with
        member _this.Usage: string = 
            "Not Implemented"

[<EntryPoint>]
let main (argv: string array): int =
    let argumentParser = ArgumentParser.Create<Arguments>(programName = "sdplugin.exe")
    let results = argumentParser.Parse argv
    // System.Diagnostics.Debugger.Launch() |> ignore
    System.IO.File.WriteAllText("D:/something.txt", (sprintf "%A" results))
    printfn "%A" results

    let mainTask = task {
        do! websocket.ConnectAsync(Uri(sprintf "ws://localhost:%d" (results.GetResult Port)), CancellationToken.None) |> ToTaskUnit

        let json = sprintf """{"event": "%s","uuid": "%s"}""" (results.GetResult RegisterEvent) (results.GetResult PluginUUID)
        let toSend = ReadOnlyMemory((System.Text.UTF8Encoding()).GetBytes(json))

        do! websocket.SendAsync(toSend, WebSocketMessageType.Text, true, CancellationToken.None).AsTask() |> ToTaskUnit
        use writer = System.IO.File.CreateText("D:/socket.txt")
        while websocket.State = WebSocketState.Open do
            let! firstThing = receiveString websocket
            let parsed = toMessage firstThing
            match parsed with
            | Choice1Of2 p -> handleMessage p
            | _ -> ()
            do! writer.WriteLineAsync(sprintf "%A" parsed) |> ToTaskUnit
            do! writer.FlushAsync () |> ToTaskUnit
        return ()
    }

    mainTask.Result |> ignore

    0 // return an integer exit code
