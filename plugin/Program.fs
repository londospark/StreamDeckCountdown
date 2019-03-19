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
    { Minutes: Int16 option }
    static member FromJson(_: Settings): Json<Settings> = json {
        let settingsPrism =
            Json.Object_ >?> Map.key_ "settings"

        let minutePrism =
            settingsPrism >?> 
            Json.Object_ >?> Map.key_ "minutes" >?>
            Json.Number_

        let! minutes = Json.Optic.tryGet minutePrism
        return { Minutes = minutes |> Option.map (fun value -> int16 value)  }
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
    | PressedButton of Context * int16
    | CountdownCompleted

let websocket = new ClientWebSocket()
let task = TaskBuilder()
let payload (json: string) = ReadOnlyMemory((System.Text.UTF8Encoding()).GetBytes(json))

let writeFile (filename: string) (text: string): Task<unit> =
    System.IO.File.WriteAllTextAsync(filename, text) |> ToTaskUnit

let setTitle' (target: string) (Context context) (title: string) =
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


type Arguments = 
    | [<AltCommandLine("-port")>] Port of int
    | [<AltCommandLine("-pluginUUID")>] PluginUUID of string
    | [<AltCommandLine("-registerEvent")>] RegisterEvent of string
    | [<AltCommandLine("-info")>] Info of String
with
    interface IArgParserTemplate with
        member _this.Usage: string = 
            "Not Implemented"

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

let startCountdown (minutes: int16) (context: Context): unit =
    let writeTime = writeFile "C:/Snaz/TextFiles/ChronoDown.txt"
    let rec updateText (secondsRemaining: int): Task<unit> =
        if secondsRemaining > 0 then
            task {
                let minutesRemaining = secondsRemaining / 60
                let secondsInMinute = secondsRemaining % 60
                let text = (sprintf "Back in %i:%02i" minutesRemaining secondsInMinute)
                do! writeTime text
                let json = setTitle context (sprintf "%i:%02i" minutesRemaining secondsInMinute)
                do! websocket.SendAsync(payload json, WebSocketMessageType.Text, true, CancellationToken.None).AsTask() |> ToTaskUnit
                do! Task.Delay(1000) |> ToTaskUnit
                return! updateText (secondsRemaining - 1)
            }
        else task { do! writeTime "Back soon" }
    updateText (int minutes * 60) |> ignore


type CountdownState = Stopped | Running
type PluginState = Map<Context, CountdownState>

type Countdown() =

    let mailbox = MailboxProcessor.Start(fun inbox -> 
        let rec loop (state: PluginState) = async {
            match! inbox.Receive() with
            | PressedButton (context, minutes) ->
                startCountdown minutes context
                return! loop (state |> Map.add context Running)
            | CountdownCompleted ->
                return! loop state
        }
        loop Map.empty
    )

    member _this.PressedButton (context : Context) (minutes: int16) =
        mailbox.Post <| PressedButton (context, minutes)

    member _this.CountdownCompleted () =
        mailbox.Post CountdownCompleted

let countdown = Countdown()

let handleMessage (message: Message): unit =
    match message.Event with
    | KeyUp ->
        message.Settings.Minutes
        |> Option.iter (fun m -> (countdown.PressedButton message.Context m) |> ignore)
    | _ -> ()


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
