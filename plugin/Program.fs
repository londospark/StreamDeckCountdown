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

let task = TaskBuilder()

type Settings =
    { Minutes: Int16 option }
    static member FromJson(_: Settings) = json {
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

type Message =
    { Event: Event
      Context: Context
      Action: Action
      Device: Device
      Settings: Settings }

    static member FromJson (_:Message) = json {
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


[<EntryPoint>]
let main argv =
    let argumentParser = ArgumentParser.Create<Arguments>(programName = "sdplugin.exe")
    let results = argumentParser.Parse argv
    // System.Diagnostics.Debugger.Launch() |> ignore
    System.IO.File.WriteAllText("D:/something.txt", (sprintf "%A" results))
    printfn "%A" results

    let websocket = new ClientWebSocket()
    let mainTask = task {
        do! websocket.ConnectAsync(Uri(sprintf "ws://localhost:%d" (results.GetResult Port)), CancellationToken.None) |> ToTaskUnit

        let json = sprintf """{"event": "%s","uuid": "%s"}""" (results.GetResult RegisterEvent) (results.GetResult PluginUUID)
        let toSend = ReadOnlyMemory((System.Text.UTF8Encoding()).GetBytes(json))

        do! websocket.SendAsync(toSend, WebSocketMessageType.Text, true, CancellationToken.None).AsTask() |> ToTaskUnit
        use writer = System.IO.File.CreateText("D:/socket.txt")
        while websocket.State = WebSocketState.Open do
            let! firstThing = receiveString websocket
            let parsed = toMessage firstThing
            do! writer.WriteLineAsync(sprintf "%A" parsed) |> ToTaskUnit
            do! writer.FlushAsync () |> ToTaskUnit
        return ()
    }

    mainTask.Result |> ignore

    0 // return an integer exit code
