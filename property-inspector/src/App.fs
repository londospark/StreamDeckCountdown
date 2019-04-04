module App

open Elmish
open Elmish.React
open Fable.Import.Browser
open System
open Fable.Import
open Thoth.Json

let otherwise = FSharp.Core.Option.defaultValue

type Payload =
  { IsInMultiAction: bool; Settings: Settings }
  static member Decoder: Decode.Decoder<Payload> =
    Decode.object
      (fun get -> 
        {
          IsInMultiAction = get.Required.Field "isInMultiAction" Decode.bool
          Settings = get.Required.Field "settings" Settings.Decoder
        })

type ElgatoEvent =
  { Action: string; Context: string; Device: string; Event: string; Payload: Payload }
  static member Decoder: Decode.Decoder<ElgatoEvent> =
    Decode.object
      (fun get -> 
        {
          Action = get.Required.Field "action" Decode.string
          Context = get.Required.Field "context" Decode.string
          Device = get.Required.Field "device" Decode.string
          Event = get.Required.Field "event" Decode.string
          Payload = get.Required.Field "payload" Payload.Decoder
        })

module State =
  
  type Msg =
  | Event of Result<ElgatoEvent, string>
  | UpdateTime of TimeSpan
  | UpdateCountdownText of string
  | UpdateFinishedText of string
  | Error

let mutable socket : WebSocket = null
let mutable uuid: string = null

let receiveWebsocketMessages (_: Settings) =
  let sub dispatch =
    Browser.console.log "Setting up websocket for receive."
    socket.onmessage <-
      (fun message ->
        Browser.console.log message
        (State.Event (Decode.fromString ElgatoEvent.Decoder (message.data.ToString()))) |> dispatch)
    Browser.console.log "Finishing setting up websocket for receive."
  Cmd.ofSub sub

let sendSettings (model: Settings) : unit =
  let json = Encode.object [
    "event", Encode.string "setSettings"
    "context", Encode.string uuid
    "payload", Encode.object [
      //TODO(gareth): Is this not just a settings object? Should this be in shared?
      "countdownTime", Encode.int64 (model.CountdownTime.Ticks)
      "countdownText", Encode.string (model.CountdownText |> otherwise "")
      "finishedText", Encode.string (model.FinishedText |> otherwise "")
    ]
  ]
  socket.send(Encode.toString 0 json)
  Browser.console.log json
  ()

let getSettings () : unit =
  let json = Encode.object [
    "event", Encode.string "getSettings"
    "context", Encode.string uuid
  ]
  socket.send(Encode.toString 0 json)
  Browser.console.log json
  ()

let handleError (e: exn) : State.Msg = 
  Browser.console.error e
  State.Error

module ViewHelpers =
  open Fable.Helpers.React
  open Fable.Helpers.React.Props

  let TextField (label: string) (id: string) (placeholder: string) (text: string) (onchange: React.FormEvent -> unit): React.ReactElement =
    div [ Class "sdpi-item"] [
      div [ Class "sdpi-item-label" ] [ str label ]
      input [ Class "sdpi-item-value"; Id id; Value text; Placeholder placeholder; OnChange onchange]
    ]

// MODEL
module Elmish =
  open State
  open ViewHelpers
  open Fable.Helpers.React
  open Fable.Helpers.React.Props

  let init () : Settings * Cmd<_> = {CountdownTime = TimeSpan(0, 2, 30); CountdownText = None; FinishedText = None}, Cmd.none

  // UPDATE

  let update (msg: Msg) (model: Settings) : Settings * Cmd<_> =
      match msg with
      | Event (Ok data) ->
          Browser.console.log(data)
          data.Payload.Settings, Cmd.none

      | Event (Result.Error message) -> 
          Browser.console.log(message)
          model, Cmd.none

      | UpdateTime time ->
        let newModel = {model with CountdownTime = time}
        newModel, Cmd.attemptFunc sendSettings newModel handleError

      | UpdateCountdownText text ->
        let newModel = {model with CountdownText = Some text}
        newModel, Cmd.attemptFunc sendSettings newModel handleError

      | UpdateFinishedText text ->
        let newModel = {model with FinishedText = Some text}
        newModel, Cmd.attemptFunc sendSettings newModel handleError

      | Error -> model, Cmd.none


  let toTimespan (mmss: string): TimeSpan =
    let components = mmss.Split ':'
    let minutes = int components.[0]
    let seconds = int components.[1]
    TimeSpan(0, minutes, seconds)

  let fromTimespan (ts: TimeSpan): string =
    sprintf "%02d:%02d" ts.Minutes ts.Seconds

  // VIEW (rendered with React)

  let view (model: Settings) dispatch =


    div [ Class "sdpi-wrapper"] [
      
      div [ Class "sdpi-item"; Id "time"]
          [ div [ Class "sdpi-item-label" ] [ str "Time To Wait" ]
            input [ Type "time"
                    Class "sdpi-item-value"
                    OnChange (fun e -> toTimespan e.Value |> State.UpdateTime |> dispatch)
                    Value (model.CountdownTime |> fromTimespan) ] ]

      TextField "Countdown Text" "countdownText" "Text to display during the timer" (model.CountdownText |> otherwise "")
        (fun e -> e.Value |> State.UpdateCountdownText |> dispatch)

      TextField "Finished" "finishedText" "Text to display when timer finished" (model.FinishedText |> otherwise "")
        (fun e -> e.Value |> State.UpdateFinishedText |> dispatch)
    ]

let connectSocket(inPort, inPropertyInspectorUUID: string, inRegisterEvent, inInfo, inActionInfo) =
  socket <- WebSocket.Create (sprintf "ws://localhost:%d" inPort)
  Browser.console.log "Called init"
  
  socket.onopen <- (fun _ ->
    let json = Encode.object [
        "event", inRegisterEvent
        "uuid", Encode.string inPropertyInspectorUUID
    ]
    uuid <- inPropertyInspectorUUID
    Browser.console.log json
    socket.send(Encode.toString 0 json)
    getSettings ()
  )

  Program.mkProgram Elmish.init Elmish.update Elmish.view
  |> Program.withReact "elmish-app"
  |> Program.withSubscription receiveWebsocketMessages
  |> Program.withConsoleTrace
  |> Program.run

// App

