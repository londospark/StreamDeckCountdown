module App

open Elmish
open Elmish.React
open Fable.Import.Browser
open System
open Fable.Import
open Thoth.Json

let otherwise = FSharp.Core.Option.defaultValue

type Model =
  { Minutes: int16; CountdownText: string; FinishedText: string }
  static member Decoder: Decode.Decoder<Model> =
    Decode.object
      (fun get -> 
        {
          Minutes = int16 (get.Required.Field "minutes" Decode.int)
          CountdownText = get.Optional.Field "countdownText" Decode.string |> otherwise ""
          FinishedText = get.Optional.Field "finishedText" Decode.string |> otherwise ""
        })

type Payload =
  { IsInMultiAction: bool; Settings: Model }
  static member Decoder: Decode.Decoder<Payload> =
    Decode.object
      (fun get -> 
        {
          IsInMultiAction = get.Required.Field "isInMultiAction" Decode.bool
          Settings = get.Required.Field "settings" Model.Decoder
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
  | UpdateTime of int16
  | UpdateCountdownText of string
  | UpdateFinishedText of string
  | Error

let mutable socket : WebSocket = null
let mutable uuid: string = null

let receiveWebsocketMessages (_: Model) =
  let sub dispatch =
    Browser.console.log "Setting up websocket for receive."
    socket.onmessage <-
      (fun message ->
        Browser.console.log message
        (State.Event (Decode.fromString ElgatoEvent.Decoder (message.data.ToString()))) |> dispatch)
    Browser.console.log "Finishing setting up websocket for receive."
  Cmd.ofSub sub

let sendSettings (model: Model) : unit =
  let json = Encode.object [
    "event", Encode.string "setSettings"
    "context", Encode.string uuid
    "payload", Encode.object [
      "minutes", Encode.int (int model.Minutes)
      "countdownText", Encode.string model.CountdownText
      "finishedText", Encode.string model.FinishedText
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

  let init () : Model * Cmd<_> = {Minutes = 2s; CountdownText = ""; FinishedText = ""}, Cmd.none

  // UPDATE

  let update (msg: Msg) (model: Model) : Model * Cmd<_> =
      match msg with
      | Event (Ok data) ->
          Browser.console.log(data)
          data.Payload.Settings, Cmd.none

      | Event (Result.Error message) -> 
          Browser.console.log(message)
          model, Cmd.none

      | UpdateTime time ->
        let newModel = {model with Minutes = time}
        newModel, Cmd.attemptFunc sendSettings newModel handleError

      | UpdateCountdownText text ->
        let newModel = {model with CountdownText = text}
        newModel, Cmd.attemptFunc sendSettings newModel handleError

      | UpdateFinishedText text ->
        let newModel = {model with FinishedText = text}
        newModel, Cmd.attemptFunc sendSettings newModel handleError

      | Error -> model, Cmd.none

  // VIEW (rendered with React)

  let view (model: Model) dispatch =

    div [ Class "sdpi-wrapper"] [
      div [ Class "sdpi-item"; HTMLAttr.Type "range"; Id "time"]
          [ div [ Class "sdpi-item-label" ] [ str "Time To Wait" ]
            div [ Class "sdpi-item-value" ] [
              span [ Class "clickable"; Value "2"] [str ("2")]
              input [ HTMLAttr.Type "range"
                      Min "2"
                      HTMLAttr.Max "60"
                      Value model.Minutes
                      OnChange (fun e -> e.Value |> Int16.Parse |> State.UpdateTime |> dispatch)]
              span [ Class "clickable"; Value "60"] [str ("60")]]]
      details [Class "message"] [ summary [] [str (sprintf "%d minutes." model.Minutes) ]]

      TextField "Countdown Text" "countdownText" "Text to display during the timer" model.CountdownText
        (fun e -> e.Value |> State.UpdateCountdownText |> dispatch)

      TextField "Finished" "finishedText" "Text to display when timer finished" model.FinishedText
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

