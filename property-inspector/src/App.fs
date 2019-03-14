module App

(**
 The famous Increment/Decrement ported from Elm.
 You can find more info about Emish architecture and samples at https://elmish.github.io/
*)

open Elmish
open Elmish.React
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.Import.Browser
open Thoth.Json
open System
open Fable.Import

module State =
  
  type Model = int16

  type Msg =
  | Event of string
  | UpdateSettings of Model
  | Error

let mutable socket : WebSocket = null
let mutable uuid: string = null

let connectSocket(inPort, inPropertyInspectorUUID: string, inRegisterEvent, inInfo, inActionInfo) =
  socket <- WebSocket.Create (sprintf "ws://localhost:%d" inPort)
  Browser.console.log "Called init"
  socket.onmessage <- (fun message ->
    Browser.console.log message
    message.data.ToString() |> State.Event |> Cmd.ofMsg)
  socket.onopen <- (fun _ ->
    let json = Encode.object [
        "event", inRegisterEvent
        "uuid", Encode.string inPropertyInspectorUUID
    ]
    uuid <- inPropertyInspectorUUID
    Browser.console.log json
    socket.send(Encode.toString 0 json)
  )

let sendSettings (time: int16) : unit =
  let json = Encode.object [
    "event", Encode.string "setSettings"
    "context", Encode.string uuid
    "payload", Encode.object [ "minutes", Encode.int (int time) ]
  ]
  socket.send(Encode.toString 0 json)
  Browser.console.log json
  ()

let handleError (e: exn) : State.Msg = 
  Browser.console.error e
  State.Error

// MODEL
module Elmish =
  open State

  let init() : Model * Cmd<_> = 2s, Cmd.attemptFunc sendSettings 2s handleError

  // UPDATE

  let update (msg: Msg) (model: Model) : Model * Cmd<_> =
      match msg with
      | Event data ->
        Browser.console.log(data)
        30s, Cmd.none
      | UpdateSettings time -> time, Cmd.attemptFunc sendSettings time handleError
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
                      DefaultValue model
                      OnChange (fun e -> e.Value |> Int16.Parse |> State.UpdateSettings |> dispatch)]
              span [ Class "clickable"; Value "60"] [str ("60")]]]
      details [Class "message"] [ summary [] [str (sprintf "%d minutes." model) ]]]
 
// App
Program.mkProgram Elmish.init Elmish.update Elmish.view
|> Program.withReact "elmish-app"
|> Program.withConsoleTrace
|> Program.run
