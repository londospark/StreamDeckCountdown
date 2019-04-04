[<AutoOpen>]
module Shared

#if FABLE_COMPILER
open Thoth.Json
#else
open Thoth.Json.Net
#endif

open System

type Settings =
  { CountdownTime: TimeSpan; CountdownText: string option; FinishedText: string option }
  static member Decoder: Decode.Decoder<Settings> =
    Decode.object
      (fun get -> 
        {
          CountdownTime = TimeSpan(get.Optional.Field "countdownTime" Decode.int64 |> Option.defaultValue (TimeSpan.TicksPerMinute * 2L))
          CountdownText = get.Optional.Field "countdownText" Decode.string
          FinishedText = get.Optional.Field "finishedText" Decode.string
        })