module Util.Json.Decode exposing (..)

import Json.Decode exposing (..)


partialApply : Decoder a -> Decoder (a -> b) -> Decoder b
partialApply fieldDecoder recordConstructor =
    recordConstructor
        |> andThen (\parsedValue -> map parsedValue fieldDecoder)
