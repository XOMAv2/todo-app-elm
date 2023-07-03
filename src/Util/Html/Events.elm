module Util.Html.Events exposing (..)

import Html
import Html.Events exposing (..)
import Json.Decode as JD


onEnter : msg -> Html.Attribute msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                JD.succeed msg

            else
                JD.fail "not ENTER"
    in
    keyCode
        |> JD.andThen isEnter
        |> on "keydown"
