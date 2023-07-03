module Component exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Icon exposing (Icon)
import Time
import Util.Time



-- TYPES


type alias Props msg =
    List (Attribute msg)


type alias Body msg =
    List (Html msg)



-- COMPONENTS


column : Props msg -> Body msg -> Html msg
column props body =
    div (class "flex flex-col" :: props) body


row : Props msg -> Body msg -> Html msg
row props body =
    div (class "flex flex-row" :: props) body


iconButton : { icon : Icon msg } -> Props msg -> Html msg
iconButton conf props =
    button (class "rounded-full p-2" :: props)
        [ div [ class "w-4 h-4" ]
            [ conf.icon ]
        ]


iconCheckboxButton :
    { checkboxId : String
    , checkedValue : Bool
    , checkedClass : String
    , uncheckedClass : String
    , checkedIcon : Icon msg
    , uncheckedIcon : Icon msg
    }
    -> Props msg
    -> Html msg
iconCheckboxButton conf props =
    div
        [ class "flex" ]
        [ input
            ([ class "peer hidden"
             , id conf.checkboxId
             , type_ "checkbox"
             , checked conf.checkedValue
             ]
                ++ props
            )
            []
        , label
            [ for conf.checkboxId
            , class "select-none cursor-pointer rounded-full p-2 transition-colors duration-200 ease-in-out"
            , class conf.checkedClass
            , class conf.uncheckedClass
            ]
            [ div [ class "w-4 h-4" ]
                [ if conf.checkedValue then
                    conf.checkedIcon

                  else
                    conf.uncheckedIcon
                ]
            ]
        ]


dateTime :
    { locale : String
    , time : Time.Posix
    , zoneName : Time.ZoneName
    }
    -> Html msg
dateTime conf =
    let
        zoneNameAttribute =
            case conf.zoneName of
                Time.Name zoneName ->
                    attribute "zone-name" zoneName

                Time.Offset offset ->
                    attribute "offset" (String.fromInt offset)
    in
    node "date-time"
        [ attribute "locale" conf.locale
        , attribute "posix-time" (String.fromInt (Time.posixToMillis conf.time))
        , zoneNameAttribute
        ]
        []


relativeDateTime : { locale : String, timeDelta : Util.Time.TimeDelta } -> Html msg
relativeDateTime conf =
    node "relative-date-time"
        [ attribute "locale" conf.locale
        , attribute "year" (String.fromInt conf.timeDelta.year)
        , attribute "month" (String.fromInt conf.timeDelta.month)
        , attribute "day" (String.fromInt conf.timeDelta.day)
        , attribute "hour" (String.fromInt conf.timeDelta.hour)
        , attribute "minute" (String.fromInt conf.timeDelta.minute)
        , attribute "second" (String.fromInt conf.timeDelta.second)
        ]
        []
