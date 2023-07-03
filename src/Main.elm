port module Main exposing (..)

import Browser
import Browser.Navigation
import Component exposing (..)
import Enum
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed exposing (..)
import Html.Lazy exposing (..)
import Icon
import Json.Decode as JD
import Json.Encode as JE
import Task
import Time
import Url
import Util.Html.Events
import Util.Json.Decode as UtilJD
import Util.Time


main : Program JE.Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = updateWithStorage
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }



-- PORTS


port setStorage : JE.Value -> Cmd msg



-- MODELS


type Visibility
    = All
    | Active
    | Completed


visibilityEnum : Enum.Enum Visibility
visibilityEnum =
    Enum.fromIterator
        (\x ->
            case x of
                All ->
                    ( "Active", Active )

                Active ->
                    ( "Completed", Completed )

                Completed ->
                    ( "All", All )
        )
        All


type alias Model =
    { inputFieldValue : String
    , notes : List Note
    , visibility : Visibility
    , nextId : Int
    , url : Url.Url
    , key : Browser.Navigation.Key
    , zone : Time.Zone
    , zoneName : Time.ZoneName
    , currentTime : Time.Posix
    , locale : String
    }


type alias ModelDefaults =
    { url : Url.Url
    , key : Browser.Navigation.Key
    , zone : Time.Zone
    , zoneName : Time.ZoneName
    , currentTime : Time.Posix
    }


initModel : ModelDefaults -> Model
initModel defaults =
    { inputFieldValue = ""
    , notes = []
    , visibility = All
    , nextId = 0
    , url = defaults.url
    , key = defaults.key
    , zone = defaults.zone
    , zoneName = defaults.zoneName
    , currentTime = defaults.currentTime
    , locale = "en-GB"
    }


encodeModel : Model -> JE.Value
encodeModel model =
    JE.object
        [ ( "inputFieldValue", JE.string model.inputFieldValue )
        , ( "notes", JE.list encodeNote model.notes )
        , ( "visibility", visibilityEnum.encode model.visibility )
        , ( "nextId", JE.int model.nextId )

        -- url
        -- key
        -- zone
        -- zoneNmae
        -- currentTime
        , ( "locale", JE.string model.locale )
        ]


decodeModel : ModelDefaults -> JD.Decoder Model
decodeModel defaults =
    JD.succeed Model
        |> UtilJD.partialApply (JD.field "inputFieldValue" JD.string)
        |> UtilJD.partialApply (JD.field "notes" (JD.list decodeNote))
        |> UtilJD.partialApply (JD.field "visibility" visibilityEnum.decoder)
        |> UtilJD.partialApply (JD.field "nextId" JD.int)
        |> UtilJD.partialApply (JD.succeed defaults.url)
        |> UtilJD.partialApply (JD.succeed defaults.key)
        |> UtilJD.partialApply (JD.succeed defaults.zone)
        |> UtilJD.partialApply (JD.succeed defaults.zoneName)
        |> UtilJD.partialApply (JD.succeed defaults.currentTime)
        |> UtilJD.partialApply (JD.field "locale" JD.string)


type alias Note =
    { id : Int
    , description : String
    , completed : Bool
    , creationTime : Time.Posix
    }


type alias NoteDefaults =
    { id : Int
    , desc : String
    , creationTime : Time.Posix
    }


initNote : NoteDefaults -> Note
initNote defaults =
    { id = defaults.id
    , description = defaults.desc
    , completed = False
    , creationTime = defaults.creationTime
    }


encodeNote : Note -> JE.Value
encodeNote note =
    JE.object
        [ ( "id", JE.int note.id )
        , ( "description", JE.string note.description )
        , ( "completed", JE.bool note.completed )
        , ( "creationTime", encodeTime note.creationTime )
        ]


decodeNote : JD.Decoder Note
decodeNote =
    JD.map4 Note
        (JD.field "id" JD.int)
        (JD.field "description" JD.string)
        (JD.field "completed" JD.bool)
        (JD.field "creationTime" decodeTime)


encodeTime : Time.Posix -> JE.Value
encodeTime time =
    JE.int (Time.posixToMillis time)


decodeTime : JD.Decoder Time.Posix
decodeTime =
    JD.int
        |> JD.andThen
            (\int ->
                JD.succeed (Time.millisToPosix int)
            )


init : JE.Value -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        modelDefaults =
            { url = url
            , key = key
            , zone = Time.utc
            , zoneName = Time.Name "UTC"
            , currentTime = Time.millisToPosix 0
            }
    in
    ( case JD.decodeValue (decodeModel modelDefaults) flags of
        Ok model ->
            model

        Err _ ->
            initModel modelDefaults
    , Cmd.batch
        [ Task.perform AdjustTimeZone Time.here
        , Task.perform AdjustTimeZoneName Time.getZoneName
        ]
    )



-- UPDATES


type Msg
    = UpdateInputField String
    | Add
    | AddWithTime Time.Posix
    | Delete Int
    | DeleteComplete
    | Check Int
    | CheckAll Bool
    | ChangeVisibility Visibility
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | AdjustTimeZone Time.Zone
    | AdjustTimeZoneName Time.ZoneName
    | Tick Time.Posix


updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
    let
        ( newModel, cmds ) =
            update msg model
    in
    ( newModel
    , Cmd.batch [ setStorage (encodeModel newModel), cmds ]
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Add ->
            ( model
            , Task.perform AddWithTime Time.now
            )

        AddWithTime time ->
            ( { model
                | nextId = model.nextId + 1
                , inputFieldValue = ""
                , notes =
                    if String.isEmpty model.inputFieldValue then
                        model.notes

                    else
                        let
                            note =
                                initNote
                                    { id = model.nextId
                                    , desc = model.inputFieldValue
                                    , creationTime = time
                                    }

                            notes =
                                model.notes ++ [ note ]
                        in
                        notes
              }
            , Cmd.none
            )

        UpdateInputField str ->
            ( { model | inputFieldValue = str }
            , Cmd.none
            )

        Delete id ->
            ( { model | notes = List.filter (\t -> t.id /= id) model.notes }
            , Cmd.none
            )

        DeleteComplete ->
            ( { model | notes = List.filter (.completed >> not) model.notes }
            , Cmd.none
            )

        Check id ->
            let
                updateNote t =
                    if t.id == id then
                        { t | completed = not t.completed }

                    else
                        t
            in
            ( { model | notes = List.map updateNote model.notes }
            , Cmd.none
            )

        CheckAll isCompleted ->
            let
                updateNote t =
                    { t | completed = isCompleted }
            in
            ( { model | notes = List.map updateNote model.notes }
            , Cmd.none
            )

        ChangeVisibility visibility ->
            ( { model | visibility = visibility }
            , Cmd.none
            )

        LinkClicked urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Browser.Navigation.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Browser.Navigation.load href )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )

        AdjustTimeZone zone ->
            ( { model | zone = zone }
            , Cmd.none
            )

        AdjustTimeZoneName zoneName ->
            ( { model | zoneName = zoneName }
            , Cmd.none
            )

        Tick newTime ->
            ( { model | currentTime = newTime }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 Tick



-- VIEWS


view : Model -> Browser.Document Msg
view model =
    { title = "TodoMVC"
    , body =
        [ div [ class "min-w-screen min-h-screen bg-gradient-to-r from-cyan-100 to-blue-100" ]
            [ div [ class "container mx-auto max-w-xl p-4" ]
                [ column [ class "gap-4" ]
                    [ header [ class "flex justify-center" ]
                        [ h1 [ class "text-9xl font-bold text-cyan-500 drop-shadow-xl" ]
                            [ text "Todos" ]
                        ]
                    , section []
                        [ column [ class "gap-4" ]
                            [ lazy viewInputField model.inputFieldValue
                            , lazy6
                                viewNotes
                                model.visibility
                                model.locale
                                model.zone
                                model.zoneName
                                model.currentTime
                                model.notes
                            ]
                        ]
                    ]
                ]
            ]
        ]
    }


viewInputField : String -> Html Msg
viewInputField inputFieldValue =
    row [ class "gap-2 p-2 shadow-xl rounded-full bg-white" ]
        [ input
            [ class "grow bg-transparent rounded-full px-2 py-1"
            , placeholder "What needs to be done?"
            , value inputFieldValue
            , onInput UpdateInputField
            , Util.Html.Events.onEnter Add
            , type_ "text"
            ]
            []
        , iconButton
            { icon = Icon.plus }
            [ class "text-white bg-cyan-500 hover:bg-cyan-600"
            , onClick Add
            ]
        ]


viewNotes : Visibility -> String -> Time.Zone -> Time.ZoneName -> Time.Posix -> List Note -> Html Msg
viewNotes visibility locale zone zoneName currentTime notes =
    let
        isVisible note =
            case visibility of
                All ->
                    True

                Active ->
                    not note.completed

                Completed ->
                    note.completed
    in
    section []
        [ List.filter isVisible notes
            |> List.map (viewKeyedNote locale zone zoneName currentTime)
            |> Html.Keyed.ul [ class "flex flex-col gap-2" ]
        ]


viewKeyedNote : String -> Time.Zone -> Time.ZoneName -> Time.Posix -> Note -> ( String, Html Msg )
viewKeyedNote locale zone zoneName currentTime note =
    ( String.fromInt note.id, lazy5 viewNote locale zone zoneName currentTime note )


viewNote : String -> Time.Zone -> Time.ZoneName -> Time.Posix -> Note -> Html Msg
viewNote locale zone zoneName currentTime note =
    let
        ( liCompletedStyle, dateTimeCompletedStyle ) =
            if note.completed then
                ( "text-white bg-cyan-500 hover:bg-cyan-600"
                , "text-slate-200"
                )

            else
                ( "bg-white/75 hover:bg-white/100"
                , "text-slate-500"
                )
    in
    li
        [ class "rounded-3xl p-2 w-full transition-colors duration-200 ease-in-out"
        , class liCompletedStyle
        ]
        [ column [ class "gap-2" ]
            [ label [ class "pt-1 px-1 grow" ]
                [ text note.description ]
            , div [ class "grid grid-cols-3 gap-2 items-center" ]
                [ div
                    [ class "justify-self-start ps-1 text-xs"
                    , class dateTimeCompletedStyle
                    ]
                    [ dateTime
                        { locale = locale
                        , time = note.creationTime
                        , zoneName = zoneName
                        }
                    ]
                , div
                    [ class "justify-self-center text-xs"
                    , class dateTimeCompletedStyle
                    ]
                    [ relativeDateTime
                        { locale = locale
                        , timeDelta = Util.Time.timeDelta zone note.creationTime currentTime
                        }
                    ]
                , row [ class "justify-self-end gap-2" ]
                    [ iconCheckboxButton
                        { checkboxId = "choose-me-" ++ String.fromInt note.id
                        , checkedValue = note.completed
                        , checkedClass = "peer-checked:text-black peer-checked:bg-white/75 hover:peer-checked:bg-white/100"
                        , uncheckedClass = "text-white bg-cyan-500 hover:bg-cyan-600"
                        , checkedIcon = Icon.xMark
                        , uncheckedIcon = Icon.check
                        }
                        [ onClick (Check note.id) ]
                    , iconButton
                        { icon = Icon.trash }
                        [ class "text-white bg-rose-500 hover:bg-rose-600"
                        , onClick (Delete note.id)
                        ]
                    ]
                ]
            ]
        ]
