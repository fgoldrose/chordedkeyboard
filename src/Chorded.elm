module Chorded exposing (..)

import Browser
import Browser.Events exposing (..)
import Char exposing (fromCode, toCode)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes exposing (..)
import Html.Events
import Http
import Json.Decode as Decode
import Set
import Task
import Time


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type Key
    = Character Char
    | Control String


type alias Chord =
    List Key


type alias ChordMap =
    Dict (List String) String



-- Model --


type alias Model =
    { show : String
    , chord : Chord
    , chordmap : ChordMap
    , lastpress : Maybe Time.Posix
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init () =
    ( initModel, getChordFile "./defaultmap.json" )


initModel : Model
initModel =
    { show = ""
    , chord = []
    , chordmap = Dict.empty
    , lastpress = Nothing
    }



--


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch [ onKeyDown downDecoder, onKeyUp upDecoder ]


toKey : String -> Key
toKey string =
    case String.uncons string of
        Just ( char, "" ) ->
            Character char

        _ ->
            Control string


downDecoder : Decode.Decoder Msg
downDecoder =
    nonRepeat
        (Decode.field "key" Decode.string
            |> Decode.map toKey
            |> Decode.map KeyDown
        )


upDecoder : Decode.Decoder Msg
upDecoder =
    Decode.field "key" Decode.string |> Decode.map toKey |> Decode.map KeyUp


nonRepeat : Decode.Decoder Msg -> Decode.Decoder Msg
nonRepeat d =
    let
        maybeKey : Bool -> Decode.Decoder Msg
        maybeKey repeat =
            if repeat then
                Decode.fail "Repeated character"

            else
                d
    in
    Decode.field "repeat" Decode.bool |> Decode.andThen maybeKey



-- Chords


fixTuple : ( String, List String ) -> ( List String, String )
fixTuple t =
    ( Tuple.second t |> List.sort, Tuple.first t )


readChordMap : Decode.Decoder ChordMap
readChordMap =
    Decode.map
        (\l -> Dict.fromList <| List.map fixTuple l)
        (Decode.keyValuePairs <| Decode.list Decode.string)


getChordFile : String -> Cmd Msg
getChordFile filename =
    Http.get
        { url = filename
        , expect = Http.expectJson GotChordMap readChordMap
        }



--


keyToString : Key -> String
keyToString k =
    case k of
        Character c ->
            String.fromChar c

        Control s ->
            s


chordToString : Chord -> ChordMap -> String
chordToString c cm =
    case Dict.get (List.map keyToString c |> List.sort) cm of
        Nothing ->
            ""

        Just s ->
            s


removekey : Key -> Chord -> Chord
removekey i c =
    List.filter (\x -> x /= i) c


addkey : Key -> Chord -> Chord
addkey k c =
    case removekey k c of
        x :: xs ->
            x :: k :: xs

        [] ->
            [ k ]


longTimeDiff : Int
longTimeDiff =
    50


addChordVal : Model -> Time.Posix -> Model
addChordVal model updatetime =
    let
        cur =
            Time.posixToMillis updatetime

        prev =
            case model.lastpress of
                Nothing ->
                    0

                Just x ->
                    Time.posixToMillis x

        timediff =
            cur - prev

        ( addstring, changechord, changetime ) =
            case model.chord of
                f :: s :: rest ->
                    ( chordToString [ f, s ] model.chordmap
                    , if timediff < longTimeDiff then
                        s :: f :: rest

                      else
                        model.chord
                    , timediff < longTimeDiff
                    )

                _ ->
                    ( chordToString model.chord model.chordmap, model.chord, True )
    in
    { model
        | show = model.show ++ addstring
        , lastpress =
            if changetime then
                Just updatetime

            else
                model.lastpress
        , chord = changechord
    }



-- Update --


type Msg
    = GotChordMap (Result Http.Error ChordMap)
    | LoadChordMap String
    | KeyDown Key
    | KeyUp Key
    | GotTime Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotChordMap res ->
            case res of
                Ok cm ->
                    ( { model | chord = [], show = "", chordmap = cm }, Cmd.none )

                Err e ->
                    ( model, Cmd.none )

        LoadChordMap f ->
            ( model, getChordFile f )

        KeyDown i ->
            ( { model | chord = addkey i model.chord }, Task.perform GotTime Time.now )

        KeyUp i ->
            ( { model | chord = removekey i model.chord }, Cmd.none )

        GotTime time ->
            ( addChordVal model time, Cmd.none )



-- View --


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div []
            [ Html.button
                [ Html.Events.onClick <| LoadChordMap "defaultmap.json" ]
                [ Html.text "Default" ]
            , Html.button
                [ Html.Events.onClick <| LoadChordMap "onehand.json" ]
                [ Html.text "One hand" ]
            ]
        , Html.div [ style "height" "200px", style "font-size" "30px" ]
            [ Html.text model.show ]
        , Html.div []
            [ Html.text <| String.concat <| List.map keyToString model.chord ]
        ]
