module Chorded exposing (..)

import Browser
import Browser.Events exposing (..)
import Char exposing (fromCode, toCode)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode
import Set


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
        , expect = Http.expectJson LoadChordMap readChordMap
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


remove : Key -> Chord -> Chord
remove i c =
    List.filter (\x -> x /= i) c


downmodel : Key -> Model -> Model
downmodel i model =
    let
        addi =
            case remove i model.chord of
                x :: xs ->
                    x :: i :: xs

                [] ->
                    [ i ]

        string =
            chordToString addi model.chordmap
    in
    { model | show = model.show ++ string, chord = addi }


upmodel : Key -> Model -> Model
upmodel i model =
    let
        remi =
            remove i model.chord

        firsttwo =
            List.take 2 model.chord

        string =
            if List.member i firsttwo then
                chordToString remi model.chordmap

            else
                chordToString firsttwo model.chordmap
    in
    { model | show = model.show ++ string, chord = remi }



-- Update --


type Msg
    = LoadChordMap (Result Http.Error ChordMap)
    | KeyDown Key
    | KeyUp Key
    | Clear
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadChordMap res ->
            case res of
                Ok cm ->
                    ( { model | chordmap = cm }, Cmd.none )

                Err e ->
                    ( model, Cmd.none )

        KeyDown i ->
            ( downmodel i model, Cmd.none )

        KeyUp i ->
            ( upmodel i model, Cmd.none )

        Clear ->
            ( { model | chord = [] }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



-- View --


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div [ style "height" "200px", style "font-size" "30px" ]
            [ Html.text model.show ]
        , Html.div []
            [ Html.text <| String.concat <| List.map keyToString model.chord ]
        ]
