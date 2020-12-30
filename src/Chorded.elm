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
    Sub.none



-- Chords


type alias Chord =
    List Int


type alias ChordMap =
    Dict Chord String


fixTuple : ( String, List String ) -> ( List Int, String )
fixTuple t =
    ( Tuple.second t |> List.sort << List.map (Char.toCode << Tuple.first << Maybe.withDefault ( '\u{0000}', "" ) << String.uncons), Tuple.first t )


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


chordToString : Chord -> ChordMap -> String
chordToString c cm =
    let
        _ =
            Debug.log "chord" ( c, List.map Char.fromCode c )
    in
    case Dict.get (List.sort c) cm of
        Nothing ->
            ""

        Just s ->
            s


remove : Int -> Chord -> Chord
remove i c =
    List.filter (\x -> x /= i) c


downmodel : Int -> Model -> Model
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


upmodel : Int -> Model -> Model
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
    | KeyDown Int
    | KeyUp Int
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
                    let
                        _ =
                            Debug.log "err" e
                    in
                    ( model, Cmd.none )

        KeyDown i ->
            ( downmodel i model, Cmd.none )

        KeyUp i ->
            ( upmodel i model, Cmd.none )

        Clear ->
            ( { model | chord = [] }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



--


nonRepeatKey : Decode.Decoder Int
nonRepeatKey =
    let
        repeated : Decode.Decoder Bool
        repeated =
            Decode.field "repeat" Decode.bool

        maybeKeyCode : Bool -> Decode.Decoder Int
        maybeKeyCode repeat =
            if repeat then
                Decode.fail "Repeated character"

            else
                keyCode
    in
    repeated |> Decode.andThen maybeKeyCode


onKeyDown : (Int -> msg) -> Html.Attribute msg
onKeyDown tagger =
    on "keydown" <| Decode.map tagger nonRepeatKey


onKeyUp : (Int -> msg) -> Html.Attribute msg
onKeyUp tagger =
    on "keyup" (Decode.map tagger keyCode)



-- View --


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div [ style "height" "200px", style "font-size" "30px" ]
            [ Html.text model.show ]
        , Html.input
            [ type_ "text"
            , spellcheck False
            , autofocus True
            , value ""
            , onKeyDown KeyDown
            , onKeyUp KeyUp
            , onInput (\x -> NoOp)
            , onFocus Clear
            ]
            []
        , Html.div []
            [ Html.text <| String.fromList <| List.map fromCode model.chord ]
        ]
