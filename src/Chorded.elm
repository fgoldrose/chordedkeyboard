module Chorded exposing (..)

import Browser
import Browser.Events exposing (..)
import Char exposing (fromCode, toCode)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
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
    }


type alias Flags =
    ()


init : Flags -> ( Model, Cmd Msg )
init () =
    ( initModel, Cmd.none )


initModel : Model
initModel =
    { show = ""
    , chord = []
    }



--


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- Chords


type alias Chord =
    List Int


chordMap : Dict (List Int) String
chordMap =
    Dict.fromList
        [ ( [ toCode 'D', toCode 'S' ], "o" )
        , ( [ toCode 'D', toCode 'F' ], "a" )
        , ( [ toCode 'F', toCode 'S' ], "s" )

        --
        , ( [ toCode 'J', toCode 'S' ], "h" )
        , ( [ toCode 'K', toCode 'S' ], "f" )
        , ( [ toCode 'L', toCode 'S' ], "d" )
        , ( [ toCode 'D', toCode 'J' ], "t" )
        , ( [ toCode 'D', toCode 'K' ], "i" )
        , ( [ toCode 'D', toCode 'L' ], "n" )
        , ( [ toCode 'F', toCode 'J' ], "e" )
        , ( [ toCode 'F', toCode 'K' ], "l" )
        , ( [ toCode 'F', toCode 'L' ], "y" )

        --
        , ( [ toCode 'J', toCode 'K' ], "r" )
        , ( [ toCode 'J', toCode 'L' ], "c" )
        , ( [ toCode 'K', toCode 'L' ], "u" )

        --
        , ( [ toCode 'A', toCode 'S' ], "z" )
        , ( [ toCode 'A', toCode 'F' ], "m" )
        , ( [ toCode 'A', toCode 'J' ], "b" )
        , ( [ toCode 'A', toCode 'K' ], "p" )
        , ( [ toCode 'A', toCode 'L' ], "x" )
        , ( [ toCode 'L', toCode 'º' ], "q" )
        , ( [ toCode 'F', toCode 'º' ], "v" )
        , ( [ toCode 'J', toCode 'º' ], "k" )
        , ( [ toCode 'D', toCode 'º' ], "g" )
        , ( [ toCode 'A', toCode 'º' ], "w" )
        , ( [ toCode 'S', toCode 'º' ], "j" )

        --
        , ( [ toCode 'K', toCode 'º' ], "" )
        , ( [ toCode 'A', toCode 'D' ], "" )
        , ( [ toCode ' ' ], " " )
        ]


chordToString : Chord -> String
chordToString c =
    case Dict.get (List.sort c) chordMap of
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
            chordToString addi
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
                chordToString remi

            else
                chordToString firsttwo
    in
    { model | show = model.show ++ string, chord = remi }



-- Update --


type Msg
    = KeyDown Int
    | KeyUp Int
    | Clear
    | NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown i ->
            ( downmodel i model, Cmd.none )

        KeyUp i ->
            ( upmodel i model, Cmd.none )

        Clear ->
            ( { model | chord = [] }, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



-- View --


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


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.div [] [ Html.text model.show ]
        , Html.input [ type_ "text", spellcheck False, width 500, placeholder "Type something", value model.show, onKeyDown KeyDown, onKeyUp KeyUp, onInput (\x -> NoOp), onFocus Clear ] []
        , Html.div [] [ Html.text <| String.fromList <| List.map fromCode model.chord ]
        ]
