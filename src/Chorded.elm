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
    , chord = Set.empty
    }



--


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- Chords


type alias Chord =
    Set.Set Int


chord : List Int -> Chord
chord l =
    Set.fromList l



--chordMap : Dict (List Int) String
--chordMap =
--    Dict.fromList
--        [ ( [ toCode 'A', toCode 'S' ], "12 " )
--        , ( [ toCode 'A', toCode 'D' ], "13 " )
--        , ( [ toCode 'A', toCode 'F' ], "14 " )
--        , ( [ toCode 'A', toCode 'J' ], "15 " )
--        , ( [ toCode 'A', toCode 'K' ], "16 " )
--        , ( [ toCode 'A', toCode 'L' ], "17 " )
--        , ( [ toCode 'A', toCode 'º' ], "18 " )
--        , ( [ toCode 'D', toCode 'S' ], "32 " )
--        , ( [ toCode 'D', toCode 'F' ], "34 " )
--        , ( [ toCode 'D', toCode 'J' ], "35 " )
--        , ( [ toCode 'D', toCode 'K' ], "36 " )
--        , ( [ toCode 'D', toCode 'L' ], "37 " )
--        , ( [ toCode 'D', toCode 'º' ], "38 " )
--        , ( [ toCode 'F', toCode 'S' ], "42 " )
--        , ( [ toCode 'F', toCode 'J' ], "45 " )
--        , ( [ toCode 'F', toCode 'K' ], "46 " )
--        , ( [ toCode 'F', toCode 'L' ], "47 " )
--        , ( [ toCode 'F', toCode 'º' ], "48 " )
--        , ( [ toCode 'J', toCode 'S' ], "52 " )
--        , ( [ toCode 'J', toCode 'K' ], "56 " )
--        , ( [ toCode 'J', toCode 'L' ], "57 " )
--        , ( [ toCode 'J', toCode 'º' ], "58 " )
--        , ( [ toCode 'K', toCode 'S' ], "62 " )
--        , ( [ toCode 'K', toCode 'L' ], "67 " )
--        , ( [ toCode 'K', toCode 'º' ], "68 " )
--        , ( [ toCode 'L', toCode 'S' ], "72 " )
--        , ( [ toCode 'L', toCode 'º' ], "78 " )
--        , ( [ toCode 'S', toCode 'º' ], "28 " )
--        ]
-- here


chordMap : Dict (List Int) String
chordMap =
    Dict.fromList
        [ ( [ toCode 'A', toCode 'S' ], "z" )
        , ( [ toCode 'A', toCode 'D' ], "" )
        , ( [ toCode 'A', toCode 'F' ], "m" )
        , ( [ toCode 'A', toCode 'J' ], "b" )
        , ( [ toCode 'A', toCode 'K' ], "p" )
        , ( [ toCode 'A', toCode 'L' ], "x" )
        , ( [ toCode 'A', toCode 'º' ], "w" )
        , ( [ toCode 'D', toCode 'S' ], "o" )
        , ( [ toCode 'D', toCode 'F' ], "t" )
        , ( [ toCode 'D', toCode 'J' ], "i" )
        , ( [ toCode 'D', toCode 'K' ], "n" )
        , ( [ toCode 'D', toCode 'L' ], "g" )
        , ( [ toCode 'D', toCode 'º' ], "f" )
        , ( [ toCode 'F', toCode 'S' ], "a" )
        , ( [ toCode 'F', toCode 'J' ], "e" )
        , ( [ toCode 'F', toCode 'K' ], "h" )
        , ( [ toCode 'F', toCode 'L' ], "r" )
        , ( [ toCode 'F', toCode 'º' ], "v" )
        , ( [ toCode 'J', toCode 'S' ], "y" )
        , ( [ toCode 'J', toCode 'K' ], "d" )
        , ( [ toCode 'J', toCode 'L' ], "c" )
        , ( [ toCode 'J', toCode 'º' ], "k" )
        , ( [ toCode 'K', toCode 'S' ], "l" )
        , ( [ toCode 'K', toCode 'L' ], "u" )
        , ( [ toCode 'K', toCode 'º' ], "" )
        , ( [ toCode 'L', toCode 'S' ], "s" )
        , ( [ toCode 'L', toCode 'º' ], "q" )
        , ( [ toCode 'S', toCode 'º' ], "j" )
        , ( [ toCode ' ' ], " " )
        ]


chordToString : Chord -> String
chordToString c =
    case Dict.get (Set.toList c) chordMap of
        Nothing ->
            ""

        Just s ->
            s



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
            let
                newchord =
                    Set.insert i model.chord
            in
            ( { model | show = model.show ++ chordToString newchord, chord = newchord }, Cmd.none )

        KeyUp i ->
            let
                newchord =
                    Set.remove i model.chord
            in
            ( { model | show = model.show ++ chordToString newchord, chord = newchord }, Cmd.none )

        Clear ->
            ( { model | chord = Set.empty }, Cmd.none )

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
        [ Html.input [ type_ "text", spellcheck False, width 500, placeholder "Type something", value model.show, onKeyDown KeyDown, onKeyUp KeyUp, onInput (\x -> NoOp), onFocus Clear ] []
        , Html.div [] [ Html.text <| String.fromList <| List.map fromCode <| Set.toList model.chord ]
        ]
