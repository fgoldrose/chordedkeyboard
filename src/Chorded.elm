module Chorded exposing (..)

import Browser
import Browser.Events exposing (..)
import Char exposing (fromCode)
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
    , chord : Set.Set String
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
    Sub.batch [ onKeyDown downDecoder, onKeyUp upDecoder ]


downDecoder : Decode.Decoder Msg
downDecoder =
    Decode.map (\s -> KeyDown s) (Decode.field "key" Decode.string)


upDecoder : Decode.Decoder Msg
upDecoder =
    Decode.map (\s -> KeyUp s) (Decode.field "key" Decode.string)



-- Chords


type alias Chord =
    Set.Set String


chord : List String -> Chord
chord l =
    Set.fromList l


chordMap : Dict (List String) String
chordMap =
    Dict.fromList
        [ ( [ "a", "s" ], "1" )
        , ( [ "a", "d" ], "2" )
        , ( [ "a", "f" ], "" )
        , ( [ "", "" ], "" )
        , ( [ "", "" ], "" )
        , ( [ "", "" ], "" )
        , ( [ "", "" ], "" )
        , ( [ "", "" ], "" )
        , ( [ "", "" ], "" )
        , ( [ "", "" ], "" )
        , ( [ "", "" ], "" )
        ]


chordToChar : Chord -> String
chordToChar c =
    case Dict.get (Set.toList c) chordMap of
        Nothing ->
            ""

        Just s ->
            s



-- Update --


type Msg
    = KeyDown String
    | KeyUp String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        KeyDown s ->
            ( { model | chord = Set.insert s model.chord }, Cmd.none )

        KeyUp s ->
            ( { model | show = model.show ++ chordToChar model.chord, chord = Set.remove s model.chord }, Cmd.none )



-- View --


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.text model.show
        , Html.div [] [ Html.text (Set.foldl (++) "" model.chord) ]
        ]
