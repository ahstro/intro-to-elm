module Random exposing (..)

import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random
import List


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



--  Model


type alias Model =
    { dice : List Int
    }



-- Init


init : ( Model, Cmd Msg )
init =
    ( Model [ 1, 1 ], Cmd.none )



-- Update


type Msg
    = Roll
    | NewFaces (List Int)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model, Random.generate NewFaces (Random.list (List.length model.dice) (Random.int 1 6)) )

        NewFaces newFaces ->
            ( { dice = newFaces }, Cmd.none )



-- View


view : Model -> Html Msg
view model =
    div []
        (List.concat
            [ (List.map (\die -> img [ src (getImagePath die) ] []) model.dice)
            , [ button [ onClick Roll ] [ text "Roll" ]
              ]
            ]
        )


getImagePath : Int -> String
getImagePath dieFace =
    let
        file =
            case dieFace of
                1 ->
                    "one"

                2 ->
                    "two"

                3 ->
                    "three"

                4 ->
                    "four"

                5 ->
                    "five"

                6 ->
                    "six"

                _ ->
                    "one"
    in
        "images/" ++ file ++ ".png"



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
