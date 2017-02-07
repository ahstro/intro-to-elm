module TimeStuff exposing (..)

import Html exposing (..)
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time exposing (Time, second)


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { time : Time
    , paused : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( Model 0 False, Cmd.none )


type Msg
    = Tick Time
    | Pause


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model | time = newTime }, Cmd.none )

        Pause ->
            ( { model | paused = not model.paused }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.paused then
        Sub.none
    else
        Time.every second Tick


view : Model -> Html Msg
view model =
    let
        time =
            Time.inSeconds model.time

        seconds =
            (floor time) % 60

        minutes =
            (floor (time / 60)) % 60

        hours =
            (ceiling (time / 60 / 60)) % 12

        sAngle =
            degrees (toFloat (seconds * 6) - 90)

        mAngle =
            degrees (toFloat (minutes * 6) - 90)

        hAngle =
            degrees (toFloat (hours * 30) - 90)

        sHandX =
            toString (50 + 40 * cos sAngle)

        sHandY =
            toString (50 + 40 * sin sAngle)

        mHandX =
            toString (50 + 40 * cos mAngle)

        mHandY =
            toString (50 + 40 * sin mAngle)

        hHandX =
            toString (50 + 40 * (cos hAngle * 0.6))

        hHandY =
            toString (50 + 40 * (sin hAngle * 0.6))
    in
        div []
            [ svg [ viewBox "0 0 100 100", width "300px" ]
                [ circle [ cx "50", cy "50", r "45", fill "#0b79ce" ] []
                , line [ x1 "50", y1 "50", x2 sHandX, y2 sHandY, stroke "#630239" ] []
                , line [ x1 "50", y1 "50", x2 mHandX, y2 mHandY, stroke "#023963" ] []
                , line [ x1 "50", y1 "50", x2 hHandX, y2 hHandY, stroke "#023963" ] []
                ]
            , div [] [ Html.text ((toString hours) ++ ":" ++ (toString minutes) ++ ":" ++ (toString seconds)) ]
            , button [ onClick Pause ]
                [ Html.text (getButtonText model.paused)
                ]
            ]


getButtonText : Bool -> String
getButtonText paused =
    if paused then
        "Play"
    else
        "Pause"
