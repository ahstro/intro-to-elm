module WebSockets exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import WebSocket


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { input : String
    , messages : List String
    }


init : ( Model, Cmd Msg )
init =
    ( Model "" [], Cmd.none )


type Msg
    = Input String
    | Send
    | NewMessage String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Input newInput ->
            ( { model | input = newInput }, Cmd.none )

        Send ->
            ( { model | input = "" }, WebSocket.send "ws://echo.websocket.org" model.input )

        NewMessage message ->
            ( { model | messages = (message :: model.messages) }, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ div []
            (model.messages
                |> List.reverse
                |> List.map viewMessage
            )
        , input [ onInput Input, value model.input ] []
        , button [ onClick Send ] [ text "Send" ]
        ]


viewMessage : String -> Html Msg
viewMessage message =
    div [] [ text message ]


subscriptions : Model -> Sub Msg
subscriptions model =
    WebSocket.listen "ws://echo.websocket.org" NewMessage
