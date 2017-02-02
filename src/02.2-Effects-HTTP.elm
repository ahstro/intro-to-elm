module HTTP exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Json.Decode
import Http


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


type alias Model =
    { topic : String
    , gifUrl : String
    , error : Maybe String
    }


init : ( Model, Cmd Msg )
init =
    ( Model "cats" "./images/waiting.gif" Nothing, Cmd.none )


type Msg
    = MorePlease
    | NewGif (Result Http.Error String)
    | NewTopic String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MorePlease ->
            ( { model | error = Nothing }, getRandomGif model.topic )

        NewTopic newTopic ->
            ( { model | topic = newTopic }, Cmd.none )

        NewGif (Ok newUrl) ->
            ( { model | gifUrl = newUrl }, Cmd.none )

        NewGif (Err error) ->
            ( { model | error = Just (getStringFromHttpError error) }, Cmd.none )


getStringFromHttpError : Http.Error -> String
getStringFromHttpError error =
    case error of
        Http.BadUrl badUrl ->
            badUrl

        Http.Timeout ->
            "timeout"

        Http.NetworkError ->
            "network error"

        Http.BadStatus res ->
            res.status.message

        Http.BadPayload error _ ->
            error


getRandomGif : String -> Cmd Msg
getRandomGif topic =
    let
        url =
            "https://api.giphy.com/v1/gifs/random?api_key=dc6zaTOxFJmzC&tag=" ++ topic

        request =
            Http.get url decodeGifUrl
    in
        Http.send NewGif request


decodeGifUrl : Json.Decode.Decoder String
decodeGifUrl =
    Json.Decode.at [ "data", "image_url" ] Json.Decode.string


view : Model -> Html Msg
view model =
    div []
        [ select [ onInput NewTopic ]
          [ option [ value "cats" ] [ text "cats" ]
          , option [ value "dogs" ] [ text "dogs" ]
          , option [ value "slow loris" ] [ text "slow loris" ]
          ]
        , h2 [] [ text (getError model.error) ]
        , img [ src model.gifUrl ] []
        , button [ onClick MorePlease ] [ text "More please" ]
        ]


getError : Maybe String -> String
getError error =
    case error of
        Just error ->
            error

        Nothing ->
            ""


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
