module Forms exposing (..)

import Debug
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onInput, onClick)
import String
import Regex


main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }



--  Model


type alias Model =
    { name : String
    , password : String
    , passwordAgain : String
    , age : String
    , showError : Bool
    }


model : Model
model =
    Model "" "" "" "" False



-- Update


type Msg
    = Name String
    | Password String
    | PasswordAgain String
    | Age String
    | ShowError Bool


update : Msg -> Model -> Model
update msg model =
    case msg of
        Name name ->
            { model | name = name }

        Age age ->
            { model | age = age }

        Password password ->
            { model | password = password }

        PasswordAgain passwordAgain ->
            { model | passwordAgain = passwordAgain }

        ShowError showError ->
            { model | showError = showError }



-- View


view : Model -> Html Msg
view model =
    div []
        [ input [ type_ "text", onInput Name ] []
        , input [ type_ "password", onInput Password ] []
        , input [ type_ "password", onInput PasswordAgain ] []
        , input [ type_ "number", onInput Age ] []
        , button [ onClick (ShowError True) ] [ text "Submit" ]
        , viewValidation model
        ]


isPositive : Int -> Result String Int
isPositive number =
    if number >= 0 then
        Ok number
    else
        Err "negative"


isParsableAsInt : String -> Bool
isParsableAsInt str =
    let
        age =
            String.toInt str |> Result.andThen isPositive
    in
        case age of
            Ok _ ->
                True

            _ ->
                False


viewValidation : Model -> Html msg
viewValidation model =
    let
        ( color, message ) =
            if String.length model.password < 8 then
                ( "red", "too short" )
            else if
                Regex.contains (Regex.regex "[A-Z]") model.password
                    == False
                    || Regex.contains (Regex.regex "[a-z]") model.password
                    == False
                    || Regex.contains (Regex.regex "[0-9]") model.password
                    == False
            then
                ( "red", "invalid" )
            else if model.password /= model.passwordAgain then
                ( "red", "no match" )
            else if isParsableAsInt model.age == False then
                ( "red", "invalid age" )
            else
                ( "green", "OK" )
    in
        if model.showError then
            div [ style [ ( "color", color ) ] ] [ text message ]
        else
            div [] []
