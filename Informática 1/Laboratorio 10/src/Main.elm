module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


strToInt : String -> Int
strToInt num =
    String.toInt num |> Maybe.withDefault 0

listToString : List String -> String
listToString list =
    case list of
        [] -> ""
        x :: xs -> x ++ listToString xs

isNotOperator : String -> Bool
isNotOperator str =
    if str == "+" || str == "x" then False else True

combineExpressionAux : List String -> List String
combineExpressionAux list =
    case list of
        [] -> []

        [x] ->[x]

        x :: y :: xys ->
            if isNotOperator x && isNotOperator y then
                (x ++ y) :: combineExpression xys

            else if not (isNotOperator x) then
                x :: combineExpression (y :: xys)

            else
                ([x] ++ [y]) ++ combineExpression xys

combineExpression : List String -> List String
combineExpression list =
    combineExpressionAux list |> combineExpressionAux

firstElement : List String -> String
firstElement list =
    case list of
        [] -> ""
        x :: xs -> x

restOfList : List String -> List String
restOfList list =
    case list of
        [] ->[ "" ]
        x :: xs -> xs

evaluateMultiplication : List String -> List String
evaluateMultiplication list =
    case list of
        [] -> [ "0" ]

        [x] -> [x]

        x :: y :: xys ->
            if y == "x" then
                evaluateMultiplication (String.fromInt (strToInt x * strToInt (firstElement xys)) :: restOfList xys)

            else
                x :: y :: evaluateMultiplication xys

evaluateAddition : List String -> List String
evaluateAddition list =
    case list of
        [] -> [ "0" ]

        [x] -> [x]

        x :: y :: xys ->
            if y == "+" then
                evaluateAddition (String.fromInt (strToInt x + strToInt (firstElement xys)) :: restOfList xys)

            else
                x :: y :: evaluateAddition xys

evaluate : List String -> String
evaluate list =
    combineExpression list |> evaluateMultiplication |> evaluateAddition |> firstElement


type alias Model =
    { number : String
    , expression : List String
    , result : String
    }


type Msg
    = Concat String
    | Sumar
    | Multiplicar
    | Resetear
    | Calcular


init : Model
init =
    Model "0" [ "0" ] "0"


update : Msg -> Model -> Model
update msg model =
    case msg of
        Concat x ->
            if model.number == "0" then
                Model x [x] "0"

            else if model.number == model.result then
                Model x [x] (model.result)

            else
                Model (model.number ++ x) (model.expression ++ [x]) (model.result)

        Sumar ->
            Model (model.number ++ "+") (model.expression ++ [ "+" ]) (model.result)

        Multiplicar ->
            Model (model.number ++ "x") (model.expression ++ [ "x" ]) (model.result)

        Resetear ->
            Model "0" [ "0" ] "0"

        Calcular ->
            Model (evaluate model.expression) model.expression (evaluate model.expression)


view : Model -> Html Msg
view model =
    div [ Html.Attributes.class "main-div" ] [
        div [ Html.Attributes.class "calculator" ]
        [ 
            div [ Html.Attributes.class "resultado" ] [
                div [] [ text (listToString model.expression) ]
                , div [] [ text model.result ]
            ],
            div [ Html.Attributes.class "buttons" ] [
                div [ Html.Attributes.class "7-9x" ] [
                    button [ onClick (Concat "7"), Html.Attributes.class "numButton" ] [ text "7" ]
                    , button [ onClick (Concat "8"), Html.Attributes.class "numButton" ] [ text "8" ]
                    , button [ onClick (Concat "9"), Html.Attributes.class "numButton" ] [ text "9" ]
                    , button [ onClick Multiplicar, Html.Attributes.class "opButton" ] [ text "x" ]
                ],
                div [ Html.Attributes.class "4-6+" ] [
                    button [ onClick (Concat "4"), Html.Attributes.class "numButton" ] [ text "4" ]
                    , button [ onClick (Concat "5"), Html.Attributes.class "numButton" ] [ text "5" ]
                    , button [ onClick (Concat "6"), Html.Attributes.class "numButton" ] [ text "6" ]
                    , button [ onClick Sumar, Html.Attributes.class "opButton" ] [ text "+" ]
                ],
                div [ Html.Attributes.class "1-3=" ] [
                    button [ onClick (Concat "1"), Html.Attributes.class "numButton" ] [ text "1" ]
                    , button [ onClick (Concat "2"), Html.Attributes.class "numButton" ] [ text "2" ]
                    , button [ onClick (Concat "3"), Html.Attributes.class "numButton" ] [ text "3" ]
                    , button [ onClick Calcular, Html.Attributes.class "opButton" ] [ text "=" ]
                ],
                div [ Html.Attributes.class "0_reset" ] [
                    button [ onClick (Concat "0"), Html.Attributes.class "numButton" ] [ text "0" ]
                    , button [ onClick Resetear, Html.Attributes.class "resetButton" ] [ text "AC" ]
                ]
            ]
        ]
    ]


main =
    Browser.sandbox
        { init = init
        , view = view
        , update = update
        }
