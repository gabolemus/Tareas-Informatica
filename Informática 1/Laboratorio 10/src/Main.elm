module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)

type alias Model =
    { number : String,
    listOfResults : List String,
    result : String }


type Msg
    = Concat String
    | Sumar
    | Multiplicar
    | Resetear
    | Calcular

strToInt : String -> Int
strToInt num = Maybe.withDefault 0 (String.toInt num)

listToString : List String -> String
listToString list =
    case list of
        [] -> ""
        x :: xs -> x ++ listToString xs

isNotOperator : String -> Bool
isNotOperator str =
    case str of
        x -> if x == "+" || x == "*" then False else True 

combineExpressionAux : List String -> List String
combineExpressionAux list =
    case list of
        [] -> []
        [x] -> [x]
        x :: y :: xys -> if isNotOperator x && isNotOperator y then (x ++ y) :: combineExpression xys
                         else if not(isNotOperator x) then x :: combineExpression (y :: xys)
                         else ([x] ++ [y]) ++ combineExpression xys

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
        [] -> [""]
        x :: xs -> xs

evaluateMultiplication : List String -> List String
evaluateMultiplication list =
    case list of
        [] -> ["0"]
        [x] -> [x]
        x :: y :: xys -> if y == "*" then evaluateMultiplication ((String.fromInt ((strToInt x) * (strToInt (firstElement xys)))) :: (restOfList xys))
                         else x :: y :: evaluateMultiplication xys

evaluateAddition : List String -> List String
evaluateAddition list =
    case list of
        [] -> ["0"]
        [x] -> [x]
        x :: y :: xys -> if y == "+" then evaluateAddition ((String.fromInt ((strToInt x) + (strToInt (firstElement xys)))) :: (restOfList xys))
                         else x :: y :: evaluateAddition xys

evaluate : List String -> String
evaluate list =
    combineExpression list |> evaluateMultiplication |> evaluateAddition |> firstElement

init : Model
init =
    Model "0" ["0"] "0"

-- number, listOfResults, result

update : Msg -> Model -> Model
update msg model =
    case msg of
        Concat x -> if model.number == "0" then Model x [x] "0" 
            else if model.number == model.result then  Model x [x] "0"
            else Model (model.number ++ x) (model.listOfResults ++ [x]) "0"
        Sumar -> Model (model.number ++ "+") (model.listOfResults ++ ["+"]) "0"
        Multiplicar -> Model (model.number ++ "*") (model.listOfResults ++ ["*"]) "0"
        Resetear -> Model "0" ["0"] "0" 
        Calcular -> Model (evaluate model.listOfResults) (model.listOfResults) (evaluate model.listOfResults)

view : Model -> Html Msg
view model =
    div [] [
        button [ onClick (Concat "9") ] [ text "9" ],
        button [ onClick (Concat "8") ] [ text "8" ],
        button [ onClick (Concat "7") ] [ text "7" ],
        button [ onClick (Concat "6") ] [ text "6" ],
        button [ onClick (Concat "5") ] [ text "5" ],
        button [ onClick (Concat "4") ] [ text "4" ],
        button [ onClick (Concat "3") ] [ text "3" ],
        button [ onClick (Concat "2") ] [ text "2" ],
        button [ onClick (Concat "1") ] [ text "1" ],
        button [ onClick (Concat "0") ] [ text "0" ],
        button [ onClick Sumar ] [ text "+" ],
        button [ onClick Multiplicar ] [ text "*" ],
        button [ onClick Calcular ] [ text "=" ],
        button [ onClick Resetear ] [ text "Resetear" ],
        div [] [ text (listToString model.listOfResults) ],
        div [] [ text "=" ],
        div [] [ text model.result ]
    ]

main =
    Browser.sandbox
    {
        init = init,
        view = view,
        update = update
    }
