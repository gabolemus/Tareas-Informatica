module Fractales exposing (..)

import Browser
import Round
import Html exposing (..)
import Html.Attributes as Hta
import Html.Events exposing (..)
import Svg exposing (..)
import Svg.Attributes as Sva

-- Funciones -- =========================================================================================================================================================================

type alias PointXY =
    { x : Float, y : Float }

listPointsToListFloat : List PointXY -> List (Float, Float)
listPointsToListFloat list =
    case list of
        [] -> []
        punto :: resto -> ( punto.x, punto.y ) :: listPointsToListFloat resto

listFloatsToPointXY : List (Float, Float) -> List PointXY
listFloatsToPointXY list =
    case list of
        [] -> []
        (a,b) :: abs -> (PointXY a b) :: listFloatsToPointXY (abs)

pointToString : (Float, Float) -> String
pointToString point =
    case point of
        ( a, b ) -> String.fromFloat a ++ "," ++ String.fromFloat b

pointsToString : List (Float, Float) -> String
pointsToString list =
    case list of
        [] -> ""
        (x, y) :: xys -> pointToString (x, y) ++ " " ++ pointsToString xys

firstElementOfList : List (Float, Float) -> (Float, Float)
firstElementOfList list =
    case list of
        [] -> (0, 0)
        x :: xs -> x

listWithFirstElement : List (Float, Float) -> List (Float, Float)
listWithFirstElement list =
    list ++ [ firstElementOfList list ]

noDuplicates : List a -> List a
noDuplicates list =
    case list of
        [] -> []

        [x] -> [x]

        x :: y :: resto ->
            if x == y then
                noDuplicates (x :: resto)
            else
                x :: noDuplicates (y :: resto)

removeFirst : List a -> List a
removeFirst list =
    case list of
        [] -> []
        [x] -> []
        x :: xs -> xs

pop : List a -> List a
pop list = List.reverse list |> removeFirst |> List.reverse

round : Float -> Float 
round x = Round.round 2 x |> String.toFloat |> Maybe.withDefault 0

stringToInt : String -> Int
stringToInt str = String.toInt str |> Maybe.withDefault 0 

roundedList : List (Float, Float) -> List (Float, Float)
roundedList list =
    case list of
        [] -> []
        (x, y) :: xys -> (round x, round y) :: roundedList xys

listOfRoundedLists : List (List (Float, Float)) -> List (List (Float, Float))
listOfRoundedLists list =
    case list of
        [] -> []
        x :: xs ->roundedList x :: listOfRoundedLists xs

listOfListsToPolylines : Int -> List (List (Float, Float)) -> List (Svg.Svg msg)
listOfListsToPolylines num list =
    if num == 1 then
        case list of
            [] -> []
            x :: xs -> polyline [ Sva.points (pointsToString (listWithFirstElement x)), Sva.fill "blue", Sva.stroke "black", Sva.strokeWidth "0.5" ] [] :: listOfListsToPolylines 1 xs
    else
        case list of
            [] -> []
            x :: xs -> polyline [ Sva.points (pointsToString (listWithFirstElement x)), Sva.fill "blue", Sva.stroke "none", Sva.strokeWidth "0.5" ] [] :: listOfListsToPolylines 2 xs

-- Copo de Nieve de Koch -- =============================================================================================================================================================

length = 300

p1 = PointXY ((500 - length) / 2) 135 -- (100,135)
p2 = PointXY ((500 - length) * 2) 135 -- (400,135)
p3 = PointXY (((p2.x - p1.x) / 2) + p1.x) (p1.y + ((p2.x - p1.x) * sin (pi / 3))) -- (250,394.81)

t1 = PointXY ((500 - length) / 2) 350 -- (100,350)

add : PointXY -> PointXY -> PointXY
add a b = PointXY (a.x + b.x) (a.y + b.y)

mult : Float -> PointXY -> PointXY
mult num point = PointXY (num * point.x) (num * point.y)

sub : PointXY -> PointXY -> PointXY
sub a b = mult -1 b |> add a

kochCurveAux : PointXY -> PointXY -> List (PointXY)
kochCurveAux punto1 punto2 =
    let
        distanciaXY = sub punto2 punto1
    in
        [
            punto1
            , add punto1 <| mult (1 / 3) distanciaXY
            , add punto1 <| add (mult (1 / 2) distanciaXY) (mult (sqrt 3 / 6) <| PointXY distanciaXY.y (-1 * distanciaXY.x))
            , add punto1 <| mult (2 / 3) distanciaXY
            , punto2
        ]
            
kochCurve : PointXY -> PointXY -> List (Float, Float)
kochCurve punto1 punto2 =
    kochCurveAux punto1 punto2 |> listPointsToListFloat

applyKochCurve : List PointXY -> List (Float, Float)
applyKochCurve list =
    case list of
        [] -> []
        [x] -> listPointsToListFloat [x]
        x :: y :: xys -> (kochCurve x y) ++ applyKochCurve (y :: xys)

listOfPoints : PointXY -> PointXY -> List (Float, Float)
listOfPoints punto1 punto2 =
    (listPointsToListFloat [punto1]) ++ (listPointsToListFloat [punto2])

snowflake : Int -> List (Float, Float)
snowflake num =
    if num < 0 then []
    else if num == 0 then ((listOfPoints p1 p2) ++ (listOfPoints p2 p3) ++ (listOfPoints p3 p1)) |> noDuplicates |> roundedList
    else (snowflake (num - 1) |> listFloatsToPointXY |> applyKochCurve) |> roundedList |> pop

-- Triángulo de Sierpinski -- =============================================================================================================================================================

triangle : Float -> Float -> Float -> List (Float, Float)
triangle x y len = 
    case (x, y) of
        (a, b) -> [(a, b), (a + len, b), (a + len / 2, b - (len * sin (pi / 3)))]

divide : Float -> Float -> Float -> Int -> Int -> List (List (Float, Float))
divide x y len lvl max =
    if lvl == max then
        [triangle x y len]

    else
        divide x y (len / 2) (lvl + 1) max
            ++ divide (x + len / 2) y (len / 2) (lvl + 1) max
            ++ divide (x + len / 4) (y - sin (pi / 3) * len / 2) (len / 2) (lvl + 1) max

sierpinski : Int -> List (List (Float, Float))
sierpinski num =
    divide t1.x t1.y length 0 num |> listOfRoundedLists

-- Esponja de Menger -- =============================================================================================================================================================

square : Float -> Float -> Float -> List (Float, Float)
square x y len =
    case (x, y) of
        (a, b) -> [(a, b), (a + len, b), (a + len, b + len), (a, b + len)]

divideSquare : Float -> Float -> Float -> Int -> Int -> List (List (Float, Float))
divideSquare x y len lvl max =
    if max < 0 then []

    else if lvl == max then [ square x y len ]

    else
        divideSquare x y (len / 3) (lvl + 1) max
            ++ divideSquare (x + (len / 3)) y (len / 3) (lvl + 1) max
            ++ divideSquare (x + (2 * (len / 3))) y (len / 3) (lvl + 1) max
            ++ divideSquare x (y + (len / 3)) (len / 3) (lvl + 1) max
            ++ divideSquare (x + (2 * (len / 3))) (y + (len / 3)) (len / 3) (lvl + 1) max
            ++ divideSquare x (y + (2 * (len / 3))) (len / 3) (lvl + 1) max
            ++ divideSquare (x + (len / 3)) (y + (2 * (len / 3))) (len / 3) (lvl + 1) max
            ++ divideSquare (x + (2 * (len / 3))) (y + (2 * (len / 3))) (len / 3) (lvl + 1) max

mengerSponge : Int -> List (List (Float, Float))
mengerSponge num =
    divideSquare p1.x p1.y length 0 num |> listOfRoundedLists

-- Web App -- ===========================================================================================================================================================================

determineNum : Int -> Int -> Int
determineNum caso num =
    case caso of
        1 -> if num <= 0 then 0 else if num >= 5 then 5 else num
        2 -> if num <= 0 then 0 else if num >= 8 then 8 else num
        _ -> if num <= 0 then 0 else if num >= 4 then 4 else num

type alias Model =
    { iterationKoch : Int, kochS : String, kochState : Bool
    , iterationSierpinski : Int, sierpinskiT : List (List (Float, Float)), sierpinskiState : Bool
    , iterationMenger : Int, mengerS : List (List (Float, Float)), mengerState : Bool }

type Msg
    = IncrementarKoch
    | DisminuirKoch
    | ChangeKochIter String
    | ResetearKoch
    | IncrementarSierpinski
    | DisminuirSierpinski
    | ChangeSierpinskiIter String
    | ResetearSierpinski
    | IncrementarMenger
    | DisminuirMenger
    | ChangeMengerIter String
    | ResetearMenger

init : Model
init = Model 0 (pointsToString (listWithFirstElement (snowflake 0))) True 0 (sierpinski 0) False 0 (mengerSponge 0) False

update : Msg -> Model -> Model
update msg model =
    case msg of
        IncrementarKoch -> { model | iterationKoch = model.iterationKoch + 1, kochS = pointsToString (listWithFirstElement (snowflake (model.iterationKoch + 1))), kochState = True, sierpinskiState = False, mengerState = False }

        DisminuirKoch ->
            if model.iterationKoch == 0 then { model | iterationKoch = 0, kochS = init.kochS, kochState = True, sierpinskiState = False, mengerState = False }
            else { model | iterationKoch = model.iterationKoch - 1, kochS = pointsToString (listWithFirstElement (snowflake (model.iterationKoch - 1))), kochState = True, sierpinskiState = False, mengerState = False }

        ChangeKochIter num -> { model | iterationKoch = (determineNum 1 (stringToInt num)), kochS = pointsToString (listWithFirstElement (snowflake (determineNum 1 (stringToInt num)))), kochState = True, sierpinskiState = False, mengerState = False }

        ResetearKoch -> { model | iterationKoch = init.iterationKoch, kochS = init.kochS, kochState = True, sierpinskiState = False, mengerState = False }

        IncrementarSierpinski -> { model | iterationSierpinski = model.iterationSierpinski + 1, sierpinskiT = sierpinski (model.iterationSierpinski + 1), kochState = False, sierpinskiState = True, mengerState = False }

        DisminuirSierpinski ->
            if model.iterationSierpinski == 0 then { model | iterationSierpinski = 0, sierpinskiT = init.sierpinskiT, kochState = False, sierpinskiState = True, mengerState = False }
            else { model | iterationSierpinski = model.iterationSierpinski - 1, sierpinskiT = sierpinski (model.iterationSierpinski - 1), kochState = False, sierpinskiState = True, mengerState = False }

        ChangeSierpinskiIter num -> { model | iterationSierpinski = (determineNum 2 (stringToInt num)), sierpinskiT = sierpinski (stringToInt num), kochState = False, sierpinskiState = True, mengerState = False }

        ResetearSierpinski -> { model | iterationSierpinski = 0, sierpinskiT = init.sierpinskiT, kochState = False, sierpinskiState = True, mengerState = False }

        IncrementarMenger -> { model | iterationMenger = model.iterationMenger + 1, mengerS = mengerSponge (model.iterationMenger + 1), kochState = False, sierpinskiState = False, mengerState = True }

        DisminuirMenger ->
            if model.iterationMenger == 0 then { model | iterationMenger = 0, mengerS = init.mengerS, kochState = False, sierpinskiState = False, mengerState = True }
            else { model | iterationMenger = model.iterationMenger - 1, mengerS = mengerSponge (model.iterationMenger - 1), kochState = False, sierpinskiState = False, mengerState = True }

        ChangeMengerIter num -> { model | iterationMenger = (determineNum 3 (stringToInt num)), mengerS = mengerSponge (stringToInt num), kochState = False, sierpinskiState = False, mengerState = True }

        ResetearMenger -> { model | iterationMenger = 0, mengerS = init.mengerS, kochState = False, sierpinskiState = False, mengerState = True }


view : Model -> Html Msg
view model =
    div [ Hta.class "main-div" ]
        [
            input [ Hta.id "tab1", Hta.type_ "radio", Hta.name "tabs", Hta.checked model.kochState ] []
            , label [ Hta.for "tab1" ] [ Html.text "Copo de Nieve de Koch" ]
            , input [ Hta.id "tab2", Hta.type_ "radio", Hta.name "tabs", Hta.checked model.sierpinskiState ] []
            , label [ Hta.for "tab2" ] [ Html.text "Triángulo de Sierpinski" ]
            , input [ Hta.id "tab3", Hta.type_ "radio", Hta.name "tabs", Hta.checked model.mengerState ] []
            , label [ Hta.for "tab3" ] [ Html.text "Esponja de Menger" ]
            , div [ Hta.class "content" ]
            [
                div [ Hta.class "fractal-div", Hta.id "fractal1" ]
                [ 
                    svg [ Sva.viewBox "0 0 500 500", Sva.width "500", Sva.height "500", Sva.class "svgWindow" ]
                    [ 
                        polyline [ Sva.points model.kochS, Sva.fill "none", Sva.stroke "blue", Sva.strokeWidth "1.5" ] [] 
                    ]
                    , div [ Hta.class "counter" ] [ Html.text ("Iteración: " ++ String.fromInt model.iterationKoch) ]
                    , div [ Hta.class "control-buttons" ]
                    [ 
                        button [ onClick DisminuirKoch, Hta.class "fractalButton" ] [ Html.text "<<" ]
                        , input [ Hta.type_ "text", Hta.value (String.fromInt (model.iterationKoch)), Hta.class "fractal-iter-text", onInput ChangeKochIter ] []
                        , button [ onClick IncrementarKoch, Hta.class "fractalButton" ] [ Html.text ">>" ]
                        , button [ onClick ResetearKoch, Hta.class "fractalButton" ] [ Html.text "Resetear Fractal" ]
                    ]
                ]
                , div [ Hta.class "fractal-div", Hta.id "fractal2" ]
                [
                    svg [ Sva.viewBox "0 0 500 500", Sva.width "500", Sva.height "500", Sva.class "svgWindow" ]
                        (listOfListsToPolylines 2 model.sierpinskiT)
                    , div [ Hta.class "counter" ] [ Html.text ("Iteración: " ++ String.fromInt model.iterationSierpinski) ]
                    , div [ Hta.class "control-buttons" ]
                    [
                        button [ onClick DisminuirSierpinski, Hta.class "fractalButton" ] [ Html.text "<<" ]
                        , input [ Hta.type_ "text", Hta.value (String.fromInt (model.iterationSierpinski)), Hta.class "fractal-iter-text", onInput ChangeSierpinskiIter ] []
                        , button [ onClick IncrementarSierpinski, Hta.class "fractalButton" ] [ Html.text ">>" ]
                        , button [ onClick ResetearSierpinski, Hta.class "fractalButton" ] [ Html.text "Resetear Fractal" ]
                    ]
                ]
                , div [ Hta.class "fractal-div", Hta.id "fractal3" ]
                [ 
                    svg [ Sva.viewBox "0 0 500 500", Sva.width "500", Sva.height "500", Sva.class "svgWindow" ]
                        (listOfListsToPolylines 1 model.mengerS)
                    , div [ Hta.class "counter" ] [ Html.text ("Iteración: " ++ String.fromInt model.iterationMenger) ]
                    , div [ Hta.class "control-buttons" ]
                    [ 
                        button [ onClick DisminuirMenger, Hta.class "fractalButton" ] [ Html.text "<<" ]
                        , input [ Hta.type_ "text", Hta.value (String.fromInt (model.iterationMenger)), Hta.class "fractal-iter-text", onInput ChangeMengerIter ] []
                        , button [ onClick IncrementarMenger, Hta.class "fractalButton" ] [ Html.text ">>" ]
                        , button [ onClick ResetearMenger, Hta.class "fractalButton" ] [ Html.text "Resetear Fractal" ]
                    ]
                ]
            ]
        ]

main =
    Browser.sandbox
        { init = init, view = view, update = update }
