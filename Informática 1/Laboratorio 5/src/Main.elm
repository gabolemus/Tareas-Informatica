module Main exposing (..)

--Ejercicio 2
primo div x =
    if x == 1 then
        False

    else if x == 2 then
        True

    else if modBy div x == 0 then
        False

    else if div == (x - 1) then
        True

    else
        primo (div + 1) x

esPrimo : Int -> Bool
esPrimo x =
    primo 2 x



--Ejercicio 3
fibonacci x =
    case x of
        0 ->
            0

        1 ->
            1

        n ->
            fibonacci (n - 1) + fibonacci (n - 2)



--Ejercicio 4
primos : Int -> List Int
primos n =
    if n < 1 then
        []

    else if esPrimo n == False then
        primos (n - 1)

    else
        n :: primos (n - 1)



--Ejercicio 5
count ( n, y ) =
    if n == 0 then
        []

    else if esPrimo y == False then
        count ( n, y + 1 )

    else
        y :: count ( n - 1, y + 1 )

nPrimos : Int -> List Int
nPrimos n =
    count ( n, 2 )
