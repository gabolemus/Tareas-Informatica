module Main exposing (esPrimo, fibonacci, primos, nPrimos)

--Ejercicio 2
checkIfPrime : Int -> Int -> Bool
checkIfPrime x n =
    if x <= 1 then
        False

    else if x == 2 then
        True

    else if x == n then
        True

    else if modBy n x == 0 then
        False

    else
        checkIfPrime x (n + 1)

esPrimo : Int -> Bool
esPrimo x =
    checkIfPrime x 2



--Ejercicio 3
fibonacci : Int -> Int
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
primos x =
    if x <= 1 then
        []

    else if esPrimo x == False then
        primos (x - 1)

    else
        x :: primos (x - 1)



--Ejercicio 5
count x n =
    if x == 0 then
        []

    else if esPrimo n == False then
        count x (n + 1)

    else
        n :: count (x - 1) (n + 1)

nPrimos : Int -> List Int
nPrimos x =
    count x 2
