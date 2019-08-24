module Main exposing (esPrimo, fibonacci, nPrimos, primos)

--Ejercicio 2
checkIfPrime : Int -> Int -> Bool
checkIfPrime n x =
    if x <= 1 then
        False

    else if x == 2 then
        True

    else if x == n then
        True

    else if modBy n x == 0 then
        False

    else
        checkIfPrime (n + 1) x

esPrimo : Int -> Bool
esPrimo x =
    checkIfPrime 2 x



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
count n x =
    if n == 0 then
        []

    else if esPrimo x == False then
        count n (x + 1)

    else
        x :: count (n - 1) (x + 1)

nPrimos : Int -> List Int
nPrimos x =
    count x 2
