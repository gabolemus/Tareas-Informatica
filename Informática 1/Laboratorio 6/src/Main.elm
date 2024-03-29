module Main exposing (iFilter, filter, zipWith, iZipWith)

--Ejercicio 1
iFilter : Int -> List Int -> List Int
iFilter n ns =
    case ( n, ns ) of
        ( a, [] ) ->
            []

        ( 1, cs ) ->
            []

        ( x, d :: ds ) ->
            if modBy x d /= 0 then
                d :: iFilter n ds

            else
                iFilter n ds

esPar : Int -> Bool
esPar x =
    if modBy 2 x == 0 then
        True

    else
        False

esImpar : Int -> Bool
esImpar x =
    not (esPar x)



--Ejercicio 2
filter : (Int -> Bool) -> List Int -> List Int
filter func nx =
    case nx of
        [] ->
            []

        b :: bs ->
            if func b then
                b :: filter func bs

            else
                filter func bs

invsPop : List Int -> List Int
invsPop nx =
    case nx of
        [] ->
            []

        x :: lx ->
            lx

pop : List Int -> List Int
pop nx =
    nx |> List.reverse |> invsPop |> List.reverse



--Ejercicio 3
iZipWith nx ny =
    case ( nx, ny ) of
        ( [], [] ) ->
            []

        ( a, [] ) ->
            []

        ( [], b ) ->
            []

        ( x :: lx, y :: ly ) ->
            if List.length nx == List.length ny then
                (x + y) :: iZipWith lx ly

            else if List.length nx > List.length ny then
                iZipWith (pop nx) ny

            else
                iZipWith nx (pop ny)



--Ejercicio 4
zipWith : (Int -> Int -> Int) -> List Int -> List Int -> List Int
zipWith func nx ny =
    case ( nx, ny ) of
        ( [], [] ) ->
            []

        ( n :: ns, [] ) ->
            []

        ( [], m :: ms ) ->
            []

        ( x :: lx, y :: ly ) ->
            if List.length nx == List.length ny then
                func x y :: zipWith func lx ly

            else if List.length nx > List.length ny then
                zipWith func (pop nx) ny

            else
                zipWith func nx (pop ny)
