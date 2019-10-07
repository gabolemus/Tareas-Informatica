module Main exposing (..)

--Ejercicio 1
type Grupo a
    = Valor a
    | Suma (Grupo a) (Grupo a)
    | Inverso (Grupo a)

--Ejercicio 2
type Algebra a b
    = Algebra (a -> b) (b -> b -> b) (b -> b)

iValor1 a = a
iSuma1 a b = a + b
iInverso1 a = -a
algebra1 = Algebra iValor1 iSuma1 iInverso1

expr1 = (Suma (Valor 3) (Suma (Valor 5) (Inverso (Valor 3))))
expr2 = Suma (Suma (Valor 2) (Suma (Valor 7) (Inverso (Valor 3)))) (Suma (Valor 3) (Suma (Valor 5) (Inverso (Valor 2))))

--Ejercicio 3
evaluar : Algebra a b -> Grupo a -> b
evaluar (Algebra iValor iSuma iInverso) group =
    let
        alg =
            Algebra iValor iSuma iInverso
    in
        case group of
            Valor x ->
                iValor x

            Suma a b ->
                iSuma (evaluar alg a) (evaluar alg b)

            Inverso x ->
                iInverso (evaluar alg x)

modX x = modBy x

--Ejercicio 4
zAlgebra : Int -> Algebra Int Int
zAlgebra num =
    let
        iValor x = num
        iSuma a b = a + b
        iInverso x = -num
        --iInverso x = modX num

    in
        Algebra iValor iSuma iInverso
