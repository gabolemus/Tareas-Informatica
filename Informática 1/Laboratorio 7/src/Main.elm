module Main exposing (..)

--Ejercicio 1
type Arbol = Vacio | Nodo Int Arbol Arbol


nodoFinal : Int -> Arbol
nodoFinal x =
    Nodo x Vacio Vacio

crearArbol : Int -> Arbol -> Arbol -> Arbol
crearArbol x x1 x2 =
    Nodo x x1 x2



--Ejercicio 2
masUno : Arbol -> Arbol
masUno x =
    case x of
        Vacio ->
            Vacio

        Nodo centro izquierda derecha ->
            Nodo (centro + 1) (masUno izquierda) (masUno derecha)



--Ejercicio 3
map : (Int -> Int) -> Arbol -> Arbol
map f x =
    case x of
        Vacio ->
            Vacio

        Nodo centro izquierda derecha ->
            Nodo (f centro) (map f izquierda) (map f derecha)



--Ejercicio 4
sum : Arbol -> Int
sum x =
    case x of
        Vacio ->
            0

        Nodo centro izquierda derecha ->
            centro + sum izquierda + sum derecha

--Ejercicio 5
foldTree : (Int -> Int -> Int -> Int) -> Int -> Arbol -> Int
foldTree f num tree =
    case tree of
        Vacio -> num
        Nodo centro izquierda derecha -> f (centro) (foldTree f num izquierda) (foldTree f num derecha)
