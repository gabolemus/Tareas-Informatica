module Main exposing (..)

--Ejercicio 1
type Arbol a
    = Vacio
    | Nodo a (Arbol a) (Arbol a)



--Ejercicio 2
map : (a -> b) -> Arbol a -> Arbol b
map f tree =
    case tree of
        Vacio ->
            Vacio

        Nodo centro izquierda derecha ->
            Nodo (f centro) (map f izquierda) (map f derecha)



--Ejercicio 3
filtrar : (a -> Bool) -> Arbol a -> List a
filtrar f tree =
    case tree of
        Vacio ->
            []

        Nodo centro izquierda derecha ->
            if f centro then
                centro :: (filtrar f izquierda ++ filtrar f derecha)

            else
                filtrar f izquierda ++ filtrar f derecha



--Ejercicio 4
foldTree : (a -> b -> b -> b) -> b -> Arbol a -> b
foldTree f element tree =
    case tree of
        Vacio ->
            element

        Nodo centro izquierda derecha ->
            f centro (foldTree f element izquierda) (foldTree f element derecha)



-- --Ejercicio 5
-- filtrarFold : (a -> Bool) -> Arbol a -> List a
filtrarFold f tree =
    let
        inicial = []
        combinacion derecha izquierda valor = 
            if f valor then valor :: (derecha ++ izquierda)
            else (derecha ++ izquierda)
    in
        foldTree combinacion inicial tree
