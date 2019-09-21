module Main exposing (..)

--Ejercicio 1
type Arbol a = Vacio | Nodo a (Arbol a) (Arbol a)



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
                centro :: List.append (filtrar f izquierda) (filtrar f derecha)

            else
                List.append (filtrar f izquierda) (filtrar f derecha)



--Ejercicio 4
--foldTree : (a -> a -> b -> a) -> a -> Arbol b -> a
foldTree : (a -> b -> b -> b) -> b -> Arbol a -> b
foldTree f element tree =
    case tree of
        Vacio ->
            element

        Nodo centro izquierda derecha ->
            f centro (foldTree f element izquierda) (foldTree f element derecha)


--Ejercicio 5
filtrarFold : (a -> Bool) -> Arbol a -> List a
filtrarFold f tree =
    case tree of
        Vacio -> []

        Nodo centro izquierda derecha ->
            foldTree f tree



l1 =
    Nodo 1
        (Nodo 2 (Nodo 4 (Nodo 8 Vacio Vacio) (Nodo 9 Vacio Vacio)) (Nodo 5 (Nodo 10 Vacio Vacio) (Nodo 11 Vacio Vacio)))
        (Nodo 3 (Nodo 6 (Nodo 12 Vacio Vacio) (Nodo 13 Vacio Vacio)) (Nodo 7 (Nodo 14 Vacio Vacio) (Nodo 15 Vacio Vacio)))
