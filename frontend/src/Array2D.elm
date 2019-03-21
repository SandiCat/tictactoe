module Array2D exposing
    ( Array2D
    , Position
    , Size
    , get
    , height
    , indexedMap
    , map
    , repeat
    , rows
    , set
    , width
    )

import Array exposing (Array)


type alias Array2D a =
    { data : Array (Array a)
    , size : Size
    }


type alias Size =
    { width : Int, height : Int }


type alias Position =
    { x : Int, y : Int }


repeat : Size -> a -> Array2D a
repeat size elem =
    { data =
        Array.repeat size.height elem
            |> Array.repeat size.width
    , size = size
    }


rows : Array2D a -> List (List a)
rows { data } =
    Array.toList data
        |> List.map Array.toList


get : Position -> Array2D a -> Maybe a
get { x, y } =
    .data >> Array.get y >> Maybe.andThen (Array.get x)


set : Position -> a -> Array2D a -> Array2D a
set { x, y } elem array =
    case Array.get y array.data of
        Just oldRow ->
            { array | data = Array.set y (Array.set x elem oldRow) array.data }

        Nothing ->
            array


map : (a -> b) -> Array2D a -> Array2D b
map f array =
    { data = Array.map (Array.map f) array.data, size = array.size }


indexedMap : (Position -> a -> b) -> Array2D a -> Array2D b
indexedMap f array =
    { size = array.size
    , data = Array.indexedMap (\y row -> Array.indexedMap (\x -> f <| Position x y) row) array.data
    }


width : Array2D a -> Int
width =
    .size >> .width


height : Array2D a -> Int
height =
    .size >> .height
