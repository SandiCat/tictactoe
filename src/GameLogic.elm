module GameLogic exposing
    ( Board
    , Piece(..)
    , Square(..)
    , boardSize
    , placePiece
    , startingBoard
    , toArray
    )

import Array2D exposing (Array2D)


type Board
    = Board (Array2D Square)


boardSize : Array2D.Size
boardSize =
    { width = 6, height = 6 }


startingBoard : Board
startingBoard =
    Array2D.repeat boardSize Empty |> Board


type Piece
    = X
    | O


type Square
    = Empty
    | Has Piece


type PlacePieceError
    = OccupiedSquare


placePiece : Array2D.Position -> Piece -> Board -> Result PlacePieceError Board
placePiece position piece (Board board) =
    if Array2D.get position board |> Maybe.map ((==) Empty) |> Maybe.withDefault False then
        Array2D.set position (Has piece) board
            |> Board
            |> Result.Ok

    else
        Result.Err OccupiedSquare


toArray : Board -> Array2D Square
toArray (Board board) =
    board
