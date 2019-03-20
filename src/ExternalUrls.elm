module ExternalUrls exposing (assetsDir, pieceImage)

import GameLogic
import Url.Builder as Builder


assetsDir =
    Builder.relative [ "assets" ] []


pieceImage : GameLogic.Piece -> String
pieceImage piece =
    assetsDir
        ++ "/"
        ++ (case piece of
                GameLogic.X ->
                    "piece_x.svg"

                GameLogic.O ->
                    "piece_o.svg"
           )
