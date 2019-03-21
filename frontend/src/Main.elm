module Main exposing (Model, Msg(..), emptyModel, init, main, update, view)

import Array2D
import Browser
import Browser.Dom
import Browser.Events
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Events
import ExternalUrls
import GameLogic
import Html
import Html.Attributes
import Json.Decode as Decode


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


init : ( Model, Cmd Msg )
init =
    ( emptyModel, Cmd.none )



-- MODEL


type alias Model =
    { board : GameLogic.Board
    , selectedPiece : GameLogic.Piece
    , hoveredSquare : Maybe Array2D.Position
    , previousMove : Maybe Array2D.Position
    }


emptyModel : Model
emptyModel =
    { board = GameLogic.startingBoard
    , selectedPiece = GameLogic.X
    , hoveredSquare = Nothing
    , previousMove = Nothing
    }


placePiece :
    GameLogic.Piece
    -> Array2D.Position
    -> Model
    -> Model
placePiece piece pos model =
    { model
        | board =
            GameLogic.placePiece pos piece model.board
                |> Result.withDefault model.board
        , previousMove = Just pos
    }



-- UPDATE


type Msg
    = NoOp
    | Placing GameLogic.Piece
    | ClickedSquare Array2D.Position
    | MouseEnter Array2D.Position
    | MouseLeave Array2D.Position


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        Placing piece ->
            ( { model | selectedPiece = piece }, Cmd.none )

        ClickedSquare pos ->
            ( placePiece model.selectedPiece pos model, Cmd.none )

        MouseEnter pos ->
            ( { model | hoveredSquare = Just pos }, Cmd.none )

        MouseLeave pos ->
            ( { model | hoveredSquare = Nothing }, Cmd.none )



-- VIEW


squareView :
    Model
    -> Array2D.Position
    -> GameLogic.Square
    -> Element Msg
squareView model pos square =
    Element.el
        [ Element.Background.color <|
            if xor (modBy 2 pos.x == 0) (modBy 2 pos.y == 0) then
                -- generates a checkerboard pattern
                Element.rgb255 181 136 99

            else
                Element.rgb255 238 215 180
        , Element.Border.innerGlow (Element.rgb255 0 0 100)
            (case model.previousMove of
                Just previousMovePos ->
                    if pos == previousMovePos then
                        2

                    else
                        0

                Nothing ->
                    0
            )
        , Element.width Element.fill
        , Element.height Element.fill
        , Element.Events.onClick (ClickedSquare pos)
        , Element.Events.onMouseEnter (MouseEnter pos)
        , Element.Events.onMouseLeave (MouseLeave pos)
        ]
        (Element.el
            [ Element.centerX
            , Element.centerY
            ]
            (case ( square, model.hoveredSquare ) of
                ( GameLogic.Has piece, _ ) ->
                    Element.image []
                        { src = ExternalUrls.pieceImage piece
                        , description = ""
                        }

                ( GameLogic.Empty, Just hoveredPos ) ->
                    if pos == hoveredPos then
                        Element.image [ Element.alpha 0.2 ]
                            { src = ExternalUrls.pieceImage model.selectedPiece
                            , description = ""
                            }

                    else
                        Element.none

                _ ->
                    Element.none
            )
        )


boardSide : Int
boardSide =
    400


squareSide : Int
squareSide =
    boardSide // GameLogic.boardSize.width


view : Model -> Html.Html Msg
view model =
    Element.column
        [ Element.centerX
        , Element.paddingEach { top = 50, right = 0, bottom = 0, left = 0 }
        , Element.spacing 20
        ]
        [ GameLogic.toArray model.board
            |> Array2D.indexedMap (squareView model)
            |> Array2D.rows
            |> List.map
                (Element.row
                    [ Element.width <| Element.px boardSide
                    , Element.height <| Element.px squareSide
                    ]
                )
            |> Element.column [ Element.height <| Element.px boardSide ]
        , List.map
            (\piece ->
                Element.el
                    [ Element.width <| Element.px squareSide
                    , Element.height <| Element.px squareSide
                    , Element.Events.onClick (Placing piece)
                    , Element.Border.glow (Element.rgb255 0 0 100) <|
                        if piece == model.selectedPiece then
                            2

                        else
                            0
                    ]
                    (Element.image
                        [ Element.centerX, Element.centerY ]
                        { src = ExternalUrls.pieceImage piece, description = "" }
                    )
            )
            [ GameLogic.X, GameLogic.O ]
            |> Element.row
                [ Element.spacing 30
                , Element.height <| Element.px squareSide
                , Element.centerX
                , Element.padding 10
                ]
        ]
        |> Element.layout
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.htmlAttribute <| Html.Attributes.style "user-select" "none"
            ]

..
