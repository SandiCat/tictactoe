module Main exposing (Model, Msg(..), emptyModel, init, main, update, view)

import Array2D
import Browser
import Browser.Dom
import Browser.Events
import Element exposing (Element)
import Element.Background
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
        , subscriptions = subscriptions
        }


init : ( Model, Cmd Msg )
init =
    ( emptyModel, Cmd.none )



-- MODEL


type alias Model =
    { board : GameLogic.Board
    , placing : Maybe Placing
    }


type alias Placing =
    { piece : GameLogic.Piece
    , clientX : Float
    , clientY : Float
    }


emptyModel : Model
emptyModel =
    { board = GameLogic.startingBoard, placing = Nothing }



-- UPDATE


type Msg
    = NoOp
    | PickedUp GameLogic.Piece
    | ReleasedOver Array2D.Position
    | MouseMoved Float Float


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        PickedUp piece ->
            ( { model | placing = Just <| Placing piece 0.0 0.0 }, Cmd.none )

        ReleasedOver pos ->
            case model.placing of
                Just { piece, clientX, clientY } ->
                    ( { model
                        | board =
                            GameLogic.placePiece pos piece model.board
                                |> Result.withDefault model.board
                        , placing = Nothing
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( model, Cmd.none )

        MouseMoved x y ->
            case model.placing of
                Just placing ->
                    let
                        newPlacing =
                            { placing | clientX = x, clientY = y }
                    in
                    ( { model | placing = Just newPlacing }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )



-- VIEW


squareView : Array2D.Position -> GameLogic.Square -> Element Msg
squareView pos square =
    Element.el
        [ Element.Background.color <|
            if xor (modBy 2 pos.x == 0) (modBy 2 pos.y == 0) then
                -- generates a checkerboard pattern
                Element.rgb255 181 136 99

            else
                Element.rgb255 238 215 180
        , Element.width Element.fill
        , Element.height Element.fill
        , Element.Events.onMouseUp (ReleasedOver pos)
        ]
        (case square of
            GameLogic.Empty ->
                Element.none

            GameLogic.Has piece ->
                Element.image
                    [ Element.centerX
                    , Element.centerY
                    , Element.width Element.fill
                    , Element.height Element.fill
                    ]
                    { src = ExternalUrls.pieceImage piece, description = "" }
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
            |> Array2D.indexedMap squareView
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
                Element.image
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    , Element.Events.onMouseDown (PickedUp piece)
                    , Element.htmlAttribute <| Html.Attributes.class "draggablePiece"

                    {- css class is to disable pointer events on the image (this element only
                       controls a div that is the parent of the image) so that the dragging works
                       properly (no image ghost)
                    -}
                    ]
                    { src = ExternalUrls.pieceImage piece, description = "" }
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
            [ Element.inFront
                (case model.placing of
                    Just { piece, clientX, clientY } ->
                        Element.el []
                            (Element.image
                                [ Element.width <| Element.px squareSide
                                , Element.height <| Element.px squareSide
                                , Element.moveRight clientX
                                , Element.moveDown clientY
                                ]
                                { src = ExternalUrls.pieceImage piece, description = "" }
                            )

                    Nothing ->
                        Element.none
                )
            , Element.width Element.fill
            , Element.height Element.fill
            , Element.htmlAttribute <| Html.Attributes.style "user-select" "none"
            ]



-- SUBSCRIPTIONS


subscriptions : Model -> Sub.Sub Msg
subscriptions model =
    if model.placing /= Nothing then
        Browser.Events.onMouseMove positionDecode

    else
        Sub.none


positionDecode : Decode.Decoder Msg
positionDecode =
    Decode.map2 MouseMoved
        (Decode.field "clientX" Decode.float)
        (Decode.field "clientY" Decode.float)
