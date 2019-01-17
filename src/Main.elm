module Main exposing (..)

import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (src, height, width, class, style, alt)
import Html.Events exposing (onClick)


---- PIECES ----


type PieceColor
    = Black
    | White


type PieceType
    = Pawn
    | Bishop
    | Knight
    | Rook
    | Queen
    | King


type PieceStatus
    = Placed
    | Hovering


type GameState
    = Ready
    | InFlight


type alias Move =
    { piece : Piece
    , distance : ( Int, Int )
    }


type alias Piece =
    { color : PieceColor
    , variant : PieceType
    , row : Int
    , col : Int
    , status : PieceStatus
    , id : String
    }


pieces : List Piece
pieces =
    [ { id = "a1", color = White, variant = Rook, row = 1, col = 1, status = Placed }
    , { id = "b1", color = White, variant = Knight, row = 1, col = 2, status = Placed }
    , { id = "c1", color = White, variant = Bishop, row = 1, col = 3, status = Placed }
    , { id = "d1", color = White, variant = Queen, row = 1, col = 4, status = Placed }
    , { id = "e1", color = White, variant = King, row = 1, col = 5, status = Placed }
    , { id = "f1", color = White, variant = Bishop, row = 1, col = 6, status = Placed }
    , { id = "g1", color = White, variant = Knight, row = 1, col = 7, status = Placed }
    , { id = "h1", color = White, variant = Rook, row = 1, col = 8, status = Placed }
    , { id = "a2", color = White, variant = Pawn, row = 2, col = 1, status = Placed }
    , { id = "b2", color = White, variant = Pawn, row = 2, col = 2, status = Placed }
    , { id = "c2", color = White, variant = Pawn, row = 2, col = 3, status = Placed }
    , { id = "d2", color = White, variant = Pawn, row = 2, col = 4, status = Placed }
    , { id = "e2", color = White, variant = Pawn, row = 2, col = 5, status = Placed }
    , { id = "f2", color = White, variant = Pawn, row = 2, col = 6, status = Placed }
    , { id = "g2", color = White, variant = Pawn, row = 2, col = 7, status = Placed }
    , { id = "h2", color = White, variant = Pawn, row = 2, col = 8, status = Placed }
    , { id = "a7", color = Black, variant = Pawn, row = 7, col = 1, status = Placed }
    , { id = "b7", color = Black, variant = Pawn, row = 7, col = 2, status = Placed }
    , { id = "c7", color = Black, variant = Pawn, row = 7, col = 3, status = Placed }
    , { id = "d7", color = Black, variant = Pawn, row = 7, col = 4, status = Placed }
    , { id = "e7", color = Black, variant = Pawn, row = 7, col = 5, status = Placed }
    , { id = "f7", color = Black, variant = Pawn, row = 7, col = 6, status = Placed }
    , { id = "g7", color = Black, variant = Pawn, row = 7, col = 7, status = Placed }
    , { id = "h7", color = Black, variant = Pawn, row = 7, col = 8, status = Placed }
    , { id = "a8", color = Black, variant = Rook, row = 8, col = 1, status = Placed }
    , { id = "b8", color = Black, variant = Knight, row = 8, col = 2, status = Placed }
    , { id = "c8", color = Black, variant = Bishop, row = 8, col = 3, status = Placed }
    , { id = "d8", color = Black, variant = Queen, row = 8, col = 4, status = Placed }
    , { id = "e8", color = Black, variant = King, row = 8, col = 5, status = Placed }
    , { id = "f8", color = Black, variant = Bishop, row = 8, col = 6, status = Placed }
    , { id = "g8", color = Black, variant = Knight, row = 8, col = 7, status = Placed }
    , { id = "h8", color = Black, variant = Rook, row = 8, col = 8, status = Placed }
    ]


blackPieceImage : Piece -> String
blackPieceImage piece =
    case piece.variant of
        Pawn ->
            "pieces/Chess_pdt45.svg"

        Bishop ->
            "pieces/Chess_bdt45.svg"

        Knight ->
            "pieces/Chess_ndt45.svg"

        Rook ->
            "pieces/Chess_rdt45.svg"

        Queen ->
            "pieces/Chess_qdt45.svg"

        King ->
            "pieces/Chess_kdt45.svg"


whitePieceImage : Piece -> String
whitePieceImage piece =
    case piece.variant of
        Pawn ->
            "pieces/Chess_plt45.svg"

        Bishop ->
            "pieces/Chess_blt45.svg"

        Knight ->
            "pieces/Chess_nlt45.svg"

        Rook ->
            "pieces/Chess_rlt45.svg"

        Queen ->
            "pieces/Chess_qlt45.svg"

        King ->
            "pieces/Chess_klt45.svg"


rotatePiece : Piece -> Piece
rotatePiece piece =
    { piece | row = piece.col, col = 9 - piece.row }


getPieceStyle : Piece -> List ( String, String )
getPieceStyle piece =
    [ ( "transform", "translate(" ++ getPosition piece.row ++ ", " ++ getPosition piece.col ++ ")" ) ]


getPosition : Int -> String
getPosition float =
    toString ((float - 1) * 100) ++ "px"


pieceHtml : Piece -> Html Msg
pieceHtml piece =
    case piece.color of
        White ->
            img
                [ class "piece"
                , style (getPieceStyle (rotatePiece piece))
                , src (whitePieceImage piece)
                , onClick (PieceSelected piece)
                ]
                []

        Black ->
            img
                [ class "piece"
                , style (getPieceStyle (rotatePiece piece))
                , src (blackPieceImage piece)
                , onClick (PieceSelected piece)
                ]
                []



---- BOARD ----


type SquareColor
    = Dark
    | Light
    | Orange
    | Red
    | Green


type alias Square =
    { color : SquareColor
    , row : Int
    , col : Int
    }


type alias Row =
    List Square


type alias Board =
    { squares : List Row
    , pieces : List Piece
    }


board : Board
board =
    { squares = squares
    , pieces = pieces
    }


squares : List (List Square)
squares =
    [ [ { color = Light, row = 8, col = 1 }
      , { color = Dark, row = 8, col = 2 }
      , { color = Light, row = 8, col = 3 }
      , { color = Dark, row = 8, col = 4 }
      , { color = Light, row = 8, col = 5 }
      , { color = Dark, row = 8, col = 6 }
      , { color = Light, row = 8, col = 7 }
      , { color = Dark, row = 8, col = 8 }
      ]
    , [ { color = Dark, row = 7, col = 1 }
      , { color = Light, row = 7, col = 2 }
      , { color = Dark, row = 7, col = 3 }
      , { color = Light, row = 7, col = 4 }
      , { color = Dark, row = 7, col = 5 }
      , { color = Light, row = 7, col = 6 }
      , { color = Dark, row = 7, col = 7 }
      , { color = Light, row = 7, col = 8 }
      ]
    , [ { color = Light, row = 6, col = 1 }
      , { color = Dark, row = 6, col = 2 }
      , { color = Light, row = 6, col = 3 }
      , { color = Dark, row = 6, col = 4 }
      , { color = Light, row = 6, col = 5 }
      , { color = Dark, row = 6, col = 6 }
      , { color = Light, row = 6, col = 7 }
      , { color = Dark, row = 6, col = 8 }
      ]
    , [ { color = Dark, row = 5, col = 1 }
      , { color = Light, row = 5, col = 2 }
      , { color = Dark, row = 5, col = 3 }
      , { color = Light, row = 5, col = 4 }
      , { color = Dark, row = 5, col = 5 }
      , { color = Light, row = 5, col = 6 }
      , { color = Dark, row = 5, col = 7 }
      , { color = Light, row = 5, col = 8 }
      ]
    , [ { color = Light, row = 4, col = 1 }
      , { color = Dark, row = 4, col = 2 }
      , { color = Light, row = 4, col = 3 }
      , { color = Dark, row = 4, col = 4 }
      , { color = Light, row = 4, col = 5 }
      , { color = Dark, row = 4, col = 6 }
      , { color = Light, row = 4, col = 7 }
      , { color = Dark, row = 4, col = 8 }
      ]
    , [ { color = Dark, row = 3, col = 1 }
      , { color = Light, row = 3, col = 2 }
      , { color = Dark, row = 3, col = 3 }
      , { color = Light, row = 3, col = 4 }
      , { color = Dark, row = 3, col = 5 }
      , { color = Light, row = 3, col = 6 }
      , { color = Dark, row = 3, col = 7 }
      , { color = Light, row = 3, col = 8 }
      ]
    , [ { color = Light, row = 2, col = 1 }
      , { color = Dark, row = 2, col = 2 }
      , { color = Light, row = 2, col = 3 }
      , { color = Dark, row = 2, col = 4 }
      , { color = Light, row = 2, col = 5 }
      , { color = Dark, row = 2, col = 6 }
      , { color = Light, row = 2, col = 7 }
      , { color = Dark, row = 2, col = 8 }
      ]
    , [ { color = Dark, row = 1, col = 1 }
      , { color = Light, row = 1, col = 2 }
      , { color = Dark, row = 1, col = 3 }
      , { color = Light, row = 1, col = 4 }
      , { color = Dark, row = 1, col = 5 }
      , { color = Light, row = 1, col = 6 }
      , { color = Dark, row = 1, col = 7 }
      , { color = Light, row = 1, col = 8 }
      ]
    ]


squareHtml : Square -> Html Msg
squareHtml square =
    case square.color of
        Light ->
            div [ class "square light", onClick PieceUnselected ] []

        Dark ->
            div [ class "square dark", onClick PieceUnselected ] []

        Orange ->
            div [ class "square orange", onClick PieceUnselected ] []

        Red ->
            div [ class "square red", (onClick (PieceCaptured square)) ] []

        Green ->
            div [ class "square green", (onClick (PiecePlaced square)) ] []


rowHtml : Row -> Html Msg
rowHtml row =
    div [ class "row" ] (List.map squareHtml row)


boardHtml : Board -> Html Msg
boardHtml board =
    div [] (List.map rowHtml board.squares)


markBoard : List Move -> List Row -> List Row
markBoard moves rows =
    List.map (markRow moves) rows


markRow : List Move -> Row -> Row
markRow moves squares =
    List.map (markSquare moves) squares


markSquare : List Move -> Square -> Square
markSquare moves square =
    case List.isEmpty (List.filter (squareIsTargetOfMove square) moves) of
        False ->
            { square | color = Green }

        True ->
            square


squareIsTargetOfMove : Square -> Move -> Bool
squareIsTargetOfMove square move =
    square.row == (move.piece.row + Tuple.first move.distance) && square.col == (move.piece.col + Tuple.second move.distance)


validMoves : Piece -> List Piece -> List Move
validMoves piece pieces =
    case piece.variant of
        Pawn ->
            pawnMoves pieces piece

        _ ->
            []


pawnMoves : List Piece -> Piece -> List Move
pawnMoves pieces pawn =
    pawnForward pieces pawn
        |> List.append (pawnCapture pieces pawn)


pawnForward : List Piece -> Piece -> List Move
pawnForward pieces pawn =
    if List.any (isInFrontOf pawn) pieces then
        []
    else
        case pawn.color of
            White ->
                [ { piece = pawn, distance = ( 1, 0 ) } ]

            Black ->
                [ { piece = pawn, distance = ( -1, 0 ) } ]


isInFrontOf : Piece -> Piece -> Bool
isInFrontOf firstPiece secondPiece =
    case firstPiece.color of
        White ->
            firstPiece.row + 1 == secondPiece.row

        Black ->
            firstPiece.row - 1 == secondPiece.row


pawnCapture : List Piece -> Piece -> List Move
pawnCapture pieces pawn =
    []



---- MODEL ----


type alias Model =
    { board : Board, selectedPiece : Maybe Piece, gameState : GameState, validMoves : List Move }


init : ( Model, Cmd Msg )
init =
    ( { board = board
      , selectedPiece = Nothing
      , gameState = Ready
      , validMoves = []
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | PieceSelected Piece
    | PiecePlaced Square
    | PieceCaptured Square
    | PieceUnselected


selectRow : ( Int, Int ) -> Row -> Row
selectRow coordinates squares =
    List.map (selectSquare coordinates) squares


selectSquare : ( Int, Int ) -> Square -> Square
selectSquare coordinates square =
    if Tuple.first coordinates == square.row && Tuple.second coordinates == square.col then
        { square | color = Orange }
    else if (square.row + square.col) % 2 == 0 then
        { square | color = Dark }
    else
        { square | color = Light }


resetRow : Row -> Row
resetRow row =
    List.map resetSquare row


resetSquare : Square -> Square
resetSquare square =
    if (square.row + square.col) % 2 == 0 then
        { square | color = Dark }
    else
        { square | color = Light }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        PieceSelected piece ->
            ( { model
                | board =
                    { board
                        | squares =
                            markBoard (validMoves piece model.board.pieces) (List.map (selectRow ( piece.row, piece.col )) model.board.squares)
                    }
                , selectedPiece = Just piece
                , validMoves = validMoves piece model.board.pieces
              }
            , Cmd.none
            )

        PiecePlaced square ->
            ( model, Cmd.none )

        PieceCaptured square ->
            ( model, Cmd.none )

        PieceUnselected ->
            ( { model
                | board =
                    { board
                        | squares = List.map resetRow model.board.squares
                    }
              }
            , Cmd.none
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Get ready to chess!" ]
        , div
            [ style [ ( "display", "inline-block" ) ] ]
            (List.append (List.map pieceHtml model.board.pieces) (List.singleton (boardHtml model.board)))
        ]



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
