module Pieces exposing (..)

import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (src, height, width, class, style)


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


pieceHtml : Piece -> Html msg
pieceHtml piece =
    case piece.color of
        White ->
            img
                [ class "piece"
                , style (getPieceStyle (rotatePiece piece))
                , src (whitePieceImage piece)
                ]
                []

        Black ->
            img
                [ class "piece"
                , style (getPieceStyle (rotatePiece piece))
                , src (blackPieceImage piece)
                ]
                []


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
