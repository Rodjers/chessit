module Board exposing (..)

import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (src, height, width, class)
import List exposing (length)


type Color
    = Black
    | White


type PieceType
    = Pawn
    | Bishop
    | Knight
    | Rook
    | Queen
    | King


type alias Piece =
    { color : Color, variant : PieceType, row : Int, col : Int }


pieceHtml : Piece -> Html Msg
pieceHtml piece =
    case piece.variant of



getPosition : Int -> List (String, String)
getPosition pos =
    pos * 100
