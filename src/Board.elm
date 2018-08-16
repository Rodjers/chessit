module Board exposing (..)

import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (src, height, width, class)
import List exposing (length)
import Pieces exposing (Piece, pieces)


type SquareColor
    = Black
    | White
    | Orange
    | Red


type alias Square =
    SquareColor


type alias Row =
    List Square


type alias Board =
    { squares : List Row
    , pieces : List Piece
    }


flipColor : Square -> Square
flipColor square =
    case square of
        White ->
            Black

        Black ->
            White

        _ ->
            square


flipRow : Row -> Row
flipRow row =
    List.map flipColor row


rowBuilder : Int -> Square -> Row
rowBuilder size nextSquare =
    if size <= 0 then
        []
    else
        nextSquare :: rowBuilder (size - 1) (flipColor nextSquare)


whiteRow : Row
whiteRow =
    rowBuilder 8 White


board : Board
board =
    { squares = squareBuilder 8 whiteRow
    , pieces = pieces
    }


squareBuilder : Int -> Row -> List Row
squareBuilder size nextRow =
    if size <= 0 then
        []
    else
        nextRow :: squareBuilder (size - 1) (flipRow nextRow)


squareHtml : Square -> Html msg
squareHtml square =
    case square of
        White ->
            div [ class "square white" ] []

        Black ->
            div [ class "square black" ] []

        Orange ->
            div [ class "square orange" ] []

        Red ->
            div [ class "square red" ] []


rowHtml : Row -> Html msg
rowHtml row =
    div [ class "row" ] (List.map squareHtml row)


boardHtml : Board -> Html msg
boardHtml board =
    div [] (List.map rowHtml board.squares)
