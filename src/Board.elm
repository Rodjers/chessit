module Board exposing (..)

import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (src, height, width, class)
import List exposing (length)


type Color
    = Black
    | White


type alias Square =
    Color


type alias Row =
    List Square


type alias Board =
    List Row


flipColor : Square -> Square
flipColor square =
    case square of
        White ->
            Black

        Black ->
            White


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
    boardBuilder 8 whiteRow


boardBuilder : Int -> Row -> List Row
boardBuilder size nextRow =
    if size <= 0 then
        []
    else
        nextRow :: boardBuilder (size - 1) (flipRow nextRow)


squareHtml : Square -> Html msg
squareHtml square =
    case square of
        White ->
            div [ class "square white" ] []

        Black ->
            div [ class "square black" ] []


rowHtml : Row -> Html msg
rowHtml row =
    div [ class "row" ] (List.map squareHtml row)


boardHtml : Board -> Html msg
boardHtml board =
    div [] (List.map rowHtml board)
