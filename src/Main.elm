module Main exposing (..)

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


type alias Square =
    { color : Color }


type alias Row =
    { squares : List Square }


type alias Board =
    { rows : List Row }


flipColor : Square -> Square
flipColor square =
    case square.color of
        White ->
            { color = Black }

        Black ->
            { color = White }


flipRow : Row -> Row
flipRow row =
    { squares = List.map flipColor row.squares }


rowBuilder : Int -> Square -> List Square
rowBuilder size nextSquare =
    if size <= 0 then
        []
    else
        nextSquare :: rowBuilder (size - 1) (flipColor nextSquare)


whiteRow =
    { squares = rowBuilder 8 { color = White } }


board =
    { rows = boardBuilder 8 whiteRow }


boardBuilder : Int -> Row -> List Row
boardBuilder size nextRow =
    if size <= 0 then
        []
    else
        nextRow :: boardBuilder (size - 1) (flipRow nextRow)


squareHtml : Square -> Html msg
squareHtml square =
    case square.color of
        White ->
            div [ class "square white" ] []

        Black ->
            div [ class "square black" ] []


rowHtml : Row -> Html msg
rowHtml row =
    div [ class "row" ] (List.map squareHtml row.squares)


boardHtml : Board -> Html msg
boardHtml board =
    div [] (List.map rowHtml board.rows)



---- MODEL ----


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



---- UPDATE ----


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "Get ready to chess!" ]
        , boardHtml board
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
