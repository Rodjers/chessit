module Main exposing (..)

import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (src, height, width, class, style)
import Board exposing (boardHtml, board)


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
        _ ->
            img
                [ class "piece"
                , style (getPieceStyle piece)
                , src "pieces/Chess_plt45.svg"
                ]
                []


getPieceStyle : Piece -> List ( String, String )
getPieceStyle piece =
    [ ( "transform", "translate(" ++ getPosition piece.col ++ ", " ++ getPosition piece.row ++ ")" ) ]


getPosition : Int -> String
getPosition int =
    toString ((int - 1) * 100) ++ "px"



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
        , div
            [ style [ ( "display", "inline-block" ) ] ]
            [ pieceHtml { color = White, variant = Pawn, row = 1, col = 1 }
            , pieceHtml { color = Black, variant = Pawn, row = 2, col = 1 }
            , boardHtml board
            ]
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
