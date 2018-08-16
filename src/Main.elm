module Main exposing (..)

import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (src, height, width, class, style, alt)
import Board exposing (boardHtml, board, Board)
import Pieces exposing (pieces, pieceHtml, Piece)


---- MODEL ----


type alias Model =
    { board : Board }


init : ( Model, Cmd Msg )
init =
    ( { board = board
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | PieceLifted Piece


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
            (List.append (List.map pieceHtml model.board.pieces) (List.singleton (boardHtml board)))
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
