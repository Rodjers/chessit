module Main exposing (..)

import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (src, height, width, class, style, alt)
import Html.Events exposing (onClick)
import Matrix exposing (Matrix)


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


type alias MatrixPiece =
    { color : PieceColor
    , variant : PieceType
    , status : PieceStatus
    , id : String
    }


initialPieces : Matrix (Maybe MatrixPiece)
initialPieces =
    Matrix.fromList
        [ [ Just { id = "a1", color = White, variant = Rook, status = Placed }
          , Just { id = "b1", color = White, variant = Knight, status = Placed }
          , Just { id = "c1", color = White, variant = Bishop, status = Placed }
          , Just { id = "d1", color = White, variant = Queen, status = Placed }
          , Just { id = "e1", color = White, variant = King, status = Placed }
          , Just { id = "f1", color = White, variant = Bishop, status = Placed }
          , Just { id = "g1", color = White, variant = Knight, status = Placed }
          , Just { id = "h1", color = White, variant = Rook, status = Placed }
          ]
        , [ Just { id = "a2", color = White, variant = Pawn, status = Placed }
          , Just { id = "b2", color = White, variant = Pawn, status = Placed }
          , Just { id = "c2", color = White, variant = Pawn, status = Placed }
          , Just { id = "d2", color = White, variant = Pawn, status = Placed }
          , Just { id = "e2", color = White, variant = Pawn, status = Placed }
          , Just { id = "f2", color = White, variant = Pawn, status = Placed }
          , Just { id = "g2", color = White, variant = Pawn, status = Placed }
          , Just { id = "h2", color = White, variant = Pawn, status = Placed }
          ]
        , [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ]
        , [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ]
        , [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ]
        , [ Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing, Nothing ]
        , [ Just { id = "a7", color = Black, variant = Pawn, status = Placed }
          , Just { id = "b7", color = Black, variant = Pawn, status = Placed }
          , Just { id = "c7", color = Black, variant = Pawn, status = Placed }
          , Just { id = "d7", color = Black, variant = Pawn, status = Placed }
          , Just { id = "e7", color = Black, variant = Pawn, status = Placed }
          , Just { id = "f7", color = Black, variant = Pawn, status = Placed }
          , Just { id = "g7", color = Black, variant = Pawn, status = Placed }
          , Just { id = "h7", color = Black, variant = Pawn, status = Placed }
          ]
        , [ Just { id = "a8", color = Black, variant = Rook, status = Placed }
          , Just { id = "b8", color = Black, variant = Knight, status = Placed }
          , Just { id = "c8", color = Black, variant = Bishop, status = Placed }
          , Just { id = "d8", color = Black, variant = Queen, status = Placed }
          , Just { id = "e8", color = Black, variant = King, status = Placed }
          , Just { id = "f8", color = Black, variant = Bishop, status = Placed }
          , Just { id = "g8", color = Black, variant = Knight, status = Placed }
          , Just { id = "h8", color = Black, variant = Rook, status = Placed }
          ]
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
    [ ( "transform", "translate(" ++ (getColPosition piece.col) ++ ", " ++ (getRowPosition piece.row) ++ ")" ) ]


getRowPosition : Int -> String
getRowPosition row =
    toString ((7 - row) * 100) ++ "px"


getColPosition : Int -> String
getColPosition col =
    toString (col * 100) ++ "px"


pieceHtml : Piece -> Html Msg
pieceHtml piece =
    case piece.color of
        White ->
            img
                [ class "piece"
                , style (getPieceStyle piece)
                , src (whitePieceImage piece)
                , onClick (PieceSelected piece)
                ]
                []

        Black ->
            img
                [ class "piece"
                , style (getPieceStyle piece)
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


type alias SquareMatrix =
    Matrix MatrixSquare


type alias Square =
    { color : SquareColor
    , row : Int
    , col : Int
    }


type alias MatrixSquare =
    { color : SquareColor
    }


type alias Board =
    { squares : SquareMatrix
    , pieces : Matrix (Maybe MatrixPiece)
    }


initialBoard : Board
initialBoard =
    { squares = initialSquares
    , pieces = initialPieces
    }


getPieces : Board -> List Piece
getPieces board =
    (Matrix.flatten (Matrix.mapWithLocation toPiece board.pieces))
        |> List.filterMap (\p -> p)


getPiece : Board -> Matrix.Location -> Maybe Piece
getPiece board location =
    let
        maybeMaybeMatrixPiece =
            Matrix.get location board.pieces
    in
        case maybeMaybeMatrixPiece of
            Just maybeMatrixPiece ->
                case maybeMatrixPiece of
                    Just matrixPiece ->
                        Just
                            { status = matrixPiece.status
                            , color = matrixPiece.color
                            , id = matrixPiece.id
                            , variant = matrixPiece.variant
                            , col = Matrix.col location
                            , row = Matrix.row location
                            }

                    Nothing ->
                        Nothing

            Nothing ->
                Nothing


setPiece : Board -> Maybe Piece -> Matrix.Location -> Board
setPiece board piece location =
    case piece of
        Just piece ->
            { board
                | pieces =
                    Matrix.set location (Just { color = piece.color, status = piece.status, variant = piece.variant, id = piece.id }) board.pieces
            }

        Nothing ->
            { board
                | pieces =
                    Matrix.set location Nothing board.pieces
            }


getSquares : Board -> List Square
getSquares board =
    Matrix.flatten (Matrix.mapWithLocation toSquare board.squares)


toSquare : Matrix.Location -> MatrixSquare -> Square
toSquare location matrixSquare =
    { color = matrixSquare.color
    , col = Matrix.col location
    , row = Matrix.row location
    }


toSquareMatrix : List Square -> Matrix MatrixSquare
toSquareMatrix inSquares =
    List.foldl (\s m -> Matrix.set (Matrix.loc s.col s.row) { color = s.color } m) initialSquares inSquares


toPiece : Matrix.Location -> Maybe MatrixPiece -> Maybe Piece
toPiece location matrixPiece =
    case matrixPiece of
        Just piece ->
            Just
                { color = piece.color
                , variant = piece.variant
                , status = piece.status
                , id = piece.id
                , col = Matrix.col location
                , row = Matrix.row location
                }

        Nothing ->
            Nothing


toPieceMatrix : List Piece -> Matrix (Maybe MatrixPiece)
toPieceMatrix inPieces =
    let
        initialMatrix =
            Matrix.matrix 8 8 (\n -> Nothing)
    in
        List.foldl (\p m -> Matrix.set (Matrix.loc p.col p.row) (Just { color = p.color, variant = p.variant, status = p.status, id = p.id }) m) initialPieces inPieces


initialSquares : Matrix MatrixSquare
initialSquares =
    Matrix.fromList
        [ [ { color = Light }
          , { color = Dark }
          , { color = Light }
          , { color = Dark }
          , { color = Light }
          , { color = Dark }
          , { color = Light }
          , { color = Dark }
          ]
        , [ { color = Dark }
          , { color = Light }
          , { color = Dark }
          , { color = Light }
          , { color = Dark }
          , { color = Light }
          , { color = Dark }
          , { color = Light }
          ]
        , [ { color = Light }
          , { color = Dark }
          , { color = Light }
          , { color = Dark }
          , { color = Light }
          , { color = Dark }
          , { color = Light }
          , { color = Dark }
          ]
        , [ { color = Dark }
          , { color = Light }
          , { color = Dark }
          , { color = Light }
          , { color = Dark }
          , { color = Light }
          , { color = Dark }
          , { color = Light }
          ]
        , [ { color = Light }
          , { color = Dark }
          , { color = Light }
          , { color = Dark }
          , { color = Light }
          , { color = Dark }
          , { color = Light }
          , { color = Dark }
          ]
        , [ { color = Dark }
          , { color = Light }
          , { color = Dark }
          , { color = Light }
          , { color = Dark }
          , { color = Light }
          , { color = Dark }
          , { color = Light }
          ]
        , [ { color = Light }
          , { color = Dark }
          , { color = Light }
          , { color = Dark }
          , { color = Light }
          , { color = Dark }
          , { color = Light }
          , { color = Dark }
          ]
        , [ { color = Dark }
          , { color = Light }
          , { color = Dark }
          , { color = Light }
          , { color = Dark }
          , { color = Light }
          , { color = Dark }
          , { color = Light }
          ]
        ]


squareHtml : Square -> Html Msg
squareHtml square =
    case square.color of
        Light ->
            div [ class "square light", style (getSquareStyle square), onClick PieceUnselected ] []

        Dark ->
            div [ class "square dark", style (getSquareStyle square), onClick PieceUnselected ] []

        Orange ->
            div [ class "square orange", style (getSquareStyle square), onClick PieceUnselected ] []

        Red ->
            div [ class "square red", style (getSquareStyle square), (onClick (PieceCaptured square)) ] []

        Green ->
            div [ class "square green", style (getSquareStyle square), (onClick (PiecePlaced square)) ] []


getSquareStyle : Square -> List ( String, String )
getSquareStyle square =
    [ ( "top", Basics.toString ((8 - square.row) * 100) ++ "px" )
    , ( "left", Basics.toString ((square.col) * 100) ++ "px" )
    ]


boardHtml : Board -> Html Msg
boardHtml board =
    div [] (List.map squareHtml (getSquares board))


markSquares : List Move -> SquareMatrix -> SquareMatrix
markSquares moves squares =
    List.foldl (\m s -> markSquare (getTargetSquare m) s Green) squares moves


markSquare : Matrix.Location -> SquareMatrix -> SquareColor -> SquareMatrix
markSquare location squares color =
    Matrix.set location { color = color } squares


getTargetSquare : Move -> Matrix.Location
getTargetSquare move =
    ( move.piece.row + Tuple.second move.distance, move.piece.col + Tuple.first move.distance )


squareIsTargetOfMove : Square -> Move -> Bool
squareIsTargetOfMove square move =
    square.row == (move.piece.row + Tuple.second move.distance) && square.col == (move.piece.col + Tuple.first move.distance)


validMoves : Piece -> Board -> List Move
validMoves piece board =
    case piece.variant of
        Pawn ->
            pawnMoves board piece

        _ ->
            []


pawnMoves : Board -> Piece -> List Move
pawnMoves board pawn =
    pawnMove board pawn
        |> List.append (pawnCapture initialBoard pawn)


pawnMove : Board -> Piece -> List Move
pawnMove board pawn =
    case pawn.color of
        White ->
            if isBlocked board pawn ( 0, 1 ) then
                []
            else if pawn.row == 1 && not (isBlocked board pawn ( 0, 1 )) then
                [ { piece = pawn, distance = ( 0, 1 ) }, { piece = pawn, distance = ( 0, 2 ) } ]
            else
                [ { piece = pawn, distance = ( 0, 1 ) } ]

        Black ->
            if isBlocked board pawn ( 0, -1 ) then
                []
            else if pawn.row == 6 && not (isBlocked board pawn ( 0, -1 )) then
                [ { piece = pawn, distance = ( 0, -1 ) }, { piece = pawn, distance = ( 0, -2 ) } ]
            else
                [ { piece = pawn, distance = ( 0, -1 ) } ]


pawnCapture : Board -> Piece -> List Move
pawnCapture board pawn =
    case pawn.color of
        White ->
            case getPiece board ( (pawn.col - 1), (pawn.row + 1) ) of
                Just piece ->
                    [ { piece = piece, distance = ( 0, 1 ) } ]

                Nothing ->
                    []

        Black ->
            case getPiece board ( (pawn.col - 1), (pawn.row - 1) ) of
                Just piece ->
                    [ { piece = piece, distance = ( 0, -1 ) } ]

                Nothing ->
                    []


isBlocked : Board -> Piece -> ( Int, Int ) -> Bool
isBlocked board piece distance =
    case getPiece board (Matrix.loc (piece.row + (Tuple.second distance)) (piece.col + (Tuple.first distance))) of
        Just piece ->
            True

        Nothing ->
            False



---- MODEL ----


type alias Model =
    { board : Board, selectedPiece : Maybe Piece, gameState : GameState }


init : ( Model, Cmd Msg )
init =
    ( { board = initialBoard
      , selectedPiece = Nothing
      , gameState = Ready
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


selectSquare : ( Int, Int ) -> SquareMatrix -> SquareMatrix
selectSquare coordinates squares =
    squares


resetSquare : Square -> Square
resetSquare square =
    if (square.row + square.col) % 2 == 0 then
        { square | color = Dark }
    else
        { square | color = Light }


performMove : Move -> Board -> Board
performMove move board =
    let
        newBoard =
            setPiece (setPiece board (Just move.piece) (getTargetSquare move)) Nothing (Matrix.loc move.piece.row move.piece.col)
    in
        { newBoard
            | squares = initialSquares
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        modelBoard =
            model.board
    in
        case msg of
            NoOp ->
                ( model, Cmd.none )

            PieceSelected piece ->
                ( { model
                    | board =
                        { modelBoard
                            | squares =
                                markSquares (validMoves piece model.board) (markSquare ( piece.row, piece.col ) initialSquares Orange)
                        }
                    , selectedPiece = Just piece
                  }
                , Cmd.none
                )

            PiecePlaced square ->
                case model.selectedPiece of
                    Just piece ->
                        ( { model
                            | board = performMove { piece = piece, distance = ( square.col - piece.col, square.row - piece.row ) } model.board
                            , selectedPiece = Nothing
                          }
                        , Cmd.none
                        )

                    Nothing ->
                        ( model, Cmd.none )

            PieceCaptured square ->
                ( model, Cmd.none )

            PieceUnselected ->
                ( { model
                    | board =
                        { modelBoard
                            | squares = initialSquares
                        }
                    , selectedPiece = Nothing
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
            (List.append (List.map pieceHtml (getPieces model.board)) (List.singleton (boardHtml model.board)))
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
