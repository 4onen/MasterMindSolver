module Main exposing (Model, Msg, update, view, subscriptions, init)

import Html exposing (..)
import Html.Events

import MMPuzzle
import MMPuzzleView

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
    }


type alias Model =
    { puzzle : Maybe MMPuzzle.PuzzleData
    }


type Msg
    = PuzzleSolver MMPuzzle.Msg
    | StartPuzzle Int Int
    | EndPuzzle


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        PuzzleSolver mmMsg ->
            { model | puzzle = Maybe.map (MMPuzzle.update mmMsg) model.puzzle } ! []
        StartPuzzle pegs colors ->
            { model | puzzle = Maybe.Just <| MMPuzzle.init pegs colors } ! []
        EndPuzzle ->
            { model | puzzle = Maybe.Nothing } ! []


view : Model -> Html Msg
view model =
    case model.puzzle of
        Just puzzle ->
            Html.div [] 
                [ Html.button [Html.Events.onClick EndPuzzle] [Html.text "End Puzzle"]
                , Html.map PuzzleSolver <| MMPuzzleView.view puzzle
                ]
        Nothing ->
            viewStartup

viewStartup : Html Msg
viewStartup =
    List.range 3 5
        |> List.repeat 4
        |> List.indexedMap (\colors listPegs -> List.map (\pegs -> (pegs,colors+3)) listPegs)
        |> List.map (List.map viewPuzzleButton)
        |> List.map (Html.tr [])
        |> Html.table []

viewPuzzleButton : (Int,Int) -> Html Msg
viewPuzzleButton (pegs,colors) = 
    Html.button
        [ Html.Events.onClick (StartPuzzle pegs colors)
        ]
        [ (pegs,colors) 
            |> toString 
            |> Html.text
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


init : (Model, Cmd Msg)
init = 
    Model Maybe.Nothing ! []
