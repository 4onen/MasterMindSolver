module MMPuzzle exposing (..)

import List.Extra

type alias Peg = Int

type alias Guess = List (Maybe Peg)

type alias ClueData = (Guess,Int,Int)

type alias PuzzleData =
    { clues : List ClueData
    , numColors : Int
    , numPegs : Int
    }

blankGuess : Int -> Guess
blankGuess numPegs = List.repeat numPegs Maybe.Nothing

blankClue : Int -> ClueData
blankClue numPegs = (blankGuess numPegs,0,0)

init : Int -> Int -> PuzzleData
init numPegs numColors = 
    { clues = numPegs 
        |> blankClue
        |> List.singleton
    , numColors = numColors
    , numPegs = numPegs
    }

type Msg 
    = AddClue
    --| RemoveClue Int
    | SetCol Int Int Int
    | SetDots Int Int
    | SetHearts Int Int

update : Msg -> PuzzleData -> PuzzleData
update msg model =
    case msg of
        AddClue ->
            { model | clues = List.append [model.numPegs |> blankClue] model.clues}
        SetCol i j c ->
            { model | clues = List.Extra.updateAt i (updateCluePeg model.numColors j c) model.clues }
        SetDots i n ->
            { model | clues = List.Extra.updateAt i (\(g,h,_)->(g,h,n)) model.clues }
        SetHearts i n ->
            { model | clues = List.Extra.updateAt i (\(g,_,d)->(g,n,d)) model.clues }

updateCluePeg : Int -> Int -> Int -> (Guess,Int,Int) -> (Guess,Int,Int)
updateCluePeg numColors j c (pegs,numHeart,numDot) =
    (List.Extra.updateAt j (updatePeg numColors c) pegs,numHeart,numDot)
    

updatePeg : Int -> Int -> Maybe Peg -> Maybe Peg
updatePeg numColors c _ =
    if (0<c) && (c<=numColors) then
        Maybe.Just c
    else
        Maybe.Nothing

