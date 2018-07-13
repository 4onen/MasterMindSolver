module MMPuzzleView exposing (view)

import Html exposing (Html)
import Html.Attributes
import Html.Events
import List.Extra

import MMPuzzle exposing (..)

view : PuzzleData -> Html Msg
view model =
    Html.div []
        [ viewClueTable model
        , Html.button 
            [ Html.Events.onClick AddClue ]
            [ Html.text "Add Clue" ]
        , viewSolutionTable model
        ]

-- ClueTable --

viewClueTable : PuzzleData -> Html Msg
viewClueTable model =
    model.clues
        |> List.indexedMap (viewClue model.numColors model.numPegs)
        |> Html.table []

viewClue : Int -> Int -> Int -> ClueData -> Html Msg
viewClue numColors numPegs i (g,h,d) =
    g
        |> List.indexedMap (viewPeg numColors (h<0 && d<0) i)
        |> (Basics.flip List.append)
            ( if (h<0 && d<0) then
                [Html.td [] [], Html.td [] []]
              else
                (List.map (List.singleton >> (Html.td [])) 
                    [viewHearts numPegs i h, viewDots numPegs i d]
                )
            )
        |> Html.tr []

viewPeg : Int -> Bool -> Int -> Int -> Maybe Peg -> Html Msg
viewPeg numColors disable i j mc =
    Html.td [] [
        Html.input
            [ Html.Attributes.disabled disable
            , Html.Attributes.type_ "number"
            , Html.Attributes.min "1"
            , Html.Attributes.max <| toString numColors
            , Html.Attributes.maxlength 1
            , Html.Attributes.value 
                (case mc of
                    Just c ->
                        toString c
                    Nothing ->
                        ""
                )
            , Html.Events.onInput 
                ( String.toInt 
                    >> Result.withDefault 0 
                    >> SetCol i j
                )
            ] []
    ]

viewDots : Int -> Int -> Int -> Html Msg
viewDots numPegs i d =
    viewReqs numPegs i False d

viewHearts : Int -> Int -> Int -> Html Msg
viewHearts numPegs i d =
    viewReqs numPegs i True d

viewReqs : Int -> Int -> Bool -> Int -> Html Msg
viewReqs numPegs i heart d =
    Html.input
            [ Html.Attributes.type_ "number"
            , Html.Attributes.min "0"
            , Html.Attributes.max (toString numPegs)
            , Html.Attributes.maxlength 1
            , Html.Attributes.value <| toString d
            , Html.Events.onInput 
                ( String.toInt 
                    >> Result.withDefault 0 
                    >> (if heart then SetHearts else SetDots) i
                )
            , Html.Attributes.style
                [ ("border-left","4px solid black")]
            ] []

-- Solution Table --

viewSolutionTable : PuzzleData -> Html msg
viewSolutionTable model =
    let
        possibilities = model.numColors
            |> List.range 1
            |> List.repeat model.numPegs
            |> List.Extra.cartesianProduct
        filteredPossibilities =
            List.foldl applyClueToSpace possibilities model.clues
        cnt = List.length filteredPossibilities
    in
        if cnt < 1 || cnt > 32 then
            Html.div [] 
                [ Html.text 
                    ("There are "
                        ++(toString <| List.length filteredPossibilities)
                        ++" possibilities."
                    )
                ]
        else
            filteredPossibilities
                |> List.map (toString>>Html.text>>(\p->Html.tr [] [Html.td [] [p]]))
                |> Html.table []

applyClueToSpace : ClueData -> List (List Peg) -> List (List Peg)
applyClueToSpace clue =
    List.filter (applyClue clue)

applyClue : ClueData -> List Peg -> Bool
applyClue (cs,h,d) guess =
    let
        zip = List.Extra.zip cs guess
        hs = zip 
            |> List.map (\(a,b)->(a==Just b)||(a==Nothing))
        notHeartZip = zip
            |> List.map2 (\h c->if h then Nothing else Just c) hs
            |> List.filterMap identity
        (notHeartClues,notHeartGuess) = List.unzip notHeartZip
        notEither = List.foldl findDot notHeartClues notHeartGuess
        numDots = (List.length notHeartZip) - (List.length notEither)
    in
        (  List.Extra.count identity hs == h 
        && numDots == d
        )

findDot : Peg -> List (Maybe Peg) -> List (Maybe Peg)
findDot col clues =
    if List.member (Just col) clues then
        List.Extra.remove (Just col) clues
    else if List.member Nothing clues then
        List.Extra.remove (Nothing) clues
    else
        clues