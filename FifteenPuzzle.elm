module FifteenPuzzle exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import String
import List exposing (..)
import Random exposing (..)


main : Program Never FifteenBoard Msg
main =
    Html.program
        { init = init
        , view = myView
        , update = myUpdate
        , subscriptions = subscriptions
        }



--VIEW


myView : FifteenBoard -> Html Msg
myView model =
    div
        []
        [ div [ class "puzzle-container" ]
            [ div [ class "placeholders" ]
                (List.map tilePlaceholderHtml listOfPlaces)
            , div [ class "tiles" ]
                (List.map tileHtml model.tiles)
            , coverContainer model
            ]
        , buttonContainer
        ]


tilePlaceholderHtml : Int -> Html Msg
tilePlaceholderHtml n =
    div [ class ("placeholder placeholder-" ++ (toString n)) ] []


tileHtml : Tile -> Html Msg
tileHtml tile =
    if (tile.tileNumber /= 0) then
        div
            [ class (tileClassName tile)
            , onClick (Click tile.tilePlace)
            ]
            [ div [ class "puzzle-item-inner" ] [ text (tileInnerText tile.tileNumber) ]
            ]
    else
        (text "")


tileClassName : Tile -> String
tileClassName tile =
    String.join " " [ "puzzle-item", "tile-" ++ (toString tile.tileNumber), "place-" ++ (toString tile.tilePlace) ]


tileInnerText : Int -> String
tileInnerText n =
    if n == 0 then
        ""
    else
        (toString n)


buttonContainer : Html Msg
buttonContainer =
    div [ class "btn-container" ]
        [ button [ class "btn", onClick Rnd ] [ text "Start New Game" ]
        ]


coverContainer : FifteenBoard -> Html Msg
coverContainer model =
    div
        [ class (coverClassName model) ]
        [ div [ class "shadow" ] []
        , div [ class "text" ] [ text "Completed!" ]
        ]


coverClassName : FifteenBoard -> String
coverClassName model =
    case model.completed of
        Completed ->
            "cover completed"

        _ ->
            "cover"



-- SUBSCRIPTIONS


subscriptions : FifteenBoard -> Sub Msg
subscriptions model =
    Sub.none



--UPDATE


type Msg
    = Click Int
    | Rnd
    | Shuffle (List Float)


myUpdate : Msg -> FifteenBoard -> ( FifteenBoard, Cmd Msg )
myUpdate msg model =
    case msg of
        Click n ->
            ( checkPuzzle (moveTileIfPossible model n), Cmd.none )

        Rnd ->
            ( model, generateRandomList )

        Shuffle randomList ->
            ( checkPuzzle (shuffleModel randomList model), Cmd.none )


generateRandomList : Cmd Msg
generateRandomList =
    Random.generate Shuffle (Random.list 15 (float 0 1))


shuffleModel : List Float -> FifteenBoard -> FifteenBoard
shuffleModel randomList model =
    let
        listOfTilesTuples =
            List.map2 (,) listOfTiles randomList

        sortedListOfTilesTuples =
            List.sortBy Tuple.second listOfTilesTuples

        listOfShuffledTilesPlaces =
            Tuple.first (List.unzip sortedListOfTilesTuples)
    in
        { model
            | tiles = List.indexedMap buildTile listOfShuffledTilesPlaces
            , emptyPlace = initialEmptyPlace
        }


checkTilePlace : Tile -> State -> State
checkTilePlace tile correctPlace =
    case correctPlace of
        Completed ->
            if (tile.tileNumber == tile.tilePlace) then
                Completed
            else
                Incompleted

        _ ->
            Incompleted


checkPuzzle : FifteenBoard -> FifteenBoard
checkPuzzle model =
    let
        completed =
            List.foldl checkTilePlace Completed model.tiles
    in
        { model | completed = completed }


getRow : Int -> Int
getRow place =
    if (place % boardSize /= 0) then
        place // boardSize + 1
    else
        place // boardSize


getColumn : Int -> Int
getColumn place =
    if (place % boardSize == 0) then
        boardSize
    else
        place % boardSize


moveTileIfPossible : FifteenBoard -> Int -> FifteenBoard
moveTileIfPossible model n =
    let
        emptyRow =
            getRow model.emptyPlace

        emptyColumn =
            getColumn model.emptyPlace

        clickedRow =
            getRow n

        clickedColumn =
            getColumn n
    in
        if (clickedRow == emptyRow) then
            moveTilesHorizontally model n
        else if (clickedColumn == emptyColumn) then
            moveTilesVertically model n
        else
            model


moveTilesHorizontally : FifteenBoard -> Int -> FifteenBoard
moveTilesHorizontally model n =
    if (n == model.emptyPlace) then
        model
    else
        -- move empty tile
        let
            nearTile =
                if (n < model.emptyPlace) then
                    model.emptyPlace - 1
                else
                    model.emptyPlace + 1

            updateTiles t =
                if t.tilePlace == nearTile then
                    { t | tilePlace = model.emptyPlace }
                else
                    t

            newModel =
                { model
                    | emptyPlace = nearTile
                    , tiles = List.map updateTiles model.tiles
                }
        in
            moveTilesHorizontally newModel n


moveTilesVertically : FifteenBoard -> Int -> FifteenBoard
moveTilesVertically model n =
    if (n == model.emptyPlace) then
        model
    else
        -- move empty tile
        let
            nearTile =
                if (n < model.emptyPlace) then
                    model.emptyPlace - boardSize
                else
                    model.emptyPlace + boardSize

            updateTiles t =
                if t.tilePlace == nearTile then
                    { t | tilePlace = model.emptyPlace }
                else
                    t

            newModel =
                { model
                    | emptyPlace = nearTile
                    , tiles = List.map updateTiles model.tiles
                }
        in
            moveTilesVertically newModel n



--MODEL


boardSize : Int
boardSize =
    4


initialEmptyPlace : Int
initialEmptyPlace =
    boardSize ^ 2


listOfPlaces : List Int
listOfPlaces =
    range 1 (boardSize ^ 2)


listOfTiles : List Int
listOfTiles =
    range 1 (boardSize ^ 2 - 1)


type alias Tile =
    { tileNumber : Int
    , tilePlace : Int
    }


buildTile : Int -> Int -> Tile
buildTile index element =
    { tileNumber = index + 1, tilePlace = element }


tilesList : List Tile
tilesList =
    List.indexedMap buildTile listOfTiles


type State
    = Start
    | Completed
    | Incompleted


type alias FifteenBoard =
    { emptyPlace : Int
    , completed : State
    , tiles : List Tile
    }


myModel : FifteenBoard
myModel =
    { emptyPlace = initialEmptyPlace
    , completed = Start
    , tiles = tilesList
    }


init : ( FifteenBoard, Cmd Msg )
init =
    ( myModel, generateRandomList )
