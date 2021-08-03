module Main exposing (..)

import Browser
import Browser.Events exposing (onKeyDown)
import Debug exposing (toString, todo)
import Element exposing (Color, Element, alignLeft, alignRight, behindContent, centerX, centerY, column, el, fill, fillPortion, height, inFront, padding, px, rgb255, row, shrink, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onMouseDown, onMouseEnter, onMouseUp)
import Element.Font as Font
import Element.Input exposing (button)
import Html exposing (Html)
import Html.Events
import Json.Decode as Decode
import List.Extra exposing (initialize)
import Random
import Random.Extra
import Random.Set
import Set exposing (Set)
import Set.Extra
import Tuple exposing (first, second)


flip : (a -> b -> c) -> b -> a -> c
flip f b a =
    f a b


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


type KeyValue
    = Character Char
    | Control String


keyDecoder : Decode.Decoder KeyValue
keyDecoder =
    Decode.map toKeyValue (Decode.field "key" Decode.string)


toKeyValue : String -> KeyValue
toKeyValue string =
    let
        _ =
            Debug.log string
    in
    case String.uncons string of
        Just ( char, "" ) ->
            Character char

        _ ->
            Control string


type Direction
    = Up
    | Right
    | Down
    | Left


type SelectionCorner
    = Start
    | End


type alias MoveSelection =
    { selectionCorner : SelectionCorner, direction : Direction }


type Control
    = Move MoveSelection


toControl : KeyValue -> Maybe Control
toControl key =
    case key of
        Control "ArrowUp" ->
            Just (Move { selectionCorner = End, direction = Up })

        Control "ArrowDown" ->
            Just (Move { selectionCorner = End, direction = Down })

        Control "ArrowLeft" ->
            Just (Move { selectionCorner = End, direction = Left })

        Control "ArrowRight" ->
            Just (Move { selectionCorner = End, direction = Right })

        Control _ ->
            Nothing

        Character 'w' ->
            Just (Move { selectionCorner = Start, direction = Up })

        Character 's' ->
            Just (Move { selectionCorner = Start, direction = Down })

        Character 'a' ->
            Just (Move { selectionCorner = Start, direction = Left })

        Character 'd' ->
            Just (Move { selectionCorner = Start, direction = Right })

        Character _ ->
            Nothing


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onKeyDown (Decode.map (toControl >> ProcessControl) keyDecoder)
        ]


type alias Cell =
    ( Int, Int )


type alias Board =
    { size : Int
    , aliveCells : Set Cell
    }


type alias Selection =
    ( Cell, Cell )


type InteractiveSelection
    = Partial { start : Cell, lastHovered : Cell }
    | Completed Selection


type BotBehavior
    = Random


type GameType
    = VsBot BotBehavior
    | VsPlayer


type PlayerType
    = Player
    | Enemy


type alias Model =
    { board : Board
    , turnCount : Int
    , gameType : GameType
    , playerSelection : InteractiveSelection
    , enemySelection : Selection
    }


defaultBoardSize : number
defaultBoardSize =
    10


mirrorLeftHalf : Board -> Board
mirrorLeftHalf board =
    let
        rightPart =
            {--Set.map (\c -> ( board.size - 1 - first c, second c )) board.aliveCells --}
            Set.map (\c -> ( board.size - 1 - first c, board.size - 1 - second c )) board.aliveCells
    in
    { board | aliveCells = Set.union board.aliveCells rightPart }


randomCells : Random.Generator (Set Cell)
randomCells =
    Random.Set.set 16 (randomCell defaultBoardSize)


randomSymmetricBoard : Random.Generator Board
randomSymmetricBoard =
    randomCells |> Random.map (\cells -> mirrorLeftHalf { size = defaultBoardSize, aliveCells = cells })


gliderBoard : Board
gliderBoard =
    Board defaultBoardSize <| Set.fromList [ ( 1, 2 ), ( 2, 3 ), ( 3, 1 ), ( 3, 2 ), ( 3, 3 ) ]


initBoard : Board
initBoard =
    mirrorLeftHalf gliderBoard


init : () -> ( Model, Cmd Msg )
init _ =
    ( { board = initBoard
      , turnCount = 0
      , gameType = VsBot Random
      , playerSelection = Completed ( ( 1, 1 ), ( 4, 8 ) )
      , enemySelection = entireSelection
      }
    , Random.generate SetBoard randomSymmetricBoard
    )


leftScore : Board -> Int
leftScore board =
    Set.filter (\c -> first c < board.size // 2) board.aliveCells
        |> Set.size


rightScore : Board -> Int
rightScore board =
    Set.filter (\c -> first c >= board.size // 2) board.aliveCells
        |> Set.size


unwrapInteractiveSelection : InteractiveSelection -> Selection
unwrapInteractiveSelection interactiveSelection =
    case interactiveSelection of
        Partial { start, lastHovered } ->
            ( start, lastHovered )

        Completed selection ->
            selection


entireSelection : Selection
entireSelection =
    ( ( 0, 0 ), ( defaultBoardSize, defaultBoardSize ) )


sortSelection : Selection -> Selection
sortSelection ( ( x1, y1 ), ( x2, y2 ) ) =
    let
        startX =
            min x1 x2

        startY =
            min y1 y2

        endX =
            max x1 x2

        endY =
            max y1 y2
    in
    ( ( startX, startY ), ( endX, endY ) )


cellInSelection : Selection -> Cell -> Bool
cellInSelection selection ( cx, cy ) =
    let
        ( ( startX, startY ), ( endX, endY ) ) =
            sortSelection selection
    in
    startX <= cx && cx <= endX && startY <= cy && cy <= endY


randomCell : Int -> Random.Generator Cell
randomCell boardSize =
    Random.pair (Random.int 0 (boardSize - 1)) (Random.int 0 (boardSize - 1))


randomSelection : Int -> Random.Generator Selection
randomSelection boardSize =
    Random.pair (randomCell boardSize) (randomCell boardSize)


moveSelection : Selection -> MoveSelection -> Selection
moveSelection selection { selectionCorner, direction } =
    let
        ( otherCell, cell ) =
            case selectionCorner of
                Start ->
                    ( second selection, first selection )

                End ->
                    selection

        delta =
            case direction of
                Up ->
                    ( 0, -1 )

                Down ->
                    ( 0, 1 )

                Left ->
                    ( -1, 0 )

                Right ->
                    ( 1, 0 )

        newCell =
            Tuple.mapBoth ((+) (first delta)) ((+) (second delta)) cell
    in
    sortSelection ( newCell, otherCell )


type Msg
    = None
    | Reset
    | SetBoard Board
    | EndPlayerTurn
    | StartSelection Cell
    | UpdateSelection Cell
    | EndSelection
    | EnemyTurn Selection
    | ProcessControl (Maybe Control)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        None ->
            ( model, Cmd.none )

        Reset ->
            init ()

        SetBoard board ->
            ( { model | board = board }, Cmd.none )

        ProcessControl maybeControl ->
            ( case maybeControl of
                Just (Move control) ->
                    { model | playerSelection = Completed <| moveSelection (unwrapInteractiveSelection model.playerSelection) control }

                Nothing ->
                    model
            , Cmd.none
            )

        StartSelection cell ->
            ( { model | playerSelection = Partial { start = cell, lastHovered = cell } }
            , Cmd.none
            )

        UpdateSelection cell ->
            let
                playerSelection =
                    case model.playerSelection of
                        Partial { start } ->
                            Partial { start = start, lastHovered = cell }

                        _ ->
                            model.playerSelection
            in
            ( { model | playerSelection = playerSelection }
            , Cmd.none
            )

        EndSelection ->
            let
                playerSelection =
                    case model.playerSelection of
                        Partial { start, lastHovered } ->
                            Completed ( start, lastHovered )

                        _ ->
                            model.playerSelection
            in
            ( { model | playerSelection = playerSelection }
            , Cmd.none
            )

        EndPlayerTurn ->
            ( model
            , Random.generate EnemyTurn (randomSelection model.board.size)
            )

        EnemyTurn enemySelection ->
            ( { model
                | enemySelection = enemySelection
                , board = nextBoard model.board enemySelection <| unwrapInteractiveSelection model.playerSelection
                , turnCount = model.turnCount + 1
              }
            , Cmd.none
            )


neighbours : Int -> Cell -> Set Cell
neighbours boardSize cell =
    let
        permutations : Set ( Int, Int )
        permutations =
            List.concatMap (\y -> List.map (\x -> ( x, y )) [ -1, 0, 1 ]) [ -1, 0, 1 ]
                |> List.filter (\c -> first c /= 0 || second c /= 0)
                |> Set.fromList

        onGrid : Cell -> Bool
        onGrid c =
            first c >= 0 && first c < boardSize && second c >= 0 && second c < boardSize

        cells : Set Cell
        cells =
            Set.map (\c -> ( first cell + first c, second cell + second c )) permutations
                |> Set.filter onGrid
    in
    cells


nextBoard : Board -> Selection -> Selection -> Board
nextBoard board selection1 selection2 =
    let
        processCell : ( Cell, Int ) -> Maybe Cell
        processCell ( cell, aliveNeighbours ) =
            let
                alive =
                    Set.member cell board.aliveCells

                inSelection1 =
                    cellInSelection selection1 cell

                inSelection2 =
                    cellInSelection selection2 cell

                active =
                    xor inSelection1 inSelection2
            in
            if active then
                case ( alive, aliveNeighbours ) of
                    ( True, 2 ) ->
                        Just cell

                    ( True, 3 ) ->
                        Just cell

                    ( False, 3 ) ->
                        Just cell

                    _ ->
                        Nothing

            else if Set.member cell board.aliveCells then
                Just cell

            else
                Nothing

        cells =
            Set.toList board.aliveCells
                |> List.concatMap (neighbours board.size >> Set.toList)
                |> List.Extra.gatherEquals
                |> List.map (Tuple.mapSecond (\s -> 1 + List.length s))
                |> List.filterMap processCell
                |> Set.fromList
    in
    { board | aliveCells = cells }


dracula :
    { background : Color
    , currentLine : Color
    , foreground : Color
    , comment : Color
    , cyan : Color
    , green : Color
    , orange : Color
    , pink : Color
    , purple : Color
    , red : Color
    , yellow : Color
    }
dracula =
    { background = rgb255 40 42 54
    , currentLine = rgb255 68 71 90
    , foreground = rgb255 248 248 242
    , comment = rgb255 98 114 164
    , cyan = rgb255 139 233 253
    , green = rgb255 80 250 123
    , orange = rgb255 255 184 108
    , pink = rgb255 255 121 198
    , purple = rgb255 189 147 249
    , red = rgb255 255 85 85
    , yellow = rgb255 241 250 140
    }


scaled : Int -> Float
scaled =
    Element.modular 16 1.25


scaledInt : Int -> Int
scaledInt =
    scaled >> round


viewBoard : Model -> Board -> Element Msg
viewBoard model board =
    let
        nextStepBoard =
            nextBoard board ( ( -1, -1 ), ( -1, -1 ) ) entireSelection

        playerSelection =
            unwrapInteractiveSelection model.playerSelection

        viewCell : Cell -> Element Msg
        viewCell cell =
            let
                alive =
                    Set.member cell board.aliveCells

                nextAlive =
                    Set.member cell nextStepBoard.aliveCells

                inSelection =
                    cellInSelection playerSelection cell

                aliveColor =
                    if inSelection then
                        dracula.foreground

                    else
                        dracula.comment

                attributes =
                    if alive then
                        [ Background.color aliveColor ]

                    else
                        [ Border.color dracula.currentLine
                        , Border.width 2
                        , Border.dotted
                        ]

                indicatorSize =
                    px 6

                indicator color =
                    inFront <|
                        el
                            [ centerX
                            , centerY
                            , width indicatorSize
                            , height indicatorSize
                            , Background.color color
                            ]
                            Element.none

                nextStateAttribues =
                    case ( alive, nextAlive ) of
                        ( True, False ) ->
                            [ indicator dracula.background ]

                        ( False, True ) ->
                            [ indicator aliveColor ]

                        _ ->
                            []
            in
            el
                [ width <| px 50
                , height <| px 50
                , padding 6
                , onMouseDown <| StartSelection cell
                , onMouseEnter <| UpdateSelection cell
                ]
            <|
                el
                    (List.concat
                        [ [ width fill
                          , height fill
                          ]
                        , attributes
                        , nextStateAttribues
                        ]
                    )
                    Element.none

        viewRow : Int -> Element Msg
        viewRow y =
            row
                [ width fill
                , height <| fillPortion 1
                ]
            <|
                initialize board.size <|
                    \x -> viewCell ( x, y )

        viewSelection : Int -> PlayerType -> Selection -> Element msg
        viewSelection boardSize player selection =
            let
                ( ( startX, startY ), end ) =
                    sortSelection selection

                ( endX, endY ) =
                    Tuple.mapBoth ((+) 1) ((+) 1) end

                selectionWidth =
                    endX - startX

                selectionHeight =
                    endY - startY

                remainsX =
                    boardSize - endX

                remainsY =
                    boardSize - endY

                color =
                    case player of
                        Player ->
                            dracula.foreground

                        Enemy ->
                            dracula.comment
            in
            column
                [ width fill
                , height fill
                , padding 10
                ]
            <|
                [ el [ width fill, height <| fillPortion <| startY ] Element.none
                , row
                    [ width fill, height <| fillPortion <| selectionHeight ]
                    [ el [ height fill, width <| fillPortion <| startX ] Element.none
                    , el
                        [ height fill
                        , width <| fillPortion <| selectionWidth
                        ]
                      <|
                        el
                            [ height fill
                            , width fill
                            , Border.width 2
                            , Border.color color
                            , Border.dashed
                            ]
                            Element.none
                    , el [ height fill, width <| fillPortion <| remainsX ] Element.none
                    ]
                , el [ width fill, height <| fillPortion <| remainsY ] Element.none
                ]

        centerDelimiter =
            behindContent <|
                row [ width fill, height fill ] <|
                    [ el
                        [ width <| fillPortion 1
                        , height fill
                        , Border.widthEach { bottom = 0, top = 0, left = 0, right = 2 }
                        , Border.color dracula.currentLine
                        , Border.dotted
                        ]
                        Element.none
                    , el [ width <| fillPortion 1, height fill ] Element.none
                    ]
    in
    el
        [ width <| shrink
        , height <| shrink
        , Border.color dracula.currentLine
        , Border.dotted
        , Border.width 2
        , centerDelimiter
        ]
    <|
        column
            [ width fill
            , height fill
            , padding 10
            , behindContent <| viewSelection board.size Player playerSelection
            , behindContent <| viewSelection board.size Enemy model.enemySelection
            ]
        <|
            initialize board.size viewRow


layout : Model -> Element Msg
layout model =
    let
        root =
            el
                [ width fill
                , height fill
                , padding <| scaledInt 1
                , Background.color dracula.background
                , Font.family [ Font.typeface "Inter" ]
                , Font.color dracula.foreground
                , onMouseUp EndSelection
                ]

        score =
            row
                [ width fill
                , height shrink
                ]
                [ el [ alignLeft, Font.size 30 ] <| text <| toString <| leftScore model.board
                , el [ centerX, Font.size 40, Font.color dracula.comment ] <| text <| toString model.turnCount
                , el [ alignRight, Font.size 30 ] <| text <| toString <| rightScore model.board
                ]

        board =
            viewBoard model model.board

        nextTurnButton =
            button
                [ width fill
                , padding 15
                , Border.color dracula.foreground
                , Border.width 2
                , Font.center
                , Font.bold
                ]
                { label = text "Next turn", onPress = Just EndPlayerTurn }

        resetButton =
            button
                [ width fill
                , padding 15
                , Border.color dracula.comment
                , Font.color dracula.comment
                , Border.width 2
                , Font.center
                , Font.bold
                ]
                { label = text "Reset game", onPress = Just Reset }
    in
    root <|
        column
            [ spacing 15
            , centerY
            , centerX
            ]
            [ score
            , board
            , nextTurnButton
            , resetButton
            ]


view : Model -> Html Msg
view model =
    Element.layoutWith
        { options = [ Element.focusStyle { borderColor = Nothing, backgroundColor = Nothing, shadow = Nothing } ] }
        []
    <|
        layout model
