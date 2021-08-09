module Main exposing (..)

import Animator as Anim
import Browser
import Browser.Events exposing (onKeyDown)
import Color exposing (Color)
import Debug exposing (todo)
import Element exposing (Attribute, Color, Element, alignLeft, alignRight, behindContent, centerX, centerY, column, el, fill, fillPortion, fromRgb, height, inFront, padding, px, rgb255, row, shrink, spacing, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (onMouseDown, onMouseEnter, onMouseUp)
import Element.Font as Font
import Element.Input exposing (button)
import Html exposing (Html)
import Html.Attributes
import Html.Events
import Json.Decode as Decode
import List.Extra exposing (initialize)
import Random
import Random.Extra
import Random.Set
import Set exposing (Set)
import Set.Extra
import Time
import Tuple exposing (first, second)


flip : (a -> b -> c) -> b -> a -> c
flip f b a =
    f a b


pairSwap : ( a, b ) -> ( b, a )
pairSwap ( a, b ) =
    ( b, a )


main : Program () Model Msg
main =
    Browser.element { init = init, update = update, view = view, subscriptions = subscriptions }


type KeyValue
    = Character Char
    | Control String


type Direction
    = Up
    | Right
    | Down
    | Left


type SelectionCorner
    = LeftTop
    | BottomRight


type alias MoveSelection =
    { selectionCorner : SelectionCorner, direction : Direction }


type Control
    = Move MoveSelection
    | EndTurn
    | ControlReset


toKeyValue : String -> KeyValue
toKeyValue string =
    case String.uncons string of
        Just ( char, "" ) ->
            Character char

        _ ->
            Control string


keyDecoder : Decode.Decoder KeyValue
keyDecoder =
    Decode.map toKeyValue (Decode.field "key" Decode.string)


toControl : KeyValue -> Maybe Control
toControl key =
    case key of
        Character 'w' ->
            Just (Move { selectionCorner = LeftTop, direction = Up })

        Character 's' ->
            Just (Move { selectionCorner = LeftTop, direction = Down })

        Character 'a' ->
            Just (Move { selectionCorner = LeftTop, direction = Left })

        Character 'd' ->
            Just (Move { selectionCorner = LeftTop, direction = Right })

        Control "ArrowUp" ->
            Just (Move { selectionCorner = BottomRight, direction = Up })

        Character 'k' ->
            Just (Move { selectionCorner = BottomRight, direction = Up })

        Control "ArrowDown" ->
            Just (Move { selectionCorner = BottomRight, direction = Down })

        Character 'j' ->
            Just (Move { selectionCorner = BottomRight, direction = Down })

        Control "ArrowLeft" ->
            Just (Move { selectionCorner = BottomRight, direction = Left })

        Character 'h' ->
            Just (Move { selectionCorner = BottomRight, direction = Left })

        Control "ArrowRight" ->
            Just (Move { selectionCorner = BottomRight, direction = Right })

        Character 'l' ->
            Just (Move { selectionCorner = BottomRight, direction = Right })

        Character ' ' ->
            Just EndTurn

        Character 'r' ->
            Just ControlReset

        _ ->
            Nothing


animator : Anim.Animator Model
animator =
    Anim.animator
        |> Anim.watchingWith
            .state
            (\newState model ->
                { model | state = newState }
            )
            (always False)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ onKeyDown (Decode.map (toControl >> ProcessControl) keyDecoder)
        , Anim.toSubscription Tick model animator
        ]


type alias Cell =
    ( Int, Int )


addCell : Cell -> Cell -> Cell
addCell ( a, b ) ( c, d ) =
    ( a + c, b + d )


clampCell : Int -> Cell -> Cell
clampCell boardSize ( x, y ) =
    ( clamp 0 (boardSize - 1) x, clamp 0 (boardSize - 1) y )


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


type alias AliveCells =
    Set Cell


type TurnState
    = Idle
    | ShowSelections Selection


type alias State =
    { aliveCells : AliveCells
    , playerSelection : InteractiveSelection
    , turnState : TurnState
    }


type alias Model =
    { state : Anim.Timeline State
    , boardSize : Int
    , turnCount : Int
    , gameType : GameType
    }


defaultBoardSize : number
defaultBoardSize =
    10


mirrorLeftHalf : Int -> AliveCells -> AliveCells
mirrorLeftHalf boardSize aliveCells =
    let
        rightPart =
            {--Set.map (\c -> ( board.size - 1 - first c, second c )) aliveCells --}
            Set.map (\c -> ( boardSize - 1 - first c, boardSize - 1 - second c )) aliveCells
    in
    Set.union aliveCells rightPart


randomCells : Random.Generator (Set Cell)
randomCells =
    Random.Set.set 24 (randomCell defaultBoardSize)


randomSymmetricBoard : Int -> Random.Generator AliveCells
randomSymmetricBoard boardSize =
    randomCells |> Random.map (mirrorLeftHalf boardSize)



{--gliderBoard : Board
  - gliderBoard =
  -     Board defaultBoardSize <| Set.fromList [ ( 1, 2 ), ( 2, 3 ), ( 3, 1 ), ( 3, 2 ), ( 3, 3 ) ] --}


initState : State
initState =
    { aliveCells = Set.empty
    , playerSelection = Completed ( ( 1, 1 ), ( 4, 8 ) )
    , turnState = Idle
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { state = Anim.init initState
      , boardSize = defaultBoardSize
      , turnCount = 0
      , gameType = VsBot Random
      }
    , Random.generate SetAliveCells <| randomSymmetricBoard defaultBoardSize
    )


playerScore : Int -> AliveCells -> Int
playerScore boardSize aliveCells =
    Set.filter (\c -> first c < boardSize // 2) aliveCells
        |> Set.size


enemyScore : Int -> AliveCells -> Int
enemyScore boardSize aliveCells =
    Set.size aliveCells - playerScore boardSize aliveCells


unwrapInteractiveSelection : InteractiveSelection -> Selection
unwrapInteractiveSelection interactiveSelection =
    case interactiveSelection of
        Partial { start, lastHovered } ->
            ( start, lastHovered )

        Completed selection ->
            selection


entireSelection : Selection
entireSelection =
    ( ( 0, 0 ), ( defaultBoardSize - 1, defaultBoardSize - 1 ) )


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


moveSelection : Int -> Selection -> MoveSelection -> Selection
moveSelection boardSize selection { selectionCorner, direction } =
    let
        ( otherCell, cell ) =
            case selectionCorner of
                LeftTop ->
                    pairSwap selection

                BottomRight ->
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
            clampCell boardSize <| addCell cell delta
    in
    sortSelection ( newCell, otherCell )


type Msg
    = None
    | Reset
    | Tick Time.Posix
    | SetAliveCells AliveCells
    | EndPlayerTurn
    | StartSelection Cell
    | UpdateSelection Cell
    | EndSelection
    | EnemyTurn Selection
    | ProcessControl (Maybe Control)


endPlayerTurn : Model -> ( Model, Cmd Msg )
endPlayerTurn model =
    let
        currentAliveCells =
            (Anim.current model.state).aliveCells
    in
    ( model, Random.generate EnemyTurn (randomSelection model.boardSize) )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        currentState : State
        currentState =
            Anim.current model.state

        currentAliveCells =
            currentState.aliveCells
    in
    case msg of
        None ->
            ( model, Cmd.none )

        Reset ->
            init ()

        Tick time ->
            ( Anim.update time animator model
            , Cmd.none
            )

        SetAliveCells aliveCells ->
            ( { model | state = Anim.go Anim.immediately { aliveCells = aliveCells, playerSelection = currentState.playerSelection, turnState = Idle } model.state }
            , Cmd.none
            )

        ProcessControl maybeControl ->
            case maybeControl of
                Just (Move control) ->
                    let
                        playerSelection =
                            Completed <| moveSelection model.boardSize (unwrapInteractiveSelection currentState.playerSelection) control
                    in
                    ( { model
                        | state = Anim.go Anim.immediately { currentState | playerSelection = playerSelection } model.state
                      }
                    , Cmd.none
                    )

                Just EndTurn ->
                    endPlayerTurn model

                Just ControlReset ->
                    init ()

                Nothing ->
                    ( model, Cmd.none )

        StartSelection cell ->
            let
                playerSelection =
                    Partial { start = cell, lastHovered = cell }
            in
            ( { model
                | state = Anim.go Anim.immediately { currentState | playerSelection = playerSelection } model.state
              }
            , Cmd.none
            )

        UpdateSelection cell ->
            let
                playerSelection =
                    case currentState.playerSelection of
                        Partial { start } ->
                            Partial { start = start, lastHovered = cell }

                        _ ->
                            currentState.playerSelection
            in
            ( { model
                | state = Anim.go Anim.immediately { currentState | playerSelection = playerSelection } model.state
              }
            , Cmd.none
            )

        EndSelection ->
            let
                playerSelection =
                    case currentState.playerSelection of
                        Partial { start, lastHovered } ->
                            Completed ( start, lastHovered )

                        _ ->
                            currentState.playerSelection
            in
            ( { model
                | state = Anim.go Anim.immediately { currentState | playerSelection = playerSelection } model.state
              }
            , Cmd.none
            )

        EndPlayerTurn ->
            endPlayerTurn model

        EnemyTurn enemySelection ->
            let
                newBoard : AliveCells
                newBoard =
                    nextAliveCellsBySelections enemySelection (unwrapInteractiveSelection currentState.playerSelection) model.boardSize currentAliveCells

                steps =
                    [ Anim.event Anim.verySlowly { currentState | turnState = ShowSelections enemySelection }
                    , Anim.wait <| Anim.millis 1000
                    , Anim.event (Anim.millis 1000) { currentState | turnState = ShowSelections enemySelection, aliveCells = newBoard }
                    , Anim.wait <| Anim.millis 1000
                    , Anim.event Anim.verySlowly { currentState | turnState = Idle, aliveCells = newBoard }
                    ]
            in
            ( { model
                | turnCount = model.turnCount + 1
                , state = Anim.queue steps model.state
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
        onGrid =
            cellInSelection entireSelection

        cells : Set Cell
        cells =
            Set.map (\c -> ( first cell + first c, second cell + second c )) permutations
                |> Set.filter onGrid
    in
    cells


nextAliveCellsByPredicate : (Cell -> Bool) -> Int -> AliveCells -> AliveCells
nextAliveCellsByPredicate predicate boardSize aliveCells =
    let
        processCell : ( Cell, Int ) -> Maybe Cell
        processCell ( cell, aliveNeighbours ) =
            let
                alive =
                    Set.member cell aliveCells
            in
            if predicate cell then
                case ( alive, aliveNeighbours ) of
                    ( True, 2 ) ->
                        Just cell

                    ( True, 3 ) ->
                        Just cell

                    ( False, 3 ) ->
                        Just cell

                    _ ->
                        Nothing

            else if Set.member cell aliveCells then
                Just cell

            else
                Nothing
    in
    Set.toList aliveCells
        |> List.concatMap (neighbours boardSize >> Set.toList)
        |> List.Extra.gatherEquals
        |> List.map (Tuple.mapSecond (\s -> 1 + List.length s))
        |> List.filterMap processCell
        |> Set.fromList


nextAliveCellsBySelections : Selection -> Selection -> Int -> AliveCells -> AliveCells
nextAliveCellsBySelections selection1 selection2 =
    let
        predicate cell =
            xor (cellInSelection selection1 cell) (cellInSelection selection2 cell)
    in
    nextAliveCellsByPredicate predicate


nextAliveCells : Int -> AliveCells -> AliveCells
nextAliveCells =
    nextAliveCellsByPredicate <| always True


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


leaf : List (Attribute msg) -> Element msg
leaf attrs =
    el attrs Element.none


pattern : Element.Attribute msg
pattern =
    Html.Attributes.style "background" "radial-gradient(#f8f8f2 2px, transparent 2px),radial-gradient(#f8f8f2 2px, transparent 2px)"
        |> Element.htmlAttribute


classNameToAttribute : String -> Attribute msg
classNameToAttribute =
    Html.Attributes.class >> Element.htmlAttribute


shallowPatternClass : Element.Attribute msg
shallowPatternClass =
    classNameToAttribute "shallow-pattern"


cellPatternClass : Element.Attribute msg
cellPatternClass =
    classNameToAttribute "cell-pattern"


viewBoard : Model -> Element Msg
viewBoard model =
    let
        currentState =
            Anim.current model.state

        previousState =
            Anim.previous model.state

        currentAliveCells =
            currentState.aliveCells

        nextStepAliveCells =
            nextAliveCells model.boardSize currentAliveCells

        playerSelection =
            unwrapInteractiveSelection currentState.playerSelection

        viewCell : Cell -> Element Msg
        viewCell cell =
            let
                currentAlive =
                    Set.member cell currentAliveCells

                nextAlive =
                    Set.member cell nextStepAliveCells

                inSelection =
                    cellInSelection playerSelection cell

                aliveColor =
                    let
                        convertColor =
                            Element.toRgb >> Color.fromRgba

                        activeColor =
                            convertColor dracula.foreground

                        inactiveColor : Color.Color
                        inactiveColor =
                            convertColor dracula.comment

                        idleActivePredicate =
                            cellInSelection playerSelection

                        showSelectionsActivePredicate selection1 selection2 c =
                            xor (cellInSelection selection1 c) (cellInSelection selection2 c)

                        cellColorByPredicate predicate c =
                            if predicate c then
                                activeColor

                            else
                                inactiveColor
                    in
                    fromRgb <|
                        Color.toRgba <|
                            Anim.color model.state <|
                                \state ->
                                    case state.turnState of
                                        Idle ->
                                            cellColorByPredicate idleActivePredicate cell

                                        ShowSelections enemySelection ->
                                            cellColorByPredicate (showSelectionsActivePredicate playerSelection enemySelection) cell

                indicator color =
                    let
                        indicatorSize =
                            px 6
                    in
                    inFront <|
                        leaf
                            [ centerX
                            , centerY
                            , width indicatorSize
                            , height indicatorSize
                            , Background.color color
                            ]

                nextStateAttribues =
                    case ( currentAlive, nextAlive ) of
                        ( True, False ) ->
                            [ indicator dracula.background ]

                        ( False, True ) ->
                            [ indicator aliveColor ]

                        _ ->
                            []

                scale =
                    Anim.linear model.state <|
                        \state ->
                            Anim.at <|
                                if Set.member cell state.aliveCells then
                                    1

                                else
                                    0

                mouseDownEvent =
                    case currentState.turnState of
                        Idle ->
                            StartSelection cell

                        _ ->
                            None

                mouseEnterEvent =
                    case ( currentState.playerSelection, currentState.turnState ) of
                        ( Partial _, Idle ) ->
                            UpdateSelection cell

                        _ ->
                            None

                backgroundAttributes =
                    [ width <| px 50
                    , height <| px 50
                    , padding 6
                    , onMouseDown mouseDownEvent
                    , onMouseEnter mouseEnterEvent
                    ]

                wrapperAttributes =
                    List.concat
                        [ [ width fill
                          , height fill
                          , cellPatternClass
                          ]
                        , nextStateAttribues
                        ]

                cellLeaf =
                    leaf <|
                        [ width fill
                        , height fill
                        , Element.scale scale
                        , Background.color aliveColor
                        ]
            in
            el backgroundAttributes <| el wrapperAttributes cellLeaf

        viewRow : Int -> Element Msg
        viewRow y =
            row
                [ width fill
                , height <| fillPortion 1
                ]
            <|
                initialize model.boardSize <|
                    \x -> viewCell ( x, y )

        viewSelection : Int -> PlayerType -> Selection -> Element msg
        viewSelection boardSize player selection =
            let
                ( ( startX, startY ), end ) =
                    sortSelection selection

                ( endX, endY ) =
                    addCell end ( 1, 1 )

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
                            dracula.foreground

                -- dracula.comment
            in
            column
                [ width fill
                , height fill
                , padding 10
                ]
                [ leaf [ width fill, height <| fillPortion <| startY ]
                , row
                    [ width fill, height <| fillPortion <| selectionHeight ]
                    [ leaf [ height fill, width <| fillPortion <| startX ]
                    , el
                        [ height fill
                        , width <| fillPortion <| selectionWidth
                        ]
                      <|
                        leaf
                            [ height fill
                            , width fill
                            , Border.width 2
                            , Border.color color
                            , Border.dashed
                            ]
                    , leaf [ height fill, width <| fillPortion <| remainsX ]
                    ]
                , leaf [ width fill, height <| fillPortion <| remainsY ]
                ]

        centerDelimiter =
            behindContent <|
                row [ width fill, height fill ] <|
                    [ leaf
                        [ width <| fillPortion 1
                        , height fill
                        , Border.widthEach { bottom = 0, top = 0, left = 0, right = 2 }
                        , Border.color dracula.currentLine
                        , Border.dotted
                        ]
                    , leaf [ width <| fillPortion 1, height fill ]
                    ]

        enemySelectionAttribute =
            let
                alpha =
                    Anim.linear model.state <|
                        \newState ->
                            Anim.at <|
                                case newState.turnState of
                                    ShowSelections _ ->
                                        1

                                    _ ->
                                        0

                selection sel =
                    [ behindContent <| el [ width fill, height fill, Element.alpha alpha ] <| viewSelection model.boardSize Enemy sel ]
            in
            case ( previousState.turnState, currentState.turnState ) of
                ( ShowSelections enemySelection, _ ) ->
                    selection enemySelection

                ( _, ShowSelections enemySelection ) ->
                    selection enemySelection

                _ ->
                    []
    in
    el
        [ width <| shrink
        , height <| shrink
        , Background.color dracula.background
        , Border.width 2
        , Border.color dracula.currentLine
        , centerDelimiter
        ]
    <|
        column
            (List.concat
                [ [ width fill
                  , height fill
                  , padding 10
                  , behindContent <| viewSelection model.boardSize Player playerSelection
                  ]
                , enemySelectionAttribute
                ]
            )
        <|
            initialize model.boardSize viewRow


layout : Model -> Element Msg
layout model =
    let
        currentState =
            Anim.current model.state

        currentAliveCells =
            currentState.aliveCells

        mouseUpEvent =
            case currentState.turnState of
                Idle ->
                    EndSelection

                _ ->
                    None

        root =
            el
                [ width fill
                , height fill
                , padding <| scaledInt 1
                , Background.color dracula.background
                , shallowPatternClass
                , Font.family [ Font.typeface "Inter" ]
                , Font.color dracula.foreground
                , onMouseUp mouseUpEvent
                ]

        score =
            let
                textFromInt =
                    text << String.fromInt
            in
            row
                [ width fill
                , height shrink
                , Font.size 30
                , Font.variant Font.tabularNumbers
                ]
                [ el [ width <| fillPortion 1, Font.alignLeft ] <| textFromInt <| playerScore model.boardSize currentAliveCells
                , el [ width <| fillPortion 1, Font.center, Font.size 40, Font.color dracula.comment ] <| textFromInt model.turnCount
                , el [ width <| fillPortion 1, Font.alignRight ] <| textFromInt <| enemyScore model.boardSize currentAliveCells
                ]

        board =
            viewBoard model

        nextTurnButton =
            button
                [ width fill
                , padding 15
                , Background.color dracula.background
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
                , Background.color dracula.background
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
