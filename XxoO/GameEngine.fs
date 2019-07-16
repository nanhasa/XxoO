namespace XxoO

module Domain =

    type HorizontalPosition = Left | HCenter | Right
    type VerticalPosition = Top | VCenter | Bottom
    type Position = HorizontalPosition * VerticalPosition
    type Line = Line of Position Set

    let cellPositions : Position list =
        [ Left, Top
          HCenter, Top
          Right, Top
          Left, VCenter
          HCenter, VCenter
          Right, VCenter
          Left, Bottom
          HCenter, Bottom
          Right, Bottom ]

    let winningLines : Line list =
        let horizontal = [ Left; HCenter; Right ]
        let vertical = [ Top; VCenter; Bottom ]
        let createLines f = List.map f >> List.map Set.ofList >> List.map Line
        let columns = horizontal |> createLines (fun hor -> vertical |> List.map (fun ver -> hor, ver))
        let rows = vertical |> createLines (fun ver -> horizontal |> List.map (fun hor -> hor, ver))
        let diagA = [ Left, Top;    HCenter, VCenter; Right, Bottom ] |> set |> Line
        let diagB = [ Left, Bottom; HCenter, VCenter; Right, Top ] |> set |> Line
        [ yield! columns
          yield! rows
          yield diagA
          yield diagB ]

    type Player = PlayerX | PlayerO
    type CellState = Played of Player | Empty
    type GameStatus =
        | InProcess
        | Won of Player
        | Tie

    type SubGamePosition = Position
    type CellPosition = Position
    type NextSubGame<'SubGame> = 'SubGame
    type FinishedSubGames<'SubGame> = 'SubGame list
    type MoveResult<'GameState> =
        | InvalidMove of GameState : 'GameState
        | State of GameState : 'GameState
        | GameWon of GameState : 'GameState * Player
        | TieGame of GameState : 'GameState

    // Events
    type NewGame<'GameState> = 'GameState
    type PlayerMove<'GameState> = SubGamePosition -> CellPosition -> 'GameState -> MoveResult<'GameState>

    // Helper functions
    type GetCell<'GameState, 'Cell> = SubGamePosition -> CellPosition -> 'GameState -> 'Cell
    type GetSubGame<'GameState, 'SubGame> = SubGamePosition -> 'GameState -> 'SubGame

    // AI Helper functions
    type GetSubGameEmptyCellPositions<'GameState> = SubGamePosition -> 'GameState -> CellPosition Set
    type GetSubGamePlayerPlayedCellPositions<'GameState> = Player -> SubGamePosition -> 'GameState -> CellPosition Set
    type GetAllEmptyCellPositions<'GameState> = 'GameState -> (SubGamePosition * CellPosition Set) Set

module Models =
    open Domain

    type Cell =
        { status : CellState
          position : CellPosition }

    type SubGame =
        { status : GameStatus
          position : SubGamePosition
          cells : Cell list }

    type GameState = 
        { subGames : SubGame list
          currentSubGame : SubGamePosition Option
          player : Player }

    let createCell status position = 
        { status = status
          position = position }

    let createSubGame status cells position =
        { status = status
          position = position
          cells = cells }

    let createGameState player subGames =
        { subGames = subGames
          currentSubGame = None
          player = player }

    let createEmptyCell = createCell Empty

    let createEmptySubGame = cellPositions |> List.map createEmptyCell |> createSubGame InProcess

    let newGame : NewGame<GameState> =
        cellPositions
        |> List.map createEmptySubGame
        |> createGameState PlayerX

module Helpers =
    open Domain
    open Models

    let isSubGameInProcess subGame =
        match subGame.status with
        | InProcess -> true
        | _ -> false

    let isSubGameFinished subGame =
        subGame |> isSubGameInProcess |> not

    let subGamesInProcess gameState =
        gameState.subGames |> List.where isSubGameInProcess

    let isCellEmpty (cell : Cell) =
        match cell.status with
        | Empty -> true
        | _ -> false

    let isCellPlayedBy player (cell : Cell) =
        match cell.status with
        | Played p when p = player -> true
        | _ -> false

    let isSubGameWonByPlayer player subGame =
        match subGame.status with
        | Won p when p = player -> true
        | _ -> false

    let emptyCells subGame = subGame.cells |> List.where isCellEmpty

    let subGamesWonBy player gameState =
        gameState.subGames |> List.where (isSubGameWonByPlayer player)

    let cellsPlayedBy player subGame =
        subGame.cells |> List.where (isCellPlayedBy player)

    let subGamePosition (subGame : SubGame) = subGame.position
    let subGameStatus (subGame : SubGame) = subGame.status
    let subGameCells (subGame : SubGame) = subGame.cells
    let cellPosition (cell : Cell) = cell.position

    let subGameCellByPosition (position : CellPosition) subGame =
        subGame.cells |> List.find (cellPosition >> (=) position)

    let subGameByPosition (position : SubGamePosition) gameState =
        gameState.subGames |> List.find (subGamePosition >> (=) position)

    let cellInPosition subGamePosition cellPosition =
        subGameByPosition subGamePosition
        >> subGameCellByPosition cellPosition

    let emptyCellsOfSubGame subGamePosition =
        subGameByPosition subGamePosition
        >> fun sub -> 
            match sub.status with
            | InProcess -> 
                sub
                |> subGameCells
                |> List.choose (fun cell -> if isCellEmpty cell then Some cell.position else None)
                |> set
            | _ -> Set.empty

    let playedCellsInSubGameBy player subGamePosition =
        subGameByPosition subGamePosition
        >> cellsPlayedBy player
        >> List.map cellPosition
        >> set

    let allEmptyCellPositions gameState =
        let emptyCellsSet = emptyCells >> List.map cellPosition >> set
        let currentSubGameEmptyCells subPos gameState = [ subPos, (gameState |> emptyCellsOfSubGame subPos) ] |> set
        let allInProcessSubGameEmptyCells gameState = 
            gameState.subGames 
            |> List.where (fun sub -> sub.status = InProcess) 
            |> List.map (fun sub -> sub.position, sub |> emptyCellsSet) 
            |> set

        match gameState.currentSubGame with
        | Some subPos ->
            match gameState |> subGameByPosition subPos |> subGameStatus with
            | InProcess -> currentSubGameEmptyCells subPos gameState
            | _ -> allInProcessSubGameEmptyCells gameState
        | None ->
            allInProcessSubGameEmptyCells gameState

module Implementation =
    open Domain
    open Helpers
    open Models

    let getSubGame : GetSubGame<GameState, SubGame> = subGameByPosition
    let getCell : GetCell<GameState, Cell> = cellInPosition
    let getSubGameEmptyCellPositions : GetSubGameEmptyCellPositions<GameState> = emptyCellsOfSubGame
    let getSubGamePlayerPlayedCellPositions : GetSubGamePlayerPlayedCellPositions<GameState> = playedCellsInSubGameBy
    let getAllEmptyCellPositions : GetAllEmptyCellPositions<GameState> = allEmptyCellPositions

    let positionsFormAnyLine (lines : Line list) (positions : Position list) =
        let playerSet = positions |> Set.ofList
        let cellCountOnLine = fun (Line line) -> line |> Set.intersect playerSet |> Set.count
        let findFirstFullLine = 
            Seq.map cellCountOnLine // Find if all positions in a line exist in player positions
            >> Seq.tryFind ((=) 3)  // Lazily find the first match
            >> Option.isSome
        lines |> findFirstFullLine

    let positionsFormWinningLine = positionsFormAnyLine winningLines
    let hasPlayerWon gameState player = gameState |> (subGamesWonBy player >> List.map subGamePosition >> positionsFormWinningLine)
    let hasPlayerWonSubGame subGame player = subGame |> (cellsPlayedBy player >> List.map cellPosition >> positionsFormWinningLine)

    let calculateStatus (player : Player) (unclaimedEntities : int) (hasWon : Player -> bool) =
        match unclaimedEntities with
        | count when count >= 7 ->
            InProcess
        | _ ->
            if player |> hasWon
            then Won player
            else InProcess
        |> function
            | InProcess -> 
                if unclaimedEntities = 0 
                then Tie 
                else InProcess
            | status -> status
            
    let isMoveValid subGamePosition cellPosition gameState =
        let subGame = gameState |> getSubGame subGamePosition
        let moveIsValidIfCellIsEmpty = subGameCellByPosition cellPosition >> isCellEmpty
        let isInProcessSubGameWithEmptyCell sub =
            if sub |> isSubGameInProcess
            then sub |> moveIsValidIfCellIsEmpty
            else false
        match gameState.currentSubGame with
        | Some subPosition when subPosition = subGamePosition -> isInProcessSubGameWithEmptyCell subGame
        | Some _ -> false
        | None -> isInProcessSubGameWithEmptyCell subGame

    let nextSubGamePosition cellPosition gameState =
        let subGame = gameState |> subGameByPosition cellPosition
        if subGame |> isSubGameInProcess
        then Some (subGame |> subGamePosition)
        else None

    let nextPlayer previousplayer =
        match previousplayer with
        | PlayerX -> PlayerO
        | PlayerO -> PlayerX

    let makeMove subGamePosition cellPosition gameState =
        let player = gameState.player
        let playedCell = { status = Played player; position = cellPosition }
        let getAllButPlayedCells = List.where (fun (cell : Cell) -> cell.position <> cellPosition)
        let getAllButPlayedSubGame = List.where (fun (sub : SubGame) -> sub.position <> subGamePosition)

        let updateSubGame player =
            subGameByPosition subGamePosition
            >> fun sub -> { sub with cells = playedCell :: (sub.cells |> getAllButPlayedCells) }
            >> fun sub -> { sub with status = calculateStatus player (sub |> emptyCells |> List.length) (sub |> hasPlayerWonSubGame) }

        { gameState with subGames = (gameState |> updateSubGame player) :: (gameState.subGames |> getAllButPlayedSubGame) }
        |> fun gameState -> 
            { gameState with
                currentSubGame = gameState |> nextSubGamePosition cellPosition
                player = nextPlayer player }
        |> fun gameState ->
            gameState, calculateStatus player (gameState |> subGamesInProcess |> List.length) (gameState |> hasPlayerWon)

    let playerMove : PlayerMove<GameState> =
        fun subGamePosition cellPosition gameState ->
            match gameState |> isMoveValid subGamePosition cellPosition with
            | true ->
                match gameState |> makeMove subGamePosition cellPosition with
                | newGameState, InProcess ->
                    State newGameState
                | newGameState, Won player ->
                    GameWon (newGameState, player)
                | newGameState, Tie ->
                    TieGame newGameState
            | false ->
                InvalidMove gameState

module GameAPI =
    open Domain
    open Implementation
    open Models

    type Game = GameState

    type XxoOAPI =
        { newGame : NewGame<GameState>
          playerMove : PlayerMove<GameState>
          getCell : GetCell<GameState, Cell>
          getSubGame : GetSubGame<GameState, SubGame> }

    type AIDecisionAPI =
        { getCell : GetCell<GameState, Cell>
          getSubGame : GetSubGame<GameState, SubGame>
          getSubGameEmptyCellPositions : GetSubGameEmptyCellPositions<GameState>
          getSubGamePlayerPlayedCellPositions : GetSubGamePlayerPlayedCellPositions<GameState>
          getAllEmptyCellPositions : GetAllEmptyCellPositions<GameState> }

    let gameAPI =
        { newGame = newGame
          playerMove = playerMove
          getCell = getCell
          getSubGame = getSubGame }

    let aiAPI =
        { getCell = getCell
          getSubGame = getSubGame
          getSubGameEmptyCellPositions = getSubGameEmptyCellPositions
          getSubGamePlayerPlayedCellPositions = getSubGamePlayerPlayedCellPositions
          getAllEmptyCellPositions = getAllEmptyCellPositions }