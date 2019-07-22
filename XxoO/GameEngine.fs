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
    type GetAllInProcessPlayerPlayedCellPositions<'GameState> = Player -> 'GameState -> (SubGamePosition * CellPosition Set) Set
    type GetPlayableEmptyCellPositions<'GameState> = 'GameState -> (SubGamePosition * CellPosition Set) Set
    type GetSubGamePositionsWonByPlayer<'GameState> = Player -> 'GameState -> SubGamePosition Set
    type GetTieSubGamePositions<'GameState> = 'GameState -> SubGamePosition Set
    type GetPositionsMissingFromWinningLines = Position Set -> Position Set -> Position Set Set

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

    let isSubGameTie subGame =
        match subGame.status with
        | Tie -> true
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

    let tieSubGamePositions gameState =
        gameState.subGames |> List.where isSubGameTie |> List.map subGamePosition |> set

    let subGamePositionsWonBy player gameState =
        subGamesWonBy player gameState |> List.map subGamePosition |> set

    let subGameCellByPosition (position : CellPosition) subGame =
        subGame.cells |> List.find (cellPosition >> (=) position)

    let subGameByPosition (position : SubGamePosition) gameState =
        gameState.subGames |> List.find (subGamePosition >> (=) position)

    let cellInPosition subGamePosition cellPosition =
        subGameByPosition subGamePosition
        >> subGameCellByPosition cellPosition

    let subGameEmptyCellPositions subGamePosition =
        subGameByPosition subGamePosition
        >> fun sub -> 
            if isSubGameInProcess sub
            then sub |> emptyCells |> List.map cellPosition
            else List.Empty

    let playerCellsSet player = cellsPlayedBy player >> List.map cellPosition >> set

    let playerPlayedCellPositions player gameState =
        gameState.subGames
        |> List.where isSubGameInProcess
        |> List.map (fun sub -> sub.position, sub |> playerCellsSet player)
        |> List.where (snd >> Set.count >> (<) 0)
        |> set

    let allInProcessEmptyCellPositions gameState = 
        let emptyCellsSet = emptyCells >> List.map cellPosition >> set
        gameState.subGames 
        |> List.where isSubGameInProcess 
        |> List.map (fun sub -> sub.position, sub |> emptyCellsSet) 
        |> set

    let playableEmptyCellPositions gameState =
        let currentSubGameEmptyCells subPos gameState = 
            [ subPos, (gameState |> subGameEmptyCellPositions subPos |> set) ] |> set

        match gameState.currentSubGame with
        | Some subPos ->
            if gameState |> subGameByPosition subPos |> isSubGameInProcess
            then currentSubGameEmptyCells subPos gameState
            else allInProcessEmptyCellPositions gameState
        | None ->
            allInProcessEmptyCellPositions gameState

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

    let positionFormLine (positions : Position Set) =
        fun (Line line) ->
            line |> Set.intersect positions |> Set.count |> (=) 3

    let positionsMissingFromLine (positions : Position Set) =
        fun (Line line) ->
            Set.difference line positions

    let allPositionsAreEmpty emptyPositions positions =
        let positionCount = Set.count positions
        let intersectCount = Set.intersect positions emptyPositions |> Set.count
        positionCount = intersectCount

    let positionsMissingFromAchievableLines (lines : Line list) (emptyPositions : Position Set) (positions : Position Set) =
        lines 
        |> List.map (positionsMissingFromLine positions)
        |> List.where (allPositionsAreEmpty emptyPositions)
        |> set

    let positionsFormAnyLine (lines : Line list) (positions : Position list) =
        let playerSet = positions |> Set.ofList
        let findFirstFullLine = 
            Seq.map (positionFormLine playerSet)
            >> Seq.tryFind id
            >> Option.isSome
        lines |> findFirstFullLine

    let positionsFormWinningLine = positionsFormAnyLine winningLines

    let nextSubGamePosition cellPosition gameState =
        let subGame = gameState |> subGameByPosition cellPosition
        if subGame |> isSubGameInProcess
        then Some (subGame |> subGamePosition)
        else None

    let nextPlayer previousplayer =
        match previousplayer with
        | PlayerX -> PlayerO
        | PlayerO -> PlayerX

    let hasPlayerWon gameState player = gameState |> (subGamesWonBy player >> List.map subGamePosition >> positionsFormWinningLine)
    let hasPlayerWonSubGame subGame player = subGame |> (cellsPlayedBy player >> List.map cellPosition >> positionsFormWinningLine)

module Implementation =
    open Domain
    open Helpers
    open Models

    let calculateStatus (player : Player) (unclaimedEntities : int) (hasWon : Player -> bool) =
        match unclaimedEntities with
        | count when count >= 7 ->
            InProcess
        | count when count > 0 ->
            if player |> hasWon
            then Won player
            else InProcess
        | _ ->
            if player |> hasWon
            then Won player
            else Tie
            
    let isMoveValid subGamePosition cellPosition gameState =
        let subGame = gameState |> subGameByPosition subGamePosition
        let moveIsValidIfCellIsEmpty = subGameCellByPosition cellPosition >> isCellEmpty
        let isInProcessSubGameWithEmptyCell sub =
            if sub |> isSubGameInProcess
            then sub |> moveIsValidIfCellIsEmpty
            else false
        match gameState.currentSubGame with
        | Some subPosition when subPosition = subGamePosition -> isInProcessSubGameWithEmptyCell subGame
        | Some _ -> false
        | None -> isInProcessSubGameWithEmptyCell subGame

    let createNewState subGamePosition cellPosition gameState =
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

    let makeMove : PlayerMove<GameState> =
        fun subGamePosition cellPosition gameState ->
            match gameState |> isMoveValid subGamePosition cellPosition with
            | true ->
                match gameState |> createNewState subGamePosition cellPosition with
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
    open Helpers

    type Game = GameState

    type XxoOAPI =
        { newGame : NewGame<GameState>
          playerMove : PlayerMove<GameState>
          getCell : GetCell<GameState, Cell>
          getSubGame : GetSubGame<GameState, SubGame> }

    type GameStateInformationAPI =
        { getAllInProcessPlayerPlayedCellPositions : GetAllInProcessPlayerPlayedCellPositions<GameState>
          getPlayableEmptyCellPositions : GetPlayableEmptyCellPositions<GameState>
          getSubGamePositionsWonByPlayer : GetSubGamePositionsWonByPlayer<GameState>
          getTieSubGamePositions : GetTieSubGamePositions<GameState>
          getPositionsMissingFromWinningLines : GetPositionsMissingFromWinningLines }

    let gameplayAPI =
        { newGame = newGame
          playerMove = makeMove
          getCell = cellInPosition
          getSubGame = subGameByPosition }

    let informationAPI =
        { getAllInProcessPlayerPlayedCellPositions = playerPlayedCellPositions
          getPlayableEmptyCellPositions = playableEmptyCellPositions
          getSubGamePositionsWonByPlayer = subGamePositionsWonBy
          getTieSubGamePositions = tieSubGamePositions
          getPositionsMissingFromWinningLines = positionsMissingFromAchievableLines winningLines }