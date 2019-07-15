module Domain

module XxoO =

    type Player = PlayerX | PlayerO
    type CellState = Played of Player | Empty
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
        let diagA = set [ Left, Top;    HCenter, VCenter; Right, Bottom ] |> Line
        let diagB = set [ Left, Bottom; HCenter, VCenter; Right, Top ] |> Line
        [ yield! columns
          yield! rows
          yield diagA
          yield diagB ]

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

module XxoOModels =
    open XxoO

    type Cell =
        { status : CellState
          position : Position }

    type SubGame =
        { status : GameStatus
          position : Position
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

module XxoOHelpers =
    open XxoO
    open XxoOModels

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
        >> subGameCells
        >> List.choose (fun cell -> if isCellEmpty cell then Some cell.position else None)

    let playedCellsInSubGameBy player subGamePosition =
        subGameByPosition subGamePosition
        >> cellsPlayedBy player

    let allEmptyCells gameState =
        gameState.subGames
        |> List.map (fun sub -> sub.position, sub |> emptyCells)

module XxoOImplementation =
    open XxoO
    open XxoOHelpers
    open XxoOModels

    let getSubGame : GetSubGame<GameState, SubGame> = subGameByPosition
    let getCell : GetCell<GameState, Cell> = cellInPosition

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
        match gameState.currentSubGame with
        | Some subPosition when subPosition = subGamePosition ->
            match subGame |> subGameStatus with
            | InProcess -> subGame |> moveIsValidIfCellIsEmpty
            | _ -> false
        | Some _ -> false
        | None -> subGame |> moveIsValidIfCellIsEmpty

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
            >> fun sub -> { sub with status = calculateStatus player (sub |> emptyCells |> List.length) (hasPlayerWonSubGame sub) }

        let newGameState =
            { gameState with subGames = (gameState |> updateSubGame player) :: (gameState.subGames |> getAllButPlayedSubGame) }
            |> fun gameState -> 
                { gameState with
                    currentSubGame = gameState |> nextSubGamePosition cellPosition
                    player = nextPlayer player }

        newGameState, calculateStatus player (subGamesInProcess gameState |> List.length) (hasPlayerWon newGameState)

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

module XxoOAPI =
    open XxoO
    open XxoOImplementation
    open XxoOModels

    type Game = GameState

    type XxoOAPI =
        { newGame : NewGame<GameState>
          playerMove : PlayerMove<GameState>
          getCell : GetCell<GameState, Cell>
          getSubGame : GetSubGame<GameState, SubGame> }

    let getAPI =
        { newGame = newGame
          playerMove = playerMove
          getCell = getCell
          getSubGame = getSubGame }