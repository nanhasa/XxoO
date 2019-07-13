module Domain

module XxoO =

    type Player = PlayerX | PlayerO
    type CellState = Played of Player | Empty
    type HorizontalPosition = Left | HCenter | Right
    type VerticalPosition = Top | VCenter | Bottom
    type Position = HorizontalPosition * VerticalPosition

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

    type GameStatus =
        | InProcess
        | Won of Player
        | Tie

    type SubGamePosition = Position
    type CellPosition = Position
    type NextSubGame<'SubGameState> = 'SubGameState
    type FinishedSubGames<'SubGameState> = 'SubGameState list
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
    type GetEmptyCellPositionsOfSubGame<'GameState> = SubGamePosition -> 'GameState -> CellPosition list
    type GetSubGame<'GameState, 'SubGameState> = SubGamePosition -> 'GameState -> 'SubGameState
    type GetCurrentSubGamePosition<'GameState> = 'GameState -> SubGamePosition option
    type GetFinishedSubGames<'GameState, 'SubGameState> = 'GameState -> 'SubGameState list

module XxoOImplementation =
    open XxoO

    type Cell =
        { state : CellState
          position : Position }

    type SubGameState =
        { state : GameStatus
          position : Position
          cells : Cell list }

    type GameState = 
        { subGames : SubGameState list
          currentSubGame : SubGamePosition Option
          player : Player }

    let cell state position = 
        { state = state
          position = position }

    let subGameState state cells position =
        { state = state
          position = position
          cells = cells }

    let gameState player subGames =
        { subGames = subGames
          currentSubGame = None
          player = player }

    let emptyCell = cell Empty

    let emptySubGame = cellPositions |> List.map emptyCell |> subGameState InProcess

    let newGame : NewGame<GameState> =
        cellPositions
        |> List.map emptySubGame
        |> gameState PlayerX

    let isSubGameFinished subGame =
        match subGame.state with
        | InProcess -> false
        | _ -> true

    let subGamesInProcess gameState =
        gameState.subGames |> List.where (fun sub ->
            match sub.state with
            | InProcess -> true
            | _ -> false)

    let isCellEmpty (cell : Cell) =
        match cell.state with
        | Empty -> true
        | _ -> false

    let emptyCells subGame =
        subGame.cells |> List.where isCellEmpty

    let subGamesWonBy gameState player =
        gameState.subGames |> List.where (fun sub ->
            match sub.state with
            | Won p when p = player -> true
            | _ -> false)

    let cellsPlayedBy subGame player =
        subGame.cells |> List.where (fun cell ->
            match cell.state with
            | Played p when p = player -> true
            | _ -> true)

    let winningCombinations =
        [ // Rows
          [ Left, Top;     HCenter, Top;     Right, Top ]     // Top row
          [ Left, VCenter; HCenter, VCenter; Right, VCenter ] // Middle row
          [ Left, Bottom;  HCenter, Bottom;  Right, Bottom ]  // Bottom row
          // Columns
          [ Left, Top;    Left, VCenter;    Left, Bottom ]    // Left column
          [ HCenter, Top; HCenter, VCenter; HCenter, Bottom ] // Middle column
          [ Right, Top;   Right, VCenter;   Right, Bottom ]   // Right column
          // Diagonals
          [ Left, Top;    HCenter, VCenter; Right, Bottom ]   // Diagonal left-top to right-bottom
          [ Left, Bottom; HCenter, VCenter; Right, Top ] ]    // Diagonal left-bottom to right-top

    let playerHasWinningCombination (winningCombinations : Position list list) positions =
        let positionsInWinningRow = List.choose (fun position -> positions |> List.tryFind ((=) position)) >> List.length
        winningCombinations
        |> List.map positionsInWinningRow // Find if winning combinations exist in player positions
        |> List.where ((=) 3)             // Filter combinations that did not have all three matches
        |> List.isEmpty
        |> not

    let gameStatus gameState =
        match subGamesInProcess gameState |> List.length with
        | unfinishedCount when unfinishedCount >= 7 ->
            InProcess
        | unfinishedCount ->
            let wonSubGamePositions player = subGamesWonBy gameState player |> List.map (fun sub -> sub.position)
            let hasPlayerWon = playerHasWinningCombination winningCombinations
            match wonSubGamePositions gameState.player |> hasPlayerWon with
            | true ->
                Won gameState.player
            | false ->
                if unfinishedCount = 0
                then Tie
                else InProcess

    let subGameStatus subGame player =
        match subGame |> emptyCells |> List.length with
        | unfinishedCount when unfinishedCount >= 7 ->
            InProcess
        | unfinishedCount ->
            let playerPlayedCellPositions player = cellsPlayedBy subGame player |> List.map (fun sub -> sub.position)
            let hasPlayerWon = playerHasWinningCombination winningCombinations
            match playerPlayedCellPositions player |> hasPlayerWon with
            | true ->
                Won player
            | false ->
                if unfinishedCount = 0
                then Tie
                else InProcess

    let getCurrentSubGame : GetCurrentSubGamePosition<GameState> =
        fun gameState -> gameState.currentSubGame

    let getSubGame : GetSubGame<GameState, SubGameState> =
        fun subGamePosition gameState ->
            gameState.subGames |> List.find (fun sub -> sub.position = subGamePosition)

    let getEmptyCellPositionsOfSubGame : GetEmptyCellPositionsOfSubGame<GameState> =
        fun subGamePosition ->
            getSubGame subGamePosition
            >> fun sub -> sub.cells
            >> List.choose (fun cell -> if isCellEmpty cell then Some cell.position else None)

    let getFinishedSubGames : GetFinishedSubGames<GameState, SubGameState> =
        fun gameState ->
            gameState.subGames 
            |> List.choose (fun sub -> if isSubGameFinished sub then Some sub else None)

    let getCell : GetCell<GameState, Cell> =
        fun subGamePosition cellPosition ->
            getSubGame subGamePosition
            >> fun sub -> sub.cells 
            >> List.find (fun cell -> cell.position = cellPosition)

    let isMoveValid subGamePosition cellPosition gameState =
        match gameState.currentSubGame with
        | Some subPosition ->
            if subGamePosition = subPosition then
                match gameState |> getSubGame subGamePosition |> fun sub -> sub.state with
                | InProcess ->
                    gameState
                    |> getCell subGamePosition cellPosition 
                    |> isCellEmpty
                | _ -> false
            else
                false
        | None ->
            true

    let nextSubGame gameState playerCellPosition =
        gameState.subGames
        |> List.find (fun sub -> sub.position = playerCellPosition)

    let nextPlayer previousplayer =
        match previousplayer with
        | PlayerX -> PlayerO
        | PlayerO -> PlayerX

    let makeMove subGamePosition cellPosition gameState =
        let playedCell = { state = Played gameState.player; position = cellPosition }
        let replaceSamePositionCell (cell : Cell) (cells : Cell list) = cells |> List.map (fun c -> if c.position = cell.position then cell else c)
        let updateSubCells cells sub = { sub with cells = cells }

        let updateCells (gameState : GameState, subGamePosition : SubGamePosition) = 
            let sub = gameState |> getSubGame subGamePosition
            let cells = sub.cells |> replaceSamePositionCell playedCell
            gameState, sub, cells

        let updateSubGame (gameState : GameState, subGame : SubGameState, cells : Cell list) =
            let sub = subGame |> updateSubCells cells
            gameState, { sub with state = subGameStatus subGame gameState.player }

        let replaceSamePositionSubGame (gameState : GameState, subGame : SubGameState) = 
            gameState.subGames |> List.map (fun sub -> if sub.position = subGame.position then subGame else sub)

        let updatedSubGames = updateCells >> updateSubGame >> replaceSamePositionSubGame

        let nextSubGame gameState cellPosition = 
            if gameState |> getSubGame subGamePosition |> isSubGameFinished
            then None
            else 
                if gameState |> getSubGame cellPosition |> isSubGameFinished
                then None
                else Some cellPosition

        let updatedGameState = { gameState with subGames = updatedSubGames (gameState, subGamePosition) }

        // Set player value after evaluating the game status
        { updatedGameState with player = nextPlayer updatedGameState.player; currentSubGame = nextSubGame updatedGameState cellPosition }, gameStatus updatedGameState

    let playerMove : PlayerMove<GameState> =
        fun subGamePosition cellPosition gameState ->
            match isMoveValid subGamePosition cellPosition gameState with
            | true ->
                match makeMove subGamePosition cellPosition gameState with
                | newGameState, InProcess ->
                    State newGameState
                | newGameState, Tie ->
                    TieGame newGameState
                | newGameState, Won player ->
                    GameWon (newGameState, player)
            | false ->
                InvalidMove gameState

module XxoOAPI =
    open XxoO
    open XxoOImplementation

    type Game = GameState

    type XxoOAPI =
        { newGame : NewGame<GameState>
          playerMove : PlayerMove<GameState>
          getCell : GetCell<GameState, Cell>
          getEmptyCellPositionsOfSubGame : GetEmptyCellPositionsOfSubGame<GameState>
          getSubGame : GetSubGame<GameState, SubGameState>
          getCurrentSubGamePosition : GetCurrentSubGamePosition<GameState>
          getFinishedSubGames : GetFinishedSubGames<GameState, SubGameState> }

    let getAPI =
        { newGame = newGame
          playerMove = playerMove
          getCell = getCell
          getEmptyCellPositionsOfSubGame = getEmptyCellPositionsOfSubGame
          getSubGame = getSubGame
          getCurrentSubGamePosition = getCurrentSubGame
          getFinishedSubGames = getFinishedSubGames }