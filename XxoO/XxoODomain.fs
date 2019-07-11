module Domain

module XxoO =

    type Player = | PlayerX | PlayerO
    type CellState = | Played of Player | Empty
    type HorizontalPosition = | Left | HCenter | Right
    type VerticalPosition = | Top | VCenter | Bottom
    type Position = HorizontalPosition * VerticalPosition

    type GameStatus =
        | InProcess
        | Won of Player
        | Tie

    type NextPlayer = Player
    type NextSubGame<'SubGameState> = 'SubGameState
    type FinishedSubGames<'SubGameState> = 'SubGameState list
    type MoveResult<'GameState, 'SubGameState> =
        | State of 'GameState * 'SubGameState * 'SubGameState * NextPlayer
        | GameWon of 'GameState * Player
        | Tie of 'GameState

    // Events
    type NewGame<'GameState> = 'GameState
    type PlayerMove<'GameState, 'SubGameState> = 'GameState * 'SubGameState * Position -> MoveResult<'GameState, 'SubGameState>

    // Helper functions
    type GetCell<'SubGameState, 'Cell> = 'SubGameState * Position -> 'Cell
    type GetCells<'GameState, 'SubGameState, 'Cell> = 'GameState * 'SubGameState -> 'Cell list
    type GetSubGame<'GameState, 'SubGameState> = 'GameState * Position -> 'SubGameState
    type GetSubGames<'GameState, 'SubGameState> = 'GameState -> 'SubGameState list

module XxOoImplementation =
    open XxoO

    type Cell =
        { state : CellState
          position : Position }

    type SubGameState =
        { state : GameStatus
          cells : Cell list }

    type GameState = 
        { subGames : SubGameState list
          nextPlayer : NextPlayer }