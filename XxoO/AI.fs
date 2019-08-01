module XxoO.AI

open System
open XxoO.Domain
open XxoO.GameInfoAPI

module Helpers =

    type Decision =
        | UnableToMakeDecision of Game
        | DecisionMade of SubGamePosition * CellPosition

    let bindDecision f x =
        match x with
        | UnableToMakeDecision game -> f game
        | DecisionMade (sub, cell) -> DecisionMade (sub, cell)

    let makeDecision x = DecisionMade x
    let unableToMakeDecision game = UnableToMakeDecision game

    let tryMakeDecisionFromTuple game position =
        match position with
        | Some (sub, cell) -> makeDecision (sub, cell)
        | None -> unableToMakeDecision game

    let tryMakeDecision game sub cell = 
        match sub, cell with
        | Some sub, Some cell -> makeDecision (sub, cell)
        | _ -> unableToMakeDecision game

    let tryMakeDecisionFromFirstPosition (game : Game) (position : (SubGamePosition * CellPosition list) option) =
        match position with
        | Some position ->
            let sub = fst position
            let tryCell = snd position |> List.tryHead
            tryMakeDecision game (Some sub) tryCell
        | None -> unableToMakeDecision game

    let foldDecisions game decisions =
        match decisions |> List.tryPick (function | DecisionMade (sub, cell) -> Some (sub, cell) | UnableToMakeDecision _ -> None) with
        | Some (sub, cell) -> makeDecision (sub, cell)
        | None -> unableToMakeDecision game

    let (>>=) x f = bindDecision f x

    let players (game : Game) =
        let otherPlayer =
            function
            | PlayerX -> PlayerO
            | PlayerO -> PlayerX
        game.player, otherPlayer game.player

    let tryRandomFromList (positions : 'a List) =
        let rnd = Random()
        positions |> List.sortBy (fun _ -> rnd.Next()) |> List.tryHead

    let tryGetRandomPosition (positions : (SubGamePosition * CellPosition list) list) =
        positions
        |> tryRandomFromList
        |> Option.bind (fun (subPos, cellPositions) -> cellPositions |> tryRandomFromList |> Option.map (fun cell -> subPos, cell))

    let winningSubGameWouldWinGame (game : Game) (player : Player) (sub : SubGamePosition) =
        game.positionsMissingFromAchievableLines player
        |> List.where (List.length >> (=) 1)
        |> List.concat
        |> List.tryFind ((=) sub)
        |> Option.isSome

    let positionsOnCellLinesMissingN (n : int) (player : Player) (game : Game) =
        game.playableSubGamePositions
        |> List.map (
            game.subGameByPosition 
            >> fun sub -> sub.position, sub.positionsMissingFromAchievableLines player |> List.where (List.length >> (=) n) |> List.concat)
        |> List.where (snd >> List.isEmpty >> not)

module Filters =
    open Helpers

    let filterCellsThatWouldLetOpponentMakeFreeChoice (game : Game) : CellPosition list -> CellPosition list =
        List.where (fun position -> List.contains position game.inProcessSubGamePositions)

    let filterCellsThatWouldLeadtoPlayerWinSubGameNextTurn (player : Player) (getSub : SubGamePosition -> Sub) : CellPosition list -> CellPosition list =
        List.where (        
            getSub
            >> fun sub -> sub.positionsMissingFromAchievableLines player 
            >> List.where (List.length >> (=) 1)
            >> List.isEmpty)

    let filterCellsThatWouldLetOpponentWinSubGameNextTurn (game : Game) (cellsToChooseFrom : CellPosition list) =
        let _, opponent = players game
        filterCellsThatWouldLeadtoPlayerWinSubGameNextTurn opponent game.subGameByPosition cellsToChooseFrom
            
    let filterCellsThatWouldLetOpponentBlockSubGameWhereAiHasNextTurnSubGameWin (game : Game) (cellsToChooseFrom : CellPosition list) =
        filterCellsThatWouldLeadtoPlayerWinSubGameNextTurn game.player game.subGameByPosition cellsToChooseFrom

    let filterAllUnwantedCells game =
        filterCellsThatWouldLetOpponentMakeFreeChoice game
        >> filterCellsThatWouldLetOpponentWinSubGameNextTurn game
        >> filterCellsThatWouldLetOpponentBlockSubGameWhereAiHasNextTurnSubGameWin game

    let filterCellsThatWouldLetOpponentWinGameNextTurn game =
        let _, opponent = players game
        List.where (winningSubGameWouldWinGame game opponent)

module Decisions =
    open Helpers
    open Filters

    let tryDecideFromWinningCells game strictCellFilter leanientCellFilter sub cellsToWinSubGame =
        match winningSubGameWouldWinGame game game.player sub with
        | true ->
            tryMakeDecision game (Some sub) (cellsToWinSubGame |> List.tryHead)
        | false ->
            let winningSubProgressesLine = 
                game.positionsMissingFromAchievableLines game.player
                |> List.where (List.length >> (=) 2)
                |> List.concat
                |> List.contains sub

            let decide = List.tryHead >> tryMakeDecision game (Some sub)
            if winningSubProgressesLine
            then leanientCellFilter cellsToWinSubGame |> decide
            else strictCellFilter cellsToWinSubGame |> decide

    let tryFindSubGameToWinGame (game : Game) (player : Player) (positionsToWinSubGame : (SubGamePosition * CellPosition list) list) =
        positionsToWinSubGame 
        |> List.tryFind (fst >> winningSubGameWouldWinGame game player)
        |> Option.bind (fun (firstWinningSub,_) -> positionsToWinSubGame |> List.tryFind (fst >> (=) firstWinningSub)) 
        |> tryMakeDecisionFromFirstPosition game

    let tryChooseSubGameToWinGame (positionsToWinSubGame : (SubGamePosition * CellPosition list) list) (game : Game) =
        tryFindSubGameToWinGame game game.player positionsToWinSubGame

    let tryChooseSubGameToWinToPreventOpponentWinningGame (positionsToWinSubGame : (SubGamePosition * CellPosition list) list) (game : Game) =
        let _, opponent = players game
        tryFindSubGameToWinGame game opponent positionsToWinSubGame

    let tryChooseFromWinningPositions (positionsToWinSubGame : (SubGamePosition * CellPosition list) list) (game : Game) =
        positionsToWinSubGame
        |> List.map (fun (sub, cells) -> 
            let newSubGameLineCellFilter = filterAllUnwantedCells game
            let progressSubGameLineCellFilter = filterCellsThatWouldLetOpponentWinGameNextTurn game
            tryDecideFromWinningCells game newSubGameLineCellFilter progressSubGameLineCellFilter sub cells)
        |> foldDecisions game   

    let tryMakeDecisionFromMultipleWinningPositions (game : Game) (positionsToWinSubGame : (SubGamePosition * CellPosition list) list) =
        tryChooseSubGameToWinGame positionsToWinSubGame game
        >>= tryChooseSubGameToWinToPreventOpponentWinningGame positionsToWinSubGame
        >>= tryChooseFromWinningPositions positionsToWinSubGame

    let tryGetThirdPositionOnLineInSubGame (player : Player) decideCellWithinOneSubGame decideSubGameAndCellFromMultipleSubGames (game : Game) =
        let positionsToWinSubGame = positionsOnCellLinesMissingN 1 player game
        match positionsToWinSubGame with
        | [] -> unableToMakeDecision game
        | [ sub, cells ] -> decideCellWithinOneSubGame sub cells
        | positions -> decideSubGameAndCellFromMultipleSubGames positions

    let tryWinSubGame decideCellWithinOneSubGame decideSubGameAndCellFromMultipleSubGames (game : Game) =
        tryGetThirdPositionOnLineInSubGame game.player decideCellWithinOneSubGame decideSubGameAndCellFromMultipleSubGames game

    let tryBlockOtherPlayerFromWinningSubGame decideCellWithinOneSubGame decideSubGameAndCellFromMultipleSubGames (game : Game) =
        let _, opponent = players game
        tryGetThirdPositionOnLineInSubGame opponent decideCellWithinOneSubGame decideSubGameAndCellFromMultipleSubGames game

    let tryAddSecondPositionOnCellLine filterCells (game : Game) =
        positionsOnCellLinesMissingN 2 game.player game
        |> List.map (fun (sub, cells) -> sub, cells |> filterCells)
        |> List.tryFind (snd >> List.isEmpty >> not)
        |> tryMakeDecisionFromFirstPosition game

    let tryChooseRandomPlayablePositionWithFilter filter (game : Game) =
        game.playableCells 
        |> List.map (fun (sub, cells) -> sub, cells |> filter) 
        |> List.where (snd >> List.isEmpty >> not)
        |> tryGetRandomPosition
        |> tryMakeDecisionFromTuple game
            
    let tryChooseRandomPlayablePosition (game : Game) =
        game.playableCells
        |> tryGetRandomPosition
        |> tryMakeDecisionFromTuple game
            
module Api =
    open Decisions
    open Helpers
    open Filters

    type Difficulty =
        | Easy
        | Normal

    let makeAiMove difficulty (game : Game) =
        let strictCellFilter = filterAllUnwantedCells game
        let leanientFilter = filterCellsThatWouldLetOpponentWinGameNextTurn game
        let chooseWinningCell = tryDecideFromWinningCells game strictCellFilter leanientFilter
        let chooseFromMultipleWinningSubGames = tryMakeDecisionFromMultipleWinningPositions game

        match difficulty with
        | Easy ->
            tryWinSubGame chooseWinningCell chooseFromMultipleWinningSubGames game
            >>= tryBlockOtherPlayerFromWinningSubGame chooseWinningCell chooseFromMultipleWinningSubGames
            >>= tryAddSecondPositionOnCellLine strictCellFilter
            >>= tryChooseRandomPlayablePosition
        | Normal -> 
            tryWinSubGame chooseWinningCell chooseFromMultipleWinningSubGames game
            >>= tryBlockOtherPlayerFromWinningSubGame chooseWinningCell chooseFromMultipleWinningSubGames
            >>= tryAddSecondPositionOnCellLine strictCellFilter
            >>= tryChooseRandomPlayablePositionWithFilter strictCellFilter
            >>= tryChooseRandomPlayablePositionWithFilter leanientFilter
            >>= tryChooseRandomPlayablePosition
        |> function
            | DecisionMade (sub, cell) -> Ok (sub, cell)
            | UnableToMakeDecision game -> Error (sprintf "Unable to make decision with game: %A" game)