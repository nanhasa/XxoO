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

module Decisions =
    open Helpers

    let getCellsThatWontLetOpponentMakeFreeChoice (game : Game) =
        List.where (fun position -> List.contains position game.inProcessSubGamePositions)

    let getCellsThatWontLeadToPlayerWinSubGameNextTurn (player : Player) (game : Game) =
        List.where (        
            game.subGameByPosition
            >> fun sub -> sub.positionsMissingFromAchievableLines player 
            >> List.where (List.length >> (=) 1)
            >> List.isEmpty)

    let getCellsThatWontLetOpponentWinSubGameNextTurn (game : Game) (cellsToChooseFrom : CellPosition list) =
        let _, opponent = players game
        getCellsThatWontLeadToPlayerWinSubGameNextTurn opponent game cellsToChooseFrom
            
    let getCellsThatWontLetOpponentBlockSubGameWhereAiHasNextTurnSubGameWin (game : Game) (cellsToChooseFrom : CellPosition list) =
        getCellsThatWontLeadToPlayerWinSubGameNextTurn game.player game cellsToChooseFrom

    let filterAllUnwantedCells game =
        getCellsThatWontLetOpponentMakeFreeChoice game
        >> getCellsThatWontLetOpponentWinSubGameNextTurn game
        >> getCellsThatWontLetOpponentBlockSubGameWhereAiHasNextTurnSubGameWin game

    let winningSubGameWouldWinGame (game : Game) (player : Player) (sub : SubGamePosition) =
        game.positionsMissingFromAchievableLines player
        |> List.where (List.length >> (=) 1)
        |> List.concat
        |> List.tryFind ((=) sub)
        |> Option.isSome

    let getCellsThatWontLetOpponentWinGameNextTurn game =
        let _, opponent = players game
        List.where (winningSubGameWouldWinGame game opponent)

    let chooseFromWinningCells game filterNewSubGameLineCells filterProgressSubGameLineCells sub cellsToWinSubGame =
        match winningSubGameWouldWinGame game game.player sub with
        | true ->
            makeDecision (sub, cellsToWinSubGame |> List.head)
        | false ->
            let winningSubProgressesLine = 
                game.positionsMissingFromAchievableLines game.player
                |> List.where (List.length >> (=) 2)
                |> List.concat
                |> List.contains sub

            let decide filter =
                match cellsToWinSubGame |> filter with
                | [] -> unableToMakeDecision game
                | cell :: _ -> makeDecision (sub, cell)

            if winningSubProgressesLine
            then decide filterProgressSubGameLineCells
            else decide filterNewSubGameLineCells

    let tryMakeDecisionFromFirstPosition (game : Game) (position : SubGamePosition * CellPosition list) =
        let sub = fst position
        let tryCell = snd position |> List.tryHead
        match tryCell with
        | Some cell -> makeDecision (sub, cell)
        | None -> unableToMakeDecision game

    let tryFindSubGameToWinGame (game : Game) (player : Player) (positionsToWinSubGame : (SubGamePosition * CellPosition list) list) =
        let subGamesAbleToWin = positionsToWinSubGame |> List.map fst
        match subGamesAbleToWin |> List.where (winningSubGameWouldWinGame game player) with
        | [] ->
            unableToMakeDecision game
        | sub :: _ ->
            positionsToWinSubGame |> List.find (fst >> (=) sub) |> tryMakeDecisionFromFirstPosition game

    let tryChooseSubGameToWinGame (positionsToWinSubGame : (SubGamePosition * CellPosition list) list) (game : Game) =
        tryFindSubGameToWinGame game game.player positionsToWinSubGame

    let tryChooseSubGameToWinToPreventOpponentWinningGame (positionsToWinSubGame : (SubGamePosition * CellPosition list) list) (game : Game) =
        let _, opponent = players game
        tryFindSubGameToWinGame game opponent positionsToWinSubGame

    let tryChooseFromWinningPositions (positionsToWinSubGame : (SubGamePosition * CellPosition list) list) (game : Game) =
        positionsToWinSubGame
        |> List.map (fun (sub, cells) -> 
            let newSubGameLineCellFilter = filterAllUnwantedCells game
            let progressSubGameLineCellFilter = getCellsThatWontLetOpponentWinGameNextTurn game
            chooseFromWinningCells game newSubGameLineCellFilter progressSubGameLineCellFilter sub cells)
        |> foldDecisions game   

    let tryChooseFromMultipleWinningPositions (game : Game) (positionsToWinSubGame : (SubGamePosition * CellPosition list) list) =
        tryChooseSubGameToWinGame positionsToWinSubGame game
        >>= tryChooseSubGameToWinToPreventOpponentWinningGame positionsToWinSubGame
        >>= tryChooseFromWinningPositions positionsToWinSubGame

    let positionsOnCellLinesMissingN (n : int) (player : Player) (game : Game) =
        game.playableSubGamePositions
        |> List.map (
            game.subGameByPosition 
            >> fun sub -> 
                sub.position, sub.positionsMissingFromAchievableLines player |> List.where (List.length >> (=) n) |> List.concat)

    let tryGetThirdPositionOnLineInSubGame (player : Player) decideCellWithinOneSubGame decideSubGameAndCellFromMultipleSubGames (game : Game) =
        let positionsToWinSubGame = positionsOnCellLinesMissingN 1 player game
        match positionsToWinSubGame with
        | [] -> unableToMakeDecision game
        | [ (sub, cells) ] ->
            decideCellWithinOneSubGame sub cells
        | positions ->
            decideSubGameAndCellFromMultipleSubGames positions

    let tryWinSubGame decideCellWithinOneSubGame decideSubGameAndCellFromMultipleSubGames (game : Game) =
        tryGetThirdPositionOnLineInSubGame game.player decideCellWithinOneSubGame decideSubGameAndCellFromMultipleSubGames game

    let tryBlockOtherPlayerFromWinningSubGame decideCellWithinOneSubGame decideSubGameAndCellFromMultipleSubGames (game : Game) =
        let _, opponent = players game
        tryGetThirdPositionOnLineInSubGame opponent decideCellWithinOneSubGame decideSubGameAndCellFromMultipleSubGames game

    let tryAddSecondPositionOnCellLine filterCells (game : Game) =
        let positionsToProgressLines = positionsOnCellLinesMissingN 2 game.player game
        match positionsToProgressLines with
        | [] -> unableToMakeDecision game
        | positions ->
            let safePositions =
                positions
                |> List.map (fun (sub, cells) -> sub, cells |> filterCells)
                |> List.where (snd >> List.isEmpty >> not)
            match safePositions with
            | [] -> unableToMakeDecision game
            | position :: _ -> tryMakeDecisionFromFirstPosition game position

    let tryRandomFromList (positions : 'a List) =
        let rnd = Random()
        positions |> List.sortBy (fun _ -> rnd.Next()) |> List.tryHead

    let tryGetRandomPosition (positions : (SubGamePosition * CellPosition list) list) =
        positions
        |> tryRandomFromList
        |> Option.bind (fun (subPos, cellPositions) -> 
            cellPositions |> tryRandomFromList |> Option.map (fun cell -> subPos, cell))

    let tryChooseAnyPositionWithFilter filter (game : Game) =
        let safeCells = game.playableCells |> List.map (fun (sub, cells) -> sub, cells |> filter) |> List.where (snd >> List.isEmpty >> not)
        match safeCells with
        | [] -> unableToMakeDecision game
        | positions -> 
            match tryGetRandomPosition positions with
            | Some pos -> makeDecision pos
            | None -> UnableToMakeDecision game
            
    let chooseRandomPosition (game : Game) =
        match game.playableCells with
        | [] -> failwithf "There are no playable cells to choose from game: %A" game
        | positions -> 
            match tryGetRandomPosition positions with
            | Some pos -> makeDecision pos
            | None -> UnableToMakeDecision game
            
module Api =
    open Decisions
    open Helpers

    type Difficulty =
        | Easy
        | Normal

    let makeAiMove difficulty (game : Game) : SubGamePosition * CellPosition =
        match difficulty with
        | Easy ->
            let aiPlayer, opponent = players game
            
            let decision =
                let fullCellFilter = filterAllUnwantedCells game
                let leanientFilter = getCellsThatWontLetOpponentWinGameNextTurn game
                let chooseWinningCell = chooseFromWinningCells game fullCellFilter leanientFilter
                let chooseFromMultipleWinningSubGames = tryChooseFromMultipleWinningPositions game

                tryWinSubGame chooseWinningCell chooseFromMultipleWinningSubGames game
                >>= tryBlockOtherPlayerFromWinningSubGame chooseWinningCell chooseFromMultipleWinningSubGames
                >>= tryAddSecondPositionOnCellLine fullCellFilter
                >>= tryChooseAnyPositionWithFilter fullCellFilter
                >>= chooseRandomPosition
            
            match decision with
            | DecisionMade (sub, cell) -> sub, cell
            | UnableToMakeDecision game -> failwithf "Unable to make decision with game: %A" game
        | Normal -> 
            (Left, Top), (Left, Top) // Placeholder