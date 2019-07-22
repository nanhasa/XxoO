module XxoO.AI

open System
open System.Threading
open XxoO.Domain
open XxoO.GameAPI
open XxoO.Models

module Helpers =

    type AiInformation =
        { playableEmptyCells : (SubGamePosition * CellPosition Set) Set
          aiCellsInProcessSubs : (SubGamePosition * CellPosition Set) Set
          aiWonSubGamePositions : SubGamePosition Set
          opponentCellsInProcessSubs : (SubGamePosition * CellPosition Set) Set
          opponentWonSubGamePositions : SubGamePosition Set
          tieSubGamePositions : SubGamePosition Set
          positionsMissingFromWinningLines : Position Set -> Position Set -> Position Set Set
          decision : (SubGamePosition * CellPosition) option }

    let makeDecision info position = { info with decision = position }

    // Evaluate next plan if there was no previous plan
    let inline (>=>) f g x : AiInformation =
        let d = f x
        match d.decision with
        | Some _ -> d
        | None -> g d

    let inline (<*>) f g x =
        let d = f x
        match d with
        | Some e -> Some e
        | None -> g x

    let tryCellsOfSubGame subGamePosition (positions : (SubGamePosition * CellPosition Set) Set) =
        positions
        |> Set.toList
        |> List.tryFind (fst >> (=) subGamePosition)
        |> Option.map snd

    let finishedSubGamePositions info =
        Set.union info.aiWonSubGamePositions info.opponentWonSubGamePositions 
        |> Set.union info.tieSubGamePositions

    let otherPlayer =
        function
        | PlayerX -> PlayerO
        | PlayerO -> PlayerX

    let emptyPositionsOnLinesMissingNPositions n getMissingPositions emptyPositions positions : Position Set list =
        getMissingPositions emptyPositions positions |> Set.toList |> List.where (Set.count >> (=) n)
    
    let positionsToWin getMissingPositions availablePositions ownedPositions =
        emptyPositionsOnLinesMissingNPositions 1 getMissingPositions availablePositions ownedPositions
        |> List.map (Set.toList >> List.head)
        |> List.where (fun pos -> Set.contains pos availablePositions)

    let positionsToProgressLine getMissingPositions availablePositions ownedPositions =
        emptyPositionsOnLinesMissingNPositions 2 getMissingPositions availablePositions ownedPositions
        |> List.map (Set.toList)
        |> List.concat
        |> set
        |> Set.toList

    let positionsToWinSubGame info ownedPositions (subGamePosition, emptyCells) =
        tryCellsOfSubGame subGamePosition ownedPositions
        |> Option.map (positionsToWin info.positionsMissingFromWinningLines emptyCells)
        |> Option.map (fun positions -> subGamePosition, positions)

    let positionsToProgressLineInSubGame info (subGamePosition, emptyCells) =
        tryCellsOfSubGame subGamePosition info.aiCellsInProcessSubs
        |> Option.map (positionsToProgressLine info.positionsMissingFromWinningLines emptyCells)
        |> Option.map (fun positions -> subGamePosition, positions)

    let tryWinGame info winnableSubGamePositions =
        positionsToWin info.positionsMissingFromWinningLines winnableSubGamePositions info.aiWonSubGamePositions
        |> List.tryHead

    let tryBlockOpponentWinGame info winnableSubGamePositions =
        positionsToWin info.positionsMissingFromWinningLines winnableSubGamePositions info.opponentWonSubGamePositions
        |> List.tryHead

    let tryProgressSubGameLine info winnableSubGamePositions =
        positionsToProgressLine info.positionsMissingFromWinningLines winnableSubGamePositions info.aiWonSubGamePositions
        |> List.tryHead

    let tryChooseCellThatWontLetOpponentHaveFreeChoice info (subGamePosition : SubGamePosition, cellPositions : CellPosition list) =
        let finishedSubGames = finishedSubGamePositions info
        match Set.difference (cellPositions |> set) finishedSubGames |> Set.toList with
        | [] -> None
        | safeCells -> Some (subGamePosition, safeCells)

    let tryChooseCellThatWontLetOpponentWinSubGame info (subGamePosition : SubGamePosition, cellPositions : CellPosition list) =
        let subGamesWhereOpponentWouldWinNextTurn =
            info.playableEmptyCells
            |> Set.toList
            |> List.choose (positionsToWinSubGame info info.opponentCellsInProcessSubs)
            |> List.map fst
            |> set

        match Set.difference (cellPositions |> set) subGamesWhereOpponentWouldWinNextTurn |> Set.toList with
        | [] -> None
        | safeCells -> Some (subGamePosition, safeCells)

    let tryPickFirstSubGameAndCell =
        List.tryHead
        >> Option.map (fun (sub, cells) -> cells |> List.tryHead |> Option.map (fun cell -> sub, cell))
        >> Option.flatten

    let randomPosition (positions : 'a List) =
        let rnd = Random()
        positions |> List.sortBy (fun _ -> rnd.Next()) |> List.head

    let tryRandomPosition (positions : 'a List) =
        let rnd = Random()
        positions |> List.sortBy (fun _ -> rnd.Next()) |> List.tryHead

module Decisions =
    open Helpers

    let chooseSubGameToWin info : SubGamePosition Set -> SubGamePosition option =
        tryWinGame info 
        <*> tryBlockOpponentWinGame info
        <*> tryProgressSubGameLine info

    let cellsToAvoid info =
        tryChooseCellThatWontLetOpponentHaveFreeChoice info
        >> Option.bind (tryChooseCellThatWontLetOpponentWinSubGame info)

    let tryWinSubGame (chooseFromMultipleHits : SubGamePosition Set -> SubGamePosition option) info : AiInformation =
        let positions =
            info.playableEmptyCells
            |> Set.toList
            |> List.choose (positionsToWinSubGame info info.aiCellsInProcessSubs)

        let chosenPosition =
            chooseFromMultipleHits (positions |> List.map fst |> set)
            |> Option.map (fun subGamePos -> 
                positions 
                |> List.tryFind (fst >> (=) subGamePos) 
                |> Option.bind (fun (subPos, cells) -> cells |> List.tryHead |> Option.map (fun cell -> subPos, cell)))
            |> Option.flatten

        match chosenPosition with
        | Some pos -> makeDecision info (Some pos)
        | None -> tryPickFirstSubGameAndCell positions |> makeDecision info

    let tryBlockOtherPlayerFromWinningSubGame info =
        info.playableEmptyCells
        |> Set.toList
        |> List.choose (positionsToWinSubGame info info.opponentCellsInProcessSubs)
        |> tryPickFirstSubGameAndCell
        |> makeDecision info

    let tryAddSecondPositionOnLine (avoidCellslogic : SubGamePosition * CellPosition List -> (SubGamePosition * CellPosition List) Option) info =
        info.playableEmptyCells
        |> Set.toList
        |> List.choose (positionsToProgressLineInSubGame info >> Option.bind avoidCellslogic)
        |> tryPickFirstSubGameAndCell
        |> makeDecision info

    let tryAddRandomPositionWithoutNextPlayerGettingFreeChoice info =
        info.playableEmptyCells
        |> Set.toList
        |> List.map (fun (sub, cells) -> sub, Set.toList cells)
        |> List.choose (tryChooseCellThatWontLetOpponentHaveFreeChoice info)
        |> tryRandomPosition
        |> Option.map (fun (subPos, cellPositions) -> subPos, cellPositions |> randomPosition)
        |> makeDecision info

    let randomFromEmptyCells info =
        info.playableEmptyCells
        |> Set.toList
        |> randomPosition
        |> fun (subPos, cellPositions) -> subPos, cellPositions |> Set.toList |> randomPosition
        |> Some
        |> makeDecision info

module Api =
    open Decisions
    open Helpers

    type Difficulty =
        | Easy
        | Normal

    let makeAiMove difficulty (api : GameStateInformationAPI) (gameState : GameState) : SubGamePosition * CellPosition =
        match difficulty with
        | Easy ->
            let aiPlayer = gameState.player
            let opponentPlayer = otherPlayer aiPlayer
            let info = 
                { playableEmptyCells = api.getPlayableEmptyCellPositions gameState
                  aiCellsInProcessSubs = api.getAllInProcessPlayerPlayedCellPositions aiPlayer gameState
                  aiWonSubGamePositions = api.getSubGamePositionsWonByPlayer aiPlayer gameState
                  opponentCellsInProcessSubs = api.getAllInProcessPlayerPlayedCellPositions opponentPlayer gameState
                  opponentWonSubGamePositions = api.getSubGamePositionsWonByPlayer opponentPlayer gameState
                  tieSubGamePositions = api.getTieSubGamePositions gameState
                  positionsMissingFromWinningLines = api.getPositionsMissingFromWinningLines
                  decision = None }

            let decisionChain =
                tryWinSubGame (chooseSubGameToWin info)
                >=> tryBlockOtherPlayerFromWinningSubGame
                >=> tryAddSecondPositionOnLine (cellsToAvoid info)
                >=> tryAddRandomPositionWithoutNextPlayerGettingFreeChoice
                >=> randomFromEmptyCells

            decisionChain info
            |> fun info ->
                match info.decision with
                | Some decision -> decision
                | None -> failwithf "AI is unable to make decision with info %A" info
        | Normal -> 
            (Left, Top), (Left, Top) // Placeholder