module XxoO.AI

open System
open System.Threading
open XxoO.Domain
open XxoO.GameAPI
open XxoO.Models

type Difficulty =
    | Easy
    | Normal

let makeAiMove difficulty (api : AIDecisionAPI) (gameState : GameState) =
    match difficulty with
    | Easy ->
        let rnd = Random()
        let shuffle (a : 'a list) = a |> List.sortBy (fun _ -> rnd.Next())
        Thread.Sleep 600 // Make AI slower
        api.getAllEmptyCellPositions gameState
        |> Set.toList
        |> shuffle
        |> List.head
        |> fun (subPos, cellPositions) -> subPos, cellPositions |> Set.toList |> shuffle |> List.head
    | Normal -> (Left, Top), (Left, Top) // Placeholder