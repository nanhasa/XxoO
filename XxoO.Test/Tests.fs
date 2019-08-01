module Tests

open System
open Xunit

open XxoO.Domain
open XxoO.Helpers

[<Fact>]
let ``All winning lines should pass winning line check`` () =
    winningLines
    |> List.map (fun (Line line) -> line |> Set.toList)
    |> List.where (positionsFormWinningLine >> not)
    |> Assert.Empty

let horizontal = [ Left; HCenter; Right ]
let vertical = [ Top; VCenter; Bottom ]

[<Fact>]
let ``Any one position line should not pass winning line check`` () =
    List.allPairs horizontal vertical
    |> List.where (fun pos -> positionsFormWinningLine [ pos ])
    |> Assert.Empty

[<Fact>]
let ``Any two positions line should not pass winning line check`` () =
    let linePositions = winningLines |> List.map (fun (Line line) -> line |> Set.toList)
    let removeNPos n = List.mapi (fun i pos -> i, pos) >> List.where (fst >> (=) n >> not) >> List.map snd
    
    [ yield linePositions |> List.map (removeNPos 0)
      yield linePositions |> List.map (removeNPos 1)
      yield linePositions |> List.map (removeNPos 2) ]
    |> List.concat
    |> List.where positionsFormWinningLine
    |> Assert.Empty
    
[<Fact>]
let ``Multiple full lines at the same time pass line check`` () =
    List.allPairs horizontal vertical
    |> positionsFormWinningLine
    |> Assert.True

[<Fact>]
let ``More than three positions but no full lines should not pass validation`` () =
    let case1 = [ Left, Top; Right, Top; Right, Bottom; Left, Bottom ]
    let case2 = [ HCenter, Top; Right, VCenter; HCenter, Bottom; Left, VCenter ]

    [ case1; case2 ]
    |> List.where positionsFormWinningLine
    |> Assert.Empty