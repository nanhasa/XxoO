// Copyright 2018-2019 Fabulous contributors. See LICENSE.md for license.
namespace XxoO

open System.Diagnostics
open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms
open Domain.XxoO
open Domain.XxoOAPI

module App =
    
    let api = getAPI

    type Model = 
      { gameState : Game
        gameStatus : GameStatus }

    type Msg = 
        | NewGame
        | PlayerMove of SubGamePosition * CellPosition

    let initModel = { gameState = api.newGame; gameStatus = InProcess }

    let init () = initModel, Cmd.none

    let playerMoved model subGamePosition cellPosition =
        match model.gameState |> api.playerMove subGamePosition cellPosition with
        | State gameState | InvalidMove gameState ->
            { model with gameState = gameState; gameStatus = InProcess }, Cmd.none
        | GameWon (gameState, player) ->
            { model with gameState = gameState; gameStatus = Won player }, Cmd.none
        | TieGame gameState ->
            { model with gameState = gameState; gameStatus = Tie }, Cmd.none

    let update msg model =
        match msg with
        | NewGame -> init()
        | PlayerMove (subGamePos, cellPos) -> playerMoved model subGamePos cellPos

    let currentPlayer player =
        match player with
        | PlayerX -> "Player X"
        | PlayerO -> "Player O"

    let cellOwner cellStatus =
        match cellStatus with
        | Played PlayerX -> "X"
        | Played PlayerO -> "O"
        | Empty -> ""

    let cellColor cellStatus =
        match cellStatus with
        | Played PlayerX -> Color.LightSkyBlue
        | Played PlayerO -> Color.Orange
        | Empty -> Color.AntiqueWhite

    let wonSubColor player =
        match player with
        | PlayerX -> Color.LightSkyBlue
        | PlayerO -> Color.Orange

    let gridButtons subGamePos model dispatch =
        let cellStatus cellPos = model.gameState |> api.getCell subGamePos cellPos |> fun cell -> cell.status
        cellPositions
        |> List.mapi (fun i cellPos ->
            let status = cellStatus cellPos
            View.Button(
                text = (status |> cellOwner), 
                fontSize = 15,
                textColor = Color.White,
                backgroundColor = (status |> cellColor),
                padding = Thickness 0.1,
                command = (fun _ -> dispatch (PlayerMove (subGamePos, cellPos)))
            ).GridRow(i / 3).GridColumn(i % 3))

    let gridBackgroundColor subGamePos model =
        match model.gameState.currentSubGame with
        | Some sub when sub = subGamePos -> Color.LightGreen
        | Some _ -> Color.WhiteSmoke
        | None -> Color.WhiteSmoke

    let subGrids model dispatch =
        cellPositions
        |> List.mapi (fun i pos ->
            match model.gameState |> api.getSubGame pos |> fun sub -> sub.status with
            | Won player ->
                View.BoxView(wonSubColor player, cornerRadius = CornerRadius(10.)).GridRow(i / 3).GridColumn(i % 3)
            | Tie ->
                View.BoxView(Color.Gray, cornerRadius = CornerRadius(10.)).GridRow(i / 3).GridColumn(i % 3)
            | InProcess ->
                View.Grid(
                    rowdefs = ["*"; "*"; "*"], 
                    coldefs = ["*"; "*"; "*"],
                    margin = Thickness 3.,
                    backgroundColor = (gridBackgroundColor pos model),
                    children = (gridButtons pos model dispatch)
                ).GridRow(i / 3).GridColumn(i % 3))

    let view (model: Model) dispatch =
        View.ContentPage(
          content = View.StackLayout(
            padding = 20.0, 
            verticalOptions = LayoutOptions.Center,
            children = [
                match model.gameStatus with
                | InProcess ->
                    yield View.Label(
                        text = currentPlayer model.gameState.player, 
                        verticalTextAlignment = TextAlignment.Start, 
                        horizontalTextAlignment = TextAlignment.Center, 
                        fontAttributes = FontAttributes.Bold, 
                        fontSize = 20)
                    yield View.Grid (
                        rowdefs = ["*"; "*"; "*"], 
                        coldefs = ["*"; "*"; "*"],
                        children = subGrids model dispatch)
                | Won player ->
                    yield View.Label(
                        text = sprintf "%s won the game! Congratulations!!" (currentPlayer player),
                        verticalTextAlignment = TextAlignment.Start, 
                        horizontalTextAlignment = TextAlignment.Center, 
                        fontAttributes = FontAttributes.Bold, 
                        fontSize = 30)
                    yield View.Button(
                        text = "Start again",
                        fontSize = 11,
                        textColor = Color.White, 
                        backgroundColor = Color.Blue, 
                        command = (fun _ -> dispatch NewGame))
                | Tie ->
                    yield View.Label(
                        text = "It's a tie game! Better luck next time..",
                        verticalTextAlignment = TextAlignment.Start, 
                        horizontalTextAlignment = TextAlignment.Center, 
                        fontAttributes = FontAttributes.Bold, 
                        fontSize = 30)
                    yield View.Button(
                        text = "Start again",
                        fontSize = 11,
                        textColor = Color.White, 
                        backgroundColor = Color.Lavender, 
                        command = (fun _ -> dispatch NewGame))
            ]))

    // Note, this declaration is needed if you enable LiveUpdate
    let program = Program.mkProgram init update view

type App () as app = 
    inherit Application ()

    let runner = 
        App.program
#if DEBUG
        |> Program.withConsoleTrace
#endif
        |> XamarinFormsProgram.run app

#if DEBUG
    // Uncomment this line to enable live update in debug mode. 
    // See https://fsprojects.github.io/Fabulous/tools.html for further  instructions.
    //
    //do runner.EnableLiveUpdate()
#endif    

    // Uncomment this code to save the application state to app.Properties using Newtonsoft.Json
    // See https://fsprojects.github.io/Fabulous/models.html for further  instructions.
#if APPSAVE
    let modelId = "model"
    override __.OnSleep() = 

        let json = Newtonsoft.Json.JsonConvert.SerializeObject(runner.CurrentModel)
        Console.WriteLine("OnSleep: saving model into app.Properties, json = {0}", json)

        app.Properties.[modelId] <- json

    override __.OnResume() = 
        Console.WriteLine "OnResume: checking for model in app.Properties"
        try 
            match app.Properties.TryGetValue modelId with
            | true, (:? string as json) -> 

                Console.WriteLine("OnResume: restoring model from app.Properties, json = {0}", json)
                let model = Newtonsoft.Json.JsonConvert.DeserializeObject<App.Model>(json)

                Console.WriteLine("OnResume: restoring model from app.Properties, model = {0}", (sprintf "%0A" model))
                runner.SetCurrentModel (model, Cmd.none)

            | _ -> ()
        with ex -> 
            App.program.onError("Error while restoring model found in app.Properties", ex)

    override this.OnStart() = 
        Console.WriteLine "OnStart: using same logic as OnResume()"
        this.OnResume()
#endif


