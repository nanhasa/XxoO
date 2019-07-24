// Copyright 2018-2019 Fabulous contributors. See LICENSE.md for license.
namespace XxoO

open System
open System.Diagnostics
open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms
open XxoO.Domain
open XxoO.GameAPI
open XxoO.AI.Api
open XxoO.NightMode

module App =
    
    let infoApi = GameInfoAPI.infoApi
    let api = gameplayAPI

    type GameMode =
        | SinglePlayer
        | MultiPlayer

    type Model = 
      { gameMode : GameMode
        difficulty : Difficulty
        gameState : Game
        gameStatus : GameStatus
        isMasterPresented : bool
        nightMode : bool
        aiTurn : bool }

    type Msg = 
        | NewGame
        | NewSinglePlayerGame of Difficulty
        | NewDualGame
        | PlayerMove of SubGamePosition * CellPosition
        | IsMasterPresentedChanged of bool
        | NightModeChanged of bool
        | AiMove

    let initModel = { gameMode = SinglePlayer; difficulty = Normal; gameState = api.newGame; gameStatus = InProcess; isMasterPresented = true; nightMode = false; aiTurn = false }

    let init () = initModel, Cmd.none

    let makeMove model subGamePosition cellPosition =
        match model.gameState |> api.playerMove subGamePosition cellPosition with
        | State gameState ->
            Ok { model with gameState = gameState; gameStatus = InProcess }
        | GameWon (gameState, player) ->
            Ok { model with gameState = gameState; gameStatus = Won player }
        | TieGame gameState ->
            Ok { model with gameState = gameState; gameStatus = Tie }
        | InvalidMove gameState ->
            Error (sprintf "Invalid move: SubGamePosition %A - CellPosition %A - gameState %A" subGamePosition cellPosition gameState)

    let playerMove model subGamePosition cellPosition =
        let cmd model = 
            match model.gameStatus with
            | InProcess ->
                match model.gameMode with
                | SinglePlayer -> { model with aiTurn = true }, Cmd.ofMsg AiMove 
                | MultiPlayer -> model, Cmd.none
            | _ -> model, Cmd.none // Game ended

        match makeMove model subGamePosition cellPosition with
        | Ok newModel -> cmd newModel
        | Error msg ->
            Console.WriteLine msg
            model, Cmd.none

    let aiMove model =
        let subGamePos, cellPos = makeAiMove model.difficulty (model.gameState |> infoApi.getGameInfo)
        match makeMove model subGamePos cellPos with
        | Ok newModel -> { newModel with aiTurn = false }, Cmd.none
        | Error msg ->
            Console.WriteLine msg
            model, Cmd.ofMsg AiMove

    let update msg model =
        match msg with
        | NewGame -> { model with gameState = api.newGame; gameStatus = InProcess; isMasterPresented = false }, Cmd.none
        | NewSinglePlayerGame difficulty -> { model with gameMode = SinglePlayer; difficulty = difficulty }, Cmd.ofMsg NewGame
        | NewDualGame -> { model with gameMode = MultiPlayer }, Cmd.ofMsg NewGame
        | PlayerMove (subGamePos, cellPos) -> playerMove model subGamePos cellPos
        | AiMove -> aiMove model
        | IsMasterPresentedChanged b -> { model with isMasterPresented = b }, Cmd.none
        | NightModeChanged b -> { model with nightMode = b }, Cmd.none

    let currentPlayer player =
        match player with
        | PlayerX -> "X"
        | PlayerO -> "O"

    let cellOwner cellStatus =
        match cellStatus with
        | Played PlayerX -> "X"
        | Played PlayerO -> "O"
        | Empty -> ""

    let gridButtons subGamePos model dispatch =
        let cellStatus cellPos = model.gameState |> api.getCell subGamePos cellPos |> fun cell -> cell.status
        cellPositions
        |> List.mapi (fun i cellPos ->
            let status = cellStatus cellPos
            View.Button(
                text = cellOwner status, 
                fontSize = 15,
                textColor = Color.White,
                backgroundColor = cellColor status model.nightMode,
                padding = Thickness 0.1,
                command = (fun _ -> dispatch (PlayerMove (subGamePos, cellPos))),
                isEnabled = not model.aiTurn
            ).GridRow(i / 3).GridColumn(i % 3))

    let gridBackgroundColor subGamePos model =
        match model.gameState.currentSubGame with
        | Some sub when sub = subGamePos -> nightModeSafeGridSelectionBackgroundColor model.nightMode
        | Some _ -> backgroundColor model.nightMode
        | None -> nightModeSafeGridSelectionBackgroundColor model.nightMode

    let subGrids model dispatch =
        cellPositions
        |> List.mapi (fun i pos ->
            match model.gameState |> api.getSubGame pos |> fun sub -> sub.status with
            | Won player ->
                View.BoxView(nightModeSafePlayerColor model.nightMode player, cornerRadius = CornerRadius 10., margin = Thickness 3.).GridRow(i / 3).GridColumn(i % 3)
            | Tie ->
                View.BoxView(Color.DarkGray, cornerRadius = CornerRadius 10., margin = Thickness 3.).GridRow(i / 3).GridColumn(i % 3)
            | InProcess ->
                View.Grid(
                    rowdefs = ["*"; "*"; "*"], 
                    coldefs = ["*"; "*"; "*"],
                    padding = Thickness 3.0,
                    backgroundColor = gridBackgroundColor pos model,
                    children = gridButtons pos model dispatch
                ).GridRow(i / 3).GridColumn(i % 3))

    let view (model : Model) dispatch =
        View.MasterDetailPage(
            useSafeArea = true,
            masterBehavior = MasterBehavior.Popover,
            isPresented = model.isMasterPresented,
            isPresentedChanged = (fun b -> dispatch (IsMasterPresentedChanged b)),
            backgroundColor = backgroundColor model.nightMode,
            master = 
                View.ContentPage(
                    title = "Settings",
                    useSafeArea = true,
                    backgroundColor = backgroundColor model.nightMode,
                    content = View.StackLayout(
                        children = [
                            View.Label(
                                text = "Rules",
                                margin = Thickness 6.,
                                textColor = nightModeSafeTextColor model.nightMode,
                                verticalTextAlignment = TextAlignment.Start, 
                                horizontalTextAlignment = TextAlignment.Start, 
                                fontAttributes = FontAttributes.Bold, 
                                fontSize = 20)

                            View.Label(
                                text = sprintf "The game consists of 3x3 grid (A) with another 3x3 grid (B) inside of each of cell in grid A.",
                                textColor = nightModeSafeTextColor model.nightMode,
                                margin = Thickness 6.,
                                verticalTextAlignment = TextAlignment.Start, 
                                horizontalTextAlignment = TextAlignment.Start, 
                                fontAttributes = FontAttributes.Bold, 
                                fontSize = 13)

                            View.Label(
                                text = "To win the game, player must form a line of three owned cells in grid A. To own grid A cell, player must form a line of three cells in the grid B of that cell in grid A. However, player can only input their mark in grid B which position on grid A matches the position of cell played by previous player in grid B. If the next grid B is already owned, player can choose where ever to put their mark.",
                                textColor = nightModeSafeTextColor model.nightMode,
                                margin = Thickness 6.,
                                verticalTextAlignment = TextAlignment.Start, 
                                horizontalTextAlignment = TextAlignment.Start, 
                                fontAttributes = FontAttributes.Bold, 
                                fontSize = 13)

                            View.Label(
                                text = "Don't worry if this is confusing, the game will highlight next valid B grid(s) with green for you.",
                                textColor = nightModeSafeTextColor model.nightMode,
                                margin = Thickness 6.,
                                verticalTextAlignment = TextAlignment.Start, 
                                horizontalTextAlignment = TextAlignment.Start, 
                                fontAttributes = FontAttributes.Bold, 
                                fontSize = 13)

                            View.TableView(
                                items = [ "Screen", [ View.SwitchCell(
                                                        on = model.nightMode,
                                                        text = "Night mode",
                                                        onChanged = (fun args -> dispatch (NightModeChanged args.Value))) ]])
                                                
                            View.Button(
                                text = "New game vs easy AI", 
                                fontSize = 15,
                                margin = Thickness 2.,
                                textColor = Color.White,
                                backgroundColor = Color.DarkSlateBlue,
                                command = (fun _ -> dispatch (NewSinglePlayerGame Easy)))
                                
                            View.Button(
                                text = "New game vs AI", 
                                fontSize = 15,
                                margin = Thickness 2.,
                                textColor = Color.White,
                                backgroundColor = Color.DarkSlateBlue,
                                command = (fun _ -> dispatch (NewSinglePlayerGame Normal)))

                            View.Button(
                                text = "New game vs human", 
                                fontSize = 15,
                                margin = Thickness 2.,
                                textColor = Color.White,
                                backgroundColor = Color.DarkSlateBlue,
                                command = (fun _ -> dispatch NewDualGame))])),
            detail = 
                View.ContentPage(
                    title = "XxoO",
                    useSafeArea = true,
                    backgroundColor = backgroundColor model.nightMode,
                    content = View.StackLayout(
                      padding = 20.0, 
                      verticalOptions = LayoutOptions.Center,
                      children = [
                          match model.gameStatus with
                          | InProcess ->
                              yield View.Grid(
                                  rowdefs = ["*"],
                                  coldefs = ["*"; "*"; "*"],
                                  backgroundColor = backgroundColor model.nightMode,
                                  minimumHeightRequest = 35.,
                                  children = [
                                    yield View.Label(
                                        text = "Player",
                                        textColor = nightModeSafeTextColor model.nightMode,
                                        margin = Thickness 3.,
                                        verticalTextAlignment = TextAlignment.Start, 
                                        horizontalTextAlignment = TextAlignment.Start, 
                                        fontAttributes = FontAttributes.Bold, 
                                        fontSize = 24).GridRow(0).GridColumn(1)
                                    yield View.Label(
                                        text = currentPlayer model.gameState.player,
                                        textColor = nightModeSafePlayerColor model.nightMode model.gameState.player,
                                        margin = Thickness 3.,
                                        verticalTextAlignment = TextAlignment.Start, 
                                        horizontalTextAlignment = TextAlignment.End, 
                                        fontAttributes = FontAttributes.Bold, 
                                        fontSize = 24).GridRow(0).GridColumn(1) ])

                              yield View.Grid (
                                  rowdefs = ["*"; "*"; "*"], 
                                  coldefs = ["*"; "*"; "*"],
                                  backgroundColor = backgroundColor model.nightMode,
                                  children = subGrids model dispatch)
                          | Won player ->
                              yield View.Label(
                                  text = sprintf "Player %s won the game! Congratulations!!" (currentPlayer player),
                                  textColor = nightModeSafeTextColor model.nightMode,
                                  margin = Thickness 3.,
                                  verticalTextAlignment = TextAlignment.Start, 
                                  horizontalTextAlignment = TextAlignment.Center, 
                                  fontAttributes = FontAttributes.Bold, 
                                  fontSize = 30)
                              yield View.Button(
                                  text = "Start again",
                                  fontSize = 11,
                                  textColor = Color.White, 
                                  backgroundColor = Color.DarkSlateBlue, 
                                  command = (fun _ -> dispatch NewGame))
                          | Tie ->
                              yield View.Label(
                                  text = "It's a tie game! Better luck next time..",
                                  textColor = nightModeSafeTextColor model.nightMode,
                                  margin = Thickness 3.,
                                  verticalTextAlignment = TextAlignment.Start, 
                                  horizontalTextAlignment = TextAlignment.Center, 
                                  fontAttributes = FontAttributes.Bold, 
                                  fontSize = 30)
                              yield View.Button(
                                  text = "Start again",
                                  fontSize = 11,
                                  textColor = Color.White, 
                                  backgroundColor = Color.DarkSlateBlue, 
                                  command = (fun _ -> dispatch NewGame)) ])))
        
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


