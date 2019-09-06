// Copyright 2018-2019 Fabulous contributors. See LICENSE.md for license.
namespace XxoODomain

open System
open System.Threading
open Fabulous
open Fabulous.XamarinForms
open Xamarin.Forms
open XxoODomain.Domain
open XxoODomain.GameAPI
open XxoODomain.AI.Api
open XxoODomain.NightMode

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
        aiMoveErrors : int
        aiErrorMsg : string
        acceptedEULA : bool }

    type Msg = 
        | NewGame
        | NewSinglePlayerGame of Difficulty
        | NewDualGame
        | PlayerMove of SubGamePosition * CellPosition
        | IsMasterPresentedChanged of bool
        | NightModeChanged of bool
        | AiMove
        | AcceptedEULA

    let initModel = { gameMode = SinglePlayer; difficulty = Normal; gameState = api.newGame; gameStatus = InProcess; isMasterPresented = true; nightMode = false; aiMoveErrors = 0; aiErrorMsg = ""; acceptedEULA = false }

    let init () = initModel, Cmd.none

    let aiTurn model =
        match model.gameMode with
        | SinglePlayer ->
            match model.gameState.player with
            | PlayerX -> false
            | PlayerO -> true
        | MultiPlayer -> false

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
        let nextCmd model = 
            match model.gameStatus with
            | InProcess ->
                match model.gameMode with
                | SinglePlayer -> Cmd.ofMsg AiMove 
                | MultiPlayer -> Cmd.none
            | _ -> Cmd.none // Game ended

        match makeMove model subGamePosition cellPosition with
        | Ok newModel -> newModel, nextCmd newModel
        | Error msg ->
            Console.WriteLine msg
            model, if aiTurn model then Cmd.ofMsg AiMove else Cmd.none 

    let aiMove model =
        if model.aiMoveErrors > 20 
        then { model with aiErrorMsg = "Something unexpected happened, please start a new game" }, Cmd.none
        else 
            try
                let moveResult = 
                    makeAiMove model.difficulty (model.gameState |> infoApi.getGameInfo) 
                    |> Result.bind (fun (subPos, cellPos) -> makeMove model subPos cellPos)

                match moveResult with
                | Ok newModel -> 
                    Thread.Sleep 500
                    { newModel with aiMoveErrors = 0 }, Cmd.none
                | Error msg ->
                    Console.WriteLine msg
                    { model with aiMoveErrors = model.aiMoveErrors + 1 }, Cmd.ofMsg AiMove
            with
            | ex -> 
                Console.WriteLine ex.Message
                { model with aiMoveErrors = model.aiMoveErrors + 1 }, Cmd.ofMsg AiMove

    let update msg model =
        match msg with
        | NewGame -> { model with gameState = api.newGame; gameStatus = InProcess; isMasterPresented = false; aiMoveErrors = 0; aiErrorMsg = "" }, Cmd.none
        | NewSinglePlayerGame difficulty -> { model with gameMode = SinglePlayer; difficulty = difficulty }, Cmd.ofMsg NewGame
        | NewDualGame -> { model with gameMode = MultiPlayer }, Cmd.ofMsg NewGame
        | PlayerMove (subGamePos, cellPos) -> playerMove model subGamePos cellPos
        | AiMove -> aiMove model
        | IsMasterPresentedChanged b -> { model with isMasterPresented = b }, Cmd.none
        | NightModeChanged b -> { model with nightMode = b }, Cmd.none
        | AcceptedEULA -> { model with acceptedEULA = true }, Cmd.none

    let currentPlayer player =
        match player with
        | PlayerX -> "X"
        | PlayerO -> "O"

    let cellOwner cellStatus =
        match cellStatus with
        | Played PlayerX -> "X"
        | Played PlayerO -> "O"
        | Empty -> ""

    let winningText gameMode player = 
        match gameMode with
        | SinglePlayer ->
            match player with
            | PlayerX -> "You won the game! Congratulations!!"
            | PlayerO -> "You lost the game. Better luck next time!"
        | MultiPlayer ->
            sprintf "Player %s won the game! Congratulations!!" (currentPlayer player)

    let playerTurnLabel model =
        match model.gameMode with
        | SinglePlayer ->
            match model.gameState.player with
            | PlayerX ->
                View.Label(
                    text = "Your turn",
                    textColor = nightModeSafeTextColor model.nightMode,
                    margin = Thickness 4.,
                    verticalTextAlignment = TextAlignment.Center, 
                    horizontalTextAlignment = TextAlignment.Center, 
                    fontAttributes = FontAttributes.Bold, 
                    fontSize = 38)
            | PlayerO ->
                View.Label(
                    text = "AI turn",
                    textColor = nightModeSafeTextColor model.nightMode,
                    margin = Thickness 4.,
                    verticalTextAlignment = TextAlignment.Center, 
                    horizontalTextAlignment = TextAlignment.Center, 
                    fontAttributes = FontAttributes.Bold, 
                    fontSize = 38)
        | MultiPlayer ->
            View.Grid(
                rowdefs = ["*"],
                coldefs = ["*"; "*"; "*"; "*"; "*"; "*"; "*"],
                backgroundColor = backgroundColor model.nightMode,
                minimumHeightRequest = 35.,
                children = [
                    yield View.Label(
                        text = "Player",
                        textColor = nightModeSafeTextColor model.nightMode,
                        margin = Thickness 4.,
                        verticalTextAlignment = TextAlignment.Center, 
                        horizontalTextAlignment = TextAlignment.End, 
                        fontAttributes = FontAttributes.Bold, 
                        fontSize = 38).GridRow(0).GridColumn(0).GridColumnSpan(4)
                    yield View.Label(
                        text = currentPlayer model.gameState.player,
                        textColor = nightModeSafePlayerColor model.nightMode model.gameState.player,
                        margin = Thickness 4.,
                        verticalTextAlignment = TextAlignment.Center, 
                        horizontalTextAlignment = TextAlignment.End, 
                        fontAttributes = FontAttributes.Bold, 
                        fontSize = 38).GridRow(0).GridColumn(4) ])

    let isButtonEnabled model subGamePos =
        if aiTurn model 
        then false
        else
            model.gameState.currentSubGame 
            |> Option.map (fun subPos -> subPos = subGamePos) 
            |> Option.defaultValue true

    let gridButtons subGamePos model dispatch =
        let cellStatus cellPos = model.gameState |> api.getCell subGamePos cellPos |> fun cell -> cell.status
        let isEnabled = isButtonEnabled model subGamePos
        cellPositions
        |> List.mapi (fun i cellPos ->
            let status = cellStatus cellPos
            View.Button(
                //text = cellOwner status, This bugs every now and then for some reason
                fontSize = 15,
                textColor = Color.White,
                backgroundColor = cellColor status model.nightMode,
                padding = Thickness 0.1,
                command = (fun _ -> dispatch (PlayerMove (subGamePos, cellPos))),
                isEnabled = isEnabled,
                canExecute = isEnabled
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
        if model.acceptedEULA |> not then
            View.ContentPage(
                title = "Settings",
                useSafeArea = true,
                backgroundColor = backgroundColor model.nightMode,
                content = View.StackLayout(
                    children = [
                        yield View.Label(
                            text = "End-user license agreement (EULA)",
                            margin = Thickness 6.,
                            textColor = nightModeSafeTextColor model.nightMode,
                            verticalTextAlignment = TextAlignment.Start, 
                            horizontalTextAlignment = TextAlignment.Start, 
                            fontAttributes = FontAttributes.Bold, 
                            fontSize = 24)

                        let nl = Environment.NewLine
                        yield View.Label(
                            text = sprintf "Copyright 2019 Sampo Siltanen%s%sAll rights reserved.%s%sNot liable for any damage incurred from use of this software including but not limited to: monetary loss, loss of data and or device malfunction." nl nl nl nl,
                            textColor = nightModeSafeTextColor model.nightMode,
                            margin = Thickness 6.,
                            verticalTextAlignment = TextAlignment.Start, 
                            horizontalTextAlignment = TextAlignment.Start, 
                            fontAttributes = FontAttributes.Bold, 
                            fontSize = 18)
                            
                        yield View.Button(
                            text = "Accept", 
                            fontSize = 15,
                            margin = Thickness 2.,
                            textColor = Color.White,
                            backgroundColor = Color.DarkSlateBlue,
                            command = (fun _ -> dispatch AcceptedEULA)) ]))
        else
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
                                    text = Rules.rules,
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
                                  yield playerTurnLabel model

                                  yield View.Grid (
                                      rowdefs = ["*"; "*"; "*"], 
                                      coldefs = ["*"; "*"; "*"],
                                      backgroundColor = backgroundColor model.nightMode,
                                      children = subGrids model dispatch)
                              
                                  // Present error message if AI derped
                                  if String.IsNullOrEmpty model.aiErrorMsg
                                  then yield View.Label(
                                          text = model.aiErrorMsg,
                                          textColor = nightModeSafeTextColor model.nightMode,
                                          margin = Thickness 6.,
                                          verticalTextAlignment = TextAlignment.Center, 
                                          horizontalTextAlignment = TextAlignment.Center, 
                                          fontAttributes = FontAttributes.Bold, 
                                          fontSize = 28)
                              | Won player ->
                                  yield View.Label(
                                      text = winningText model.gameMode player,
                                      textColor = nightModeSafeTextColor model.nightMode,
                                      margin = Thickness 3.,
                                      verticalTextAlignment = TextAlignment.Start, 
                                      horizontalTextAlignment = TextAlignment.Center, 
                                      fontAttributes = FontAttributes.Bold, 
                                      fontSize = 30)
                                  yield View.Button(
                                      text = "Play again",
                                      fontSize = 11,
                                      textColor = Color.White, 
                                      backgroundColor = Color.DarkSlateBlue, 
                                      command = (fun _ -> dispatch NewGame))
                              | Tie ->
                                  yield View.Label(
                                      text = "It's a tie game! Better luck next time!",
                                      textColor = nightModeSafeTextColor model.nightMode,
                                      margin = Thickness 3.,
                                      verticalTextAlignment = TextAlignment.Start, 
                                      horizontalTextAlignment = TextAlignment.Center, 
                                      fontAttributes = FontAttributes.Bold, 
                                      fontSize = 30)
                                  yield View.Button(
                                      text = "Play again",
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


