open Tea.App
open Color

let (!) a = (a, Tea.Cmd.none)

type page =
  | WaitingForPlayersPage of waitingModel
  | PlayingPage of Game.model
and waitingModel = {
  players: Car.t list;
  error: string Js.Option.t;
  playerName: string;
}

type model = {
  currentPage: page;
}

let init () = 
  let cars = [] in
  ! {currentPage = WaitingForPlayersPage {players = cars; error = None; playerName = ""}}

type msg =
  | StartGame 
  | PlayingPageMsg of Game.msg
  | InitPageMsg of initPageMsg
and initPageMsg =
  | UpdateName of string
  | CreatePlayer 

  [@@bs.deriving {accessors}] 

let initUpdate model msg =
  match msg with
    | UpdateName name ->
      {model with playerName = name}
    | CreatePlayer ->
      let blue = Rgba.create 0 0 255 1. |> Js.Option.default Rgba.black in
      let player = Car.createCar model.playerName blue in
      {model with players = player :: model.players; playerName = ""}

let update model msg =
  match (msg, model.currentPage) with
    | (InitPageMsg msg', WaitingForPlayersPage waitingModel) ->
      let waitingModel = initUpdate waitingModel msg' in
      ! {model with currentPage =WaitingForPlayersPage waitingModel}
    | (StartGame, WaitingForPlayersPage waitingModel) ->
        begin match Game.startGame waitingModel.players with
          | Some playingState ->
            ! {currentPage = PlayingPage playingState}
          | None ->
            let withError = {waitingModel with error = Some "Not enough players to start"} in
            ! {currentPage = WaitingForPlayersPage withError}
        end
    | (PlayingPageMsg playingPageMsg, PlayingPage playingState) -> 
        let (playingState, _cmd) = Game.update playingState playingPageMsg in
      ! {currentPage = PlayingPage playingState}
    | _ -> ! model


let viewWaiting map waitingModel =
  let open Tea.Html in
  let startButton =
    match waitingModel.players with
      | [] -> noNode
      | _ -> button [onClick startGame] [text "Start game"];
  in
  match waitingModel.error with
    | Some error ->
      div [] [
        text error
      ]
    | None ->
      div [] [
        div [] [
          label [] [text "Name"];
          input' [type' "text"; onChange (fun x -> map (updateName x)); value waitingModel.playerName] [];
          button [onClick (map createPlayer)] [text ("Create: " ^ waitingModel.playerName)]
        ];
        div [] [
          text "Players:";
          ul [] (List.map (fun player -> li [] [text player.Car.name]) waitingModel.players)
        ];
        startButton
      ]

let view model =
  match model.currentPage with
    | WaitingForPlayersPage cars ->
      (viewWaiting initPageMsg cars)
    | PlayingPage playingState ->
      (Tea.App.map playingPageMsg (Game.view playingState))

let subscriptions _ = Tea.Sub.none

let main =
  standardProgram { 
    init;
    update;
    subscriptions;
    view;
  }