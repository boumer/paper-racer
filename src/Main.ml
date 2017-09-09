open Tea.App
open Color

let (!) a = (a, Tea.Cmd.none)

type page =
  | WaitingForPlayersPage of waitingModel
  | PlayingPage of Game.model
and waitingModel = {
  players: Car.t list;
  error: string Js.Option.t;
}

type model = {
  currentPage: page;
}

let init () = 
  let blue = Rgba.create 0 0 255 1. |> Js.Option.default Rgba.black in
  let green = Rgba.create 0 255 255 1. |> Js.Option.default Rgba.black in
  let cars = [ Car.createCar "Thomas" blue; Car.createCar "Michel" green] in
  ! {currentPage = WaitingForPlayersPage {players = cars; error = None}}

type msg =
  | StartGame 
  | PlayingPageMsg of Game.msg
  [@@bs.deriving {accessors}] 

let update model msg =
  match (msg, model.currentPage) with
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


let viewWaiting waitingModel =
  let open Tea.Html in
  match waitingModel.error with
    | Some error ->
      div [] [
        text error
      ]
    | None ->
      div [] [
        text "Waiting for players";
        button [onClick StartGame] [text "Start game"];
      ]

let viewPlaying _ =
  let open Tea.Html in
    div [] [text "TODO"]

let view model =
  match model.currentPage with
    | WaitingForPlayersPage cars ->
      viewWaiting cars
    | PlayingPage playingState ->
      Tea.App.map playingPageMsg (Game.view playingState)

let subscriptions _ = Tea.Sub.none

let main =
  standardProgram { 
    init;
    update;
    subscriptions;
    view;
  }