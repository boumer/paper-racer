open Tea.App

open Tea.Html

type move = int * int

module Car = struct
  type t = {
    name: string;
    color: string;
    history: move list;
  }
  
  let possibleNextMoves car =
    let (xNext, yNext) =
      match car.history with
        | [] -> (0, 0)
        | (x,y) :: [] -> (x + 1, y)
        | (xLatest,yLatest) :: (xPrevious, yPrevious) :: _ -> (xLatest + (xLatest - xPrevious), yLatest + (yLatest - yPrevious))
    in
    [
      (-1, -1); (0, -1); (1, -1);
      (-1,  0); (0,  0); (1,  0);
      (-1,  1); (0,  1); (1,  1);
    ]
    |> List.map (fun (x, y) -> x + xNext, y + yNext)
  
  let createCar name color =
    {
      name;
      color;
      history = []
    }

  let addMove car move =
    {car with history = move :: car.history}
end

type model = {
  cars: Car.t list;
  terrain: int;
  currentPlayer: Car.t;
}

type msg =
  | MoveCar of string * move
  [@@bs.deriving {accessors}] 

let init () = 
  let cars = [ Car.createCar "Thomas" "green"; Car.createCar "Michel" "blue"] in
  let model = {
    cars;
    terrain = 0;
    currentPlayer = List.hd cars;
  } in
  (model, Tea.Cmd.none)

let update model = function 
  | MoveCar (name, move) ->
    let cars = List.map (fun car -> if car.Car.name == name then Car.addMove car move else car) model.cars in
    let currentPlayer = List.filter (fun car -> car != model.currentPlayer) model.cars |> List.hd in
    ({model with cars; currentPlayer}, Tea.Cmd.none)


let subscriptions _ = Tea.Sub.none

let viewCars cars currentPlayer = 
  let module Svg = Tea.Svg in
  let module SvgA = Tea.Svg.Attributes in
  let (@$) a b =  a (string_of_int b) in
  let scale = 30 in
  let scaler (x, y) = (x * scale, y * scale) in
  let viewCar car =
    let points = 
      [5; 0; 0; 10; 10; 10]
      |> List.map (fun x -> string_of_int (x * scale / 10))
      |> String.concat " "
    in
    let rotation =
      match car.Car.history with
        | (xLast, yLast) :: (xPrevious, yPrevious) :: _ ->
          Js.Math.atan2 ~y:(float_of_int (yLast - yPrevious)) ~x:(float_of_int (xLast - xPrevious)) ()
          |> (fun x -> x *. 180. /. Js.Math._PI +. 90.)
        | _ -> 0.
    in
    let (xTranslate, yTranslate) =
      match car.history with
        | (xLast, yLast) :: _ -> scaler (xLast, yLast)
        | _ -> (0, 0)
    in
    Svg.g [ SvgA.transform {j|translate($(xTranslate),$(yTranslate)) rotate($(rotation),20,20) |j} ] [
      Svg.polygon [SvgA.points points; SvgA.fill car.color] [];
    ]

  in
  let viewPossibleMoves car =
    Car.possibleNextMoves car
    |> List.map scaler
    |> List.map (fun (x, y) -> 
      Svg.circle [SvgA.cx @$ x; SvgA.cy @$ y; SvgA.r @$ (scale / 5); SvgA.fill "rgba(0, 0, 255, 0.25)"] []
    )
  in
  let viewHistory car =
    let rec pathFromMoves path moves =
      match moves with
        | [] -> path
        | (x, y) :: [] -> {j|$(path) $(x) $(y)|j}
        | (x, y) :: tl -> pathFromMoves {j|$(path) $(x) $(y) L|j} tl
    in
    let path = 
      List.rev_map scaler car.Car.history
      |> pathFromMoves "M"
      |> (fun path -> Svg.path [SvgA.d path; SvgA.fill "none"; SvgA.stroke "black"; SvgA.strokeWidth "1"] [])
    in
    let circles = 
      List.map (fun (x, y) -> Svg.circle [SvgA.cx @$ (x * scale); SvgA.cy @$ (y * scale); SvgA.r @$ (scale /5); SvgA.fill "black"] []) car.history
    in
    (path :: circles)
  in
  let center = scale /2 in
  Svg.svg [SvgA.width "800"; SvgA.height "800"] [
    Svg.defs [] [
      Svg.pattern [SvgA.id "grid"; SvgA.width @$ scale; SvgA.height @$ scale; SvgA.patternUnits "userSpaceOnUse"] [
        Svg.rect [SvgA.width @$ scale; SvgA.height @$ scale;] [];
        Svg.path [SvgA.d {j|M 0 0 H $(scale) V $(scale) H 0|j}; SvgA.fill "white"; SvgA.stroke "gray"; SvgA.strokeWidth "0.5"] []
      ]
    ];
    Svg.rect [SvgA.width "100%"; SvgA.height "100%"; SvgA.fill "url(#grid)"] [];
    Svg.g [] (List.map viewCar cars);
    Svg.g [SvgA.transform {j|translate($(center),$(center))|j} ] 
      ((List.map viewHistory cars |> List.flatten) @ (viewPossibleMoves currentPlayer));
  ]

let viewButtons car =
  let buttons = 
    Car.possibleNextMoves car
    |> List.mapi (fun i move -> button [onClick (moveCar car.Car.name move)] [text (string_of_int i)])
  in
  div [] buttons

let view model =
  let name = model.currentPlayer.Car.name in
  div [] [
    viewCars model.cars model.currentPlayer;
    div [] [text {j|$(name) has to move|j}];
    viewButtons model.currentPlayer;
  ]

let main =
  standardProgram { 
    init;
    update;
    subscriptions;
    view;
  }