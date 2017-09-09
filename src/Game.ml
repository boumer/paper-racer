open Tea.Html

open Color
open Car

type model = {
  cars: Car.t list;
  terrain: int;
  currentPlayer: Car.t;
}

type msg =
  | MoveCar of string * move
  [@@bs.deriving {accessors}] 

let startGame cars =
  match cars with
    | [] -> None
    | hd :: _ -> 
      Some {
        cars;
        terrain = 0;
        currentPlayer = hd;
      }


let update model = function 
| MoveCar (name, move) ->
    let cars = List.map (fun car -> if car.Car.name == name then Car.addMove car move else car) model.cars in
    let currentPlayer = List.filter (fun car -> car != model.currentPlayer) model.cars |> List.hd in
    ({model with cars; currentPlayer}, Tea.Cmd.none)



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
      Svg.polygon [SvgA.points points; SvgA.fill (Rgba.toString car.color)] [];
    ]

  in
  let viewPossibleMoves car =
    let dotColor = Rgba.transparentize car.Car.color 0.5 |> Rgba.toString in
    Car.possibleNextMoves car
    |> List.map scaler
    |> List.mapi (fun i (x, y) -> 
      [
        Svg.circle [SvgA.cx @$ x; SvgA.cy @$ y; SvgA.r @$ (scale / 5); SvgA.fill dotColor] [];
        Svg.text' [SvgA.x @$ x; SvgA.y @$ y; SvgA.textAnchor "middle"] [Svg.text @$ i + 1]
      ]
    )
    |> List.flatten
  in
  let viewHistory car =
    let traceColor = Rgba.transparentize car.Car.color 0.5 |> Rgba.toString in
    let dotColor = Rgba.transparentize car.Car.color 0.2 |> Rgba.toString in
    let rec pathFromMoves path moves =
      Js.log moves;
      match moves with
        | [] -> path
        | (x, y) :: [] -> {j|$(path) $(x) $(y)|j}
        | (x, y) :: tl -> pathFromMoves {j|$(path) $(x) $(y) L|j} tl
    in
    let path = 
      List.rev_map scaler car.Car.history
      |> pathFromMoves "M"
      |> (fun path -> Svg.path [SvgA.d path; SvgA.fill "none"; SvgA.stroke traceColor; SvgA.strokeWidth "1"] [])
    in
    let circles = 
      List.map (fun (x, y) -> Svg.circle [SvgA.cx @$ (x * scale); SvgA.cy @$ (y * scale); SvgA.r @$ (scale /5); SvgA.fill dotColor] []) car.history
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
  let background = Rgba.transparentize car.Car.color 0.5 |> Rgba.toString in
  let buttons = 
    Car.possibleNextMoves car
    |> List.mapi (fun i move -> button [onClick (moveCar car.Car.name move); style "background" background] [text (string_of_int (i + 1))])
  in
  div [class' "button-grid"] buttons

let view model =
  let name = model.currentPlayer.Car.name in
  main [] [
    viewCars model.cars model.currentPlayer;
    div [class' "controls"] [
      div [] [text {j|$(name) has to move|j}];
      viewButtons model.currentPlayer;
    ]
  ]