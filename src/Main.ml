open Tea.App

open Tea.Html

type move = int * int

type car = {
  name: string;
  color: string;
  history: move list;
}

type model = {
  cars: car list;
  terrain: int;
}

type msg =
  | AddCar of car
  | MoveCar of string * move
  [@@bs.deriving {accessors}] 

let init () = 
  let model = {
    cars = [
      {name = "1"; color = "green"; history = [(3, 1); (1, 0); (0, 0)]};
      {name = "2"; color = "red"; history = [(8, 12); (5, 11); (2, 10); (0, 10)]};
    ];
    terrain = 0;
  } in
  (model, Tea.Cmd.none)

let update model = function 
  | AddCar car -> ({model with cars = car :: model.cars}, Tea.Cmd.none)
  | MoveCar (name, move) ->
    let cars = List.map (fun car -> if car.name == name then {car with history = move :: car.history} else car) model.cars in
    ({model with cars}, Tea.Cmd.none)


let subscriptions _ = Tea.Sub.none

let viewCars cars = 
  let module Svg = Tea.Svg in
  let module SvgA = Tea.Svg.Attributes in
  let (@$) a b =  a (string_of_int b) in
  let scale = 40 in
  let scaler (x, y) = (x * scale, y * scale) in
  let viewCar car =
    let points = 
      [5; 0; 0; 10; 10; 10]
      |> List.map (fun x -> string_of_int (x * scale / 10))
      |> String.concat " "
    in
    let rotation =
      match car.history with
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
    let (xNext, yNext) =
      match car.history with
        | [] -> (0, 0)
        | (x,y) :: [] -> (x + 1, y)
        | (xLatest,yLatest) :: (xPrevious, yPrevious) :: _ -> (xLatest + (xLatest - xPrevious), yLatest + (yLatest - yPrevious))
    in
    let circles = 
      [
        (-1, -1); (0, -1); (1, -1);
        (-1,  0); (0,  0); (1,  0);
        (-1,  1); (0,  1); (1,  1);
      ]
      |> List.map (fun (x, y) -> x + xNext, y + yNext)
      |> List.map scaler
      |> List.map (fun (x, y) -> 
        Svg.circle [SvgA.cx @$ x; SvgA.cy @$ y; SvgA.r @$ (scale / 5); SvgA.fill "rgba(0, 0, 255, 0.25)"] []
      )
    in
    circles
  in
  let viewHistory car =
    let rec pathFromMoves path moves =
      match moves with
        | [] -> path
        | (x, y) :: [] -> {j|$(path) $(x) $(y)|j}
        | (x, y) :: tl -> pathFromMoves {j|$(path) $(x) $(y) L|j} tl
    in
    let path = 
      List.rev_map scaler car.history
      |> pathFromMoves "M"
      |> (fun path -> Svg.path [SvgA.d path; SvgA.fill "none"; SvgA.stroke "black"; SvgA.strokeWidth "1"] [])
    in
    let circles = 
      List.map (fun (x, y) -> Svg.circle [SvgA.cx @$ (x * scale); SvgA.cy @$ (y * scale); SvgA.r @$ (scale /5); SvgA.fill "black"] []) car.history
    in
    (path :: circles)
  in
  let center = scale /2 in
  Svg.svg [SvgA.width "600"; SvgA.height "600"] [
    Svg.defs [] [
      Svg.pattern [SvgA.id "grid"; SvgA.width @$ scale; SvgA.height @$ scale; SvgA.patternUnits "userSpaceOnUse"] [
        Svg.rect [SvgA.width @$ scale; SvgA.height @$ scale;] [];
        Svg.path [SvgA.d {j|M 0 0 H $(scale) V $(scale) H 0|j}; SvgA.fill "white"; SvgA.stroke "gray"; SvgA.strokeWidth "0.5"] []
      ]
    ];
    Svg.rect [SvgA.width "100%"; SvgA.height "100%"; SvgA.fill "url(#grid)"] [];
    Svg.g [] (List.map viewCar cars);
    Svg.g [SvgA.transform {j|translate($(center),$(center))|j} ] 
      ((List.map viewHistory cars |> List.flatten) @ (List.map viewPossibleMoves cars |> List.flatten));
  ]


let view model =
  div [] [
    viewCars model.cars;
  ]

let main =
  standardProgram { 
    init;
    update;
    subscriptions;
    view;
  }