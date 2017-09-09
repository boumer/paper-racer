type t = {
  name: string;
  color: Color.Rgba.t;
  history: move list;
} and move = int * int

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
    history = [(0, 0)]
  }

let addMove car move =
  {car with history = move :: car.history}