let hexToRgb : string -> int array = [%bs.raw{|
  function hexToRgb(hex) {
    var bigint = parseInt(hex.slice(1), 16);
    var r = (bigint >> 16) & 255;
    var g = (bigint >> 8) & 255;
    var b = bigint & 255;

    return [r, g , b];
  }
|}] 
module Rgba = struct
  type t = int * int * int * float

  let create r g b a =
    match (r, g, b, a) with
      | (r, g, b, a)  when 
        (r <= 255 && r >= 0 && g <= 255 && g >= 0 && b <= 255 && b >= 0 && a <= 1. && a >= 0.)->
          Some (r, g, b, a)
      | _ -> None


  let toString (r, g, b, a) = {j|rgba($(r),$(g),$(b),$(a)|j}
  let fromString color =
    let [|r; g; b;|] = hexToRgb color in
    (r, g, b, 1.)

  let transparentize (r, g, b, a) amount = 
    match (a -. amount) with
      | a when (a <= 1. && a >= 0.) -> (r, g, b, a)
      | _ -> (r, g, b, 0.)

  let black = (0, 0, 0, 1.)
end