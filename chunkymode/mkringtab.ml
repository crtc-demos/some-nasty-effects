let pi = 4.0 *. atan 1.0

let _ =
  Printf.printf "rings:\n";
  for y = 0 to 31 do
    for x = 0 to 255 do
      let xb = float_of_int x -. 127.5
      and yb = float_of_int y -. 31.5 in
      let xf = ((xb /. 255.0) *. 8.0)
      and yf = ((yb /. 63.0) *. 2.0) *. 1.738095 in
      let dist = sqrt (xf *. xf +. yf *. yf) in
      let pt = sin (dist *. 2.0 *. pi /. 2.0) in
      Printf.printf "\t.byte $%.2x\n" ((int_of_float ((pt *. 63.999))) land 255)
    done
  done
