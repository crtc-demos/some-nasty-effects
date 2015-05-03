let pi = 4.0 *. atan 1.0

let _ =
  Printf.printf "sintab:\n";
  for i = 0 to 511 do
    let fi = (2.0 *. pi *. (float_of_int i)) /. 256.0 in
    let s = sin fi in
    Printf.printf "\t.byte $%.2x\n" ((int_of_float ((s *. 127.9))) land 255)
  done
