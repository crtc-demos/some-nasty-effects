let letter_of_bits n =
  Char.chr (Char.code 'a' + n)

let tap n =
  let b3 = n land 0b10000000
  and b2 = n land 0b00100000
  and b1 = n land 0b00001000
  and b0 = n land 0b00000010 in
  (b3 lsr 4) lor (b2 lsr 3) lor (b1 lsr 2) lor (b0 lsr 1)

let places n =
  let lst = ref [] in
  for c = 0 to 3 do
    lst := tap ((n lsl c) land 0xff) :: !lst
  done;
  !lst

let want' =
  [ 0, 0, 0, 0;
    0, 1, 0, 1;
    1, 1, 1, 1;
    1, 2, 1, 2;
    2, 2, 2, 2;
    2, 3, 2, 3;
    3, 3, 3, 3;
    3, 4, 3, 4;
    4, 4, 4, 4;
    4, 5, 4, 5;
    5, 5, 5, 5;
    5, 6, 5, 6;
    6, 6, 6, 6;
    6, 7, 6, 7;
    7, 7, 7, 7;
    7, 0, 7, 0 ]

let want = [ 0, 0, 0, 0;
             0, 1, 0, 1;
	     1, 1, 1, 1;
	     1, 2, 1, 2;
	     2, 2, 2, 2;
	     2, 3, 2, 3;
	     3, 3, 3, 3;
	     3, 4, 3, 4;
	     4, 4, 4, 4;
	     4, 5, 4, 5;
	     5, 5, 5, 5;
	     5, 0, 5, 0 ]

let rec try_adding wanted_combs mappings =
  let map mappings f t =
    try
      if List.assoc f mappings = t then
        mappings, true
      else
        mappings, false
    with Not_found ->
      (f, t) :: mappings, true in
  match wanted_combs with
    [] -> mappings
  | comb :: combs ->
      let rec attempt n =
     (* Printf.printf "attempt: %d (matched %d)\n" n (List.length mappings);*)
	if n < 256 then begin
	  match places n with
            [p0; p1; p2; p3] ->
	      let w0, w1, w2, w3 = comb in
	      let m, b0 = map mappings p0 w0 in
	      let m, b1 = map m p1 w1 in
	      let m, b2 = map m p2 w2 in
	      let m, b3 = map m p3 w3 in
	      if b0 && b1 && b2 && b3 then begin
	        try
		  try_adding combs m
		with Not_found ->
		  attempt (succ n)
	      end else
	        attempt (succ n)
	  | _ -> failwith "not possible"
	end else
          raise Not_found in
      attempt 0

(*let _ =
  try_adding want []
*)


let _ =
  for i = 0 to 3 do
    for j = 0 to 255 do
      let bits = tap ((j lsl i) land 0xff) in
      for b = 0 to 3 do
        Printf.printf "%s%d " (if ((bits lsr b) land 1) <> 0 then "" else "-")
			      (succ b)
      done;
      Printf.printf "0\n"
    done
  done
      
