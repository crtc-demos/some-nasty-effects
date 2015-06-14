module PixelPairSet = Set.Make (struct
  type t = int * int * int * int * int * int * int * int
  let compare = compare
end)

open Z3

let rainbow_palette =
  [ 0,0,0,0;
    0,1,0,1;
    1,1,1,1;
    1,3,1,3;
    3,3,3,3;
    2,3,2,3;
    2,2,2,2;
    6,2,6,2;
    6,6,6,6;
    6,6,6,4;
    6,4,6,4;
    6,4,5,4;
    5,4,1,4;
    4,4,4,4;
    4,0,4,0;
    0,0,0,0
  ]

let chunksize = 2
let change_per_row = 9

let pal_bit ctx pixels itype bv4type bitno entry =
  let rec build_operands tap bitno lst =
    if bitno = 4 then
      lst
    else
      let new_op =
	if tap >= 0 then
	  BitVector.mk_extract ctx tap tap (Z3Array.mk_select ctx pixels entry)
	else
	  BitVector.mk_numeral ctx "1" 1 in
      build_operands (tap - 2) (succ bitno) (new_op::lst) in
  let rec concat_list = function
    [] -> failwith "Not enough things to concat"
  | [a] -> a
  | a::rest -> BitVector.mk_concat ctx a (concat_list rest) in
  concat_list (List.rev (build_operands (7 - bitno) 0 []))

let lookup_cols palette byte =
  let rec tap byte =
    let entry = ((byte land 128) lsr 4)
		lor ((byte land 32) lsr 3)
		lor ((byte land 8) lsr 2)
		lor ((byte land 2) lsr 1) in
    palette.(entry) in
  let a, byte = tap byte, ((byte lsl 1) lor 1) land 255 in
  let b, byte = tap byte, ((byte lsl 1) lor 1) land 255 in
  let c, byte = tap byte, ((byte lsl 1) lor 1) land 255 in
  let d = tap byte in
  (a, b, c, d)

let col_to_rgb col =
  ((col land 1) * 255),
  (((col land 2) lsr 1) * 255),
  (((col land 4) lsr 2) * 255)

let rgb_dist (r1, g1, b1) (r2, g2, b2) =
  let rdist = sqrt (float_of_int ((r2 - r1) * (r2 - r1)))
  and gdist = sqrt (float_of_int ((g2 - g1) * (g2 - g1)))
  and bdist = sqrt (float_of_int ((b2 - b1) * (b2 - b1))) in
  0.2126 *. rdist +. 0.7152 *. gdist +. 0.0722 *. bdist

let calc_distance col1 col2 =
  let p1, p2, p3, p4 = col1
  and q1, q2, q3, q4 = col2 in
  rgb_dist (col_to_rgb p1) (col_to_rgb q1)
  +. rgb_dist (col_to_rgb p2) (col_to_rgb q2)
  +. rgb_dist (col_to_rgb p3) (col_to_rgb q3)
  +. rgb_dist (col_to_rgb p4) (col_to_rgb q4)

let constrain_previous_palette ctx solver pal prev_pal =
  let mkint n = Arithmetic.Integer.mk_numeral_i ctx n
  and mkbvint n = BitVector.mk_numeral ctx (string_of_int n) 4 in
  let i0 = mkint 0 and i1 = mkint 1 in
  let _, oplist = Array.fold_left
    (fun (idx, oplist) ent ->
      let op =
	Boolean.mk_ite ctx
	  (Boolean.mk_eq ctx
	    (Z3Array.mk_select ctx pal (mkbvint idx))
	    (mkbvint ent))
	  i1 i0 in
      (succ idx, op::oplist))
    (0, [])
    prev_pal in
  let sum = Arithmetic.mk_add ctx oplist in
  Solver.add solver [Arithmetic.mk_ge ctx sum (mkint (16 - change_per_row))]

let find_palette pixel_bytes all_colours cols_histo badness_out
		 previous_palette =
  let ctx = mk_context ["model", "true"; "proof", "false"] in
  let mkint n = Arithmetic.Integer.mk_numeral_i ctx n
  and mkbvint n = BitVector.mk_numeral ctx (string_of_int n) 4 in
  let itype = Arithmetic.Integer.mk_sort ctx
  and bv4type = BitVector.mk_sort ctx 4
  and bv8type = BitVector.mk_sort ctx 8 in
  let pal = Z3Array.mk_const_s ctx "pal" bv4type bv4type in
  let pixels = Z3Array.mk_const_s ctx "pixels" itype bv8type in
  let solver = Solver.mk_solver_s ctx "smt" in
  ignore (List.fold_right
    (fun (c0, c1, c2, c3) num ->
      let num_exp = mkint num in
      let p0_idx = pal_bit ctx pixels itype bv4type 0 num_exp in
      let p1_idx = pal_bit ctx pixels itype bv4type 1 num_exp in
      let p2_idx = pal_bit ctx pixels itype bv4type 2 num_exp in
      let p3_idx = pal_bit ctx pixels itype bv4type 3 num_exp in
      let p0 = Z3Array.mk_select ctx pal p0_idx in
      let p1 = Z3Array.mk_select ctx pal p1_idx in
      let p2 = Z3Array.mk_select ctx pal p2_idx in
      let p3 = Z3Array.mk_select ctx pal p3_idx in
      Solver.add solver [Boolean.mk_eq ctx p0 (mkbvint c0);
			 Boolean.mk_eq ctx p1 (mkbvint c1);
			 Boolean.mk_eq ctx p2 (mkbvint c2);
			 Boolean.mk_eq ctx p3 (mkbvint c3)];
      succ num)
    pixel_bytes
    0);
  begin match previous_palette with
    None -> ()
  | Some prev_pal -> constrain_previous_palette ctx solver pal prev_pal
  end;
  let status = Solver.check solver [] in
  match status with
    Solver.SATISFIABLE ->
      let ht_matched = Hashtbl.create 10
      and ht_besteffort = Hashtbl.create 10 in
      begin match Solver.get_model solver with
	Some model ->
	  let palette = Array.make 16 0 in
	  for i = 0 to 15 do
	    let pal_exp = Z3Array.mk_select ctx pal (mkbvint i) in
	    match Model.eval model pal_exp true with
	      Some num ->
		let ival = BitVector.get_int num in
		(*Printf.printf "  palette entry %d: %x\n" i ival;*)
		palette.(i) <- ival
	    | None -> failwith "Palette eval failed"
	  done;
	  let inp_length = List.length pixel_bytes in
	  for i = 0 to inp_length - 1 do
	    let byte_exp = Z3Array.mk_select ctx pixels (mkint i) in
	    match Model.eval model byte_exp true with
	      Some num ->
		let ival = BitVector.get_int num in
		let cols = lookup_cols palette ival in
		(*Printf.printf "  byte val %d: %.2x\n" i ival;*)
		Hashtbl.add ht_matched cols ival
	    | None -> failwith "Byte eval failed"
	  done;
	  let total_length = Array.length all_colours in
	  let unmatched_length = total_length - inp_length in
	  if unmatched_length > 0 then begin
	    for i = inp_length to total_length - 1 do
	      let orig_cols = all_colours.(i) in
	      let min_dist = ref infinity
	      and min_cols = ref (-1, -1, -1, -1)
	      and min_chk = ref (-1) in
	      for chk = 0 to 255 do
	        let these_cols = lookup_cols palette chk in
		let dist = calc_distance orig_cols these_cols in
		if dist < !min_dist then begin
		  min_dist := dist;
		  min_cols := these_cols;
		  min_chk := chk
		end
	      done;
	      match !min_cols, orig_cols with
	        (a1, a2, a3, a4), (a5, a6, a7, a8) ->
		  (*Printf.printf
		    "  byte val %d: %.2x, using (%d,%d,%d,%d) for \
		     (%d,%d,%d,%d) [* %d, badness %f]\n"
		    i !min_chk a1 a2 a3 a4 a5 a6 a7 a8 cols_histo.(i)
		    !min_dist;*)
		  badness_out.(i) <- !min_dist;
		  Hashtbl.add ht_besteffort (a5, a6, a7, a8) !min_chk
	    done
	  end;
	  Some (palette, ht_matched, ht_besteffort)
      | None -> None
      end
  | Solver.UNSATISFIABLE -> Printf.printf "unsat\n"; None
  | Solver.UNKNOWN -> Printf.printf "unknown\n"; None

let find_palette2 pixel_byte_alts previous_palette =
  let ctx = mk_context ["model", "true"; "proof", "false"] in
  let mkint n = Arithmetic.Integer.mk_numeral_i ctx n
  and mkbvint n = BitVector.mk_numeral ctx (string_of_int n) 4 in
  let itype = Arithmetic.Integer.mk_sort ctx
  and bv4type = BitVector.mk_sort ctx 4
  and bv8type = BitVector.mk_sort ctx 8 in
  let pal = Z3Array.mk_const_s ctx "pal" bv4type bv4type
  and pixels = Z3Array.mk_const_s ctx "pixels" itype bv8type
  and solver = Solver.mk_solver_t ctx (Tactic.mk_tactic ctx "smt") in
  ignore (List.fold_right
    (fun altlist num ->
      let num_exp = mkint num in
      let p0_idx = pal_bit ctx pixels itype bv4type 0 num_exp in
      let p1_idx = pal_bit ctx pixels itype bv4type 1 num_exp in
      let p2_idx = pal_bit ctx pixels itype bv4type 2 num_exp in
      let p3_idx = pal_bit ctx pixels itype bv4type 3 num_exp in
      let p0_a = Z3Array.mk_select ctx pal p0_idx in
      let p1_a = Z3Array.mk_select ctx pal p1_idx in
      let p2_a = Z3Array.mk_select ctx pal p2_idx in
      let p3_a = Z3Array.mk_select ctx pal p3_idx in
      let num_exp = mkint (num + 1) in
      let p0_idx = pal_bit ctx pixels itype bv4type 0 num_exp in
      let p1_idx = pal_bit ctx pixels itype bv4type 1 num_exp in
      let p2_idx = pal_bit ctx pixels itype bv4type 2 num_exp in
      let p3_idx = pal_bit ctx pixels itype bv4type 3 num_exp in
      let p0_b = Z3Array.mk_select ctx pal p0_idx in
      let p1_b = Z3Array.mk_select ctx pal p1_idx in
      let p2_b = Z3Array.mk_select ctx pal p2_idx in
      let p3_b = Z3Array.mk_select ctx pal p3_idx in
      let rows_eq = List.map
        (fun (d0, d1, d2, d3, c0, c1, c2, c3) ->
	  Boolean.mk_and ctx
	    [Boolean.mk_eq ctx p0_a (mkbvint c0);
	     Boolean.mk_eq ctx p1_a (mkbvint c1);
	     Boolean.mk_eq ctx p2_a (mkbvint c2);
	     Boolean.mk_eq ctx p3_a (mkbvint c3);
	     Boolean.mk_eq ctx p0_b (mkbvint d0);
	     Boolean.mk_eq ctx p1_b (mkbvint d1);
	     Boolean.mk_eq ctx p2_b (mkbvint d2);
	     Boolean.mk_eq ctx p3_b (mkbvint d3)])
	  altlist in
      Solver.add solver [Boolean.mk_or ctx rows_eq];
      num + 2)
    pixel_byte_alts
    0);
  begin match previous_palette with
    None -> ()
  | Some prev_pal -> constrain_previous_palette ctx solver pal prev_pal
  end;
  let status = Solver.check solver [] in
  match status with
    Solver.SATISFIABLE ->
      Printf.printf "sat\n";
      begin match Solver.get_model solver with
        Some model ->
	  let palette = Array.make 16 0 in
	  for i = 0 to 15 do
	    let pal_exp = Z3Array.mk_select ctx pal (mkbvint i) in
	    match Model.eval model pal_exp true with
	      Some num ->
		let ival = BitVector.get_int num in
		Printf.printf "  palette entry %d: %x\n" i ival;
		palette.(i) <- ival
	    | None -> failwith "Palette eval failed"
	  done;
	  let colbytes = ref [] in
	  for i = 0 to List.length pixel_byte_alts * 2 - 1 do
	    let byte_exp = Z3Array.mk_select ctx pixels (mkint i) in
	    match Model.eval model byte_exp true with
	      Some num ->
		let ival = BitVector.get_int num in
		Printf.printf "  byte val %d: %.2x\n" i ival;
		colbytes := ival :: !colbytes
	    | None -> failwith "Byte eval failed"
	  done;
	  Some (palette, !colbytes)
      | None -> None
      end
  | Solver.UNSATISFIABLE -> Printf.printf "unsat\n"; None
  | Solver.UNKNOWN -> Printf.printf "unknown\n"; None

let intensity r g b =
  0.2126 *. float_of_int r
  +. 0.7152 *. float_of_int g
  +. 0.0722 *. float_of_int b

let mean intenslist =
  let sum = List.fold_right (fun q tot -> q +. tot) intenslist 0.0 in
  sum /. float_of_int (List.length intenslist)

let variance intenslist =
  let mean = mean intenslist in
  let vari = List.fold_right (fun q tot -> (q -. mean) *. (q -. mean) +. tot)
			     intenslist 0.0 in
  vari /. float_of_int (List.length intenslist)

let rgb_of_col col =
  col land 1, (col land 2) lsr 1, (col land 4) lsr 2

let mixes () =
  let arr = Array.make 125 [] in
  for c1 = 0 to 7 do
    for c2 = c1 to 7 do
      for c3 = c2 to 7 do
        for c4 = c3 to 7 do
	  let r1, g1, b1 = rgb_of_col c1
	  and r2, g2, b2 = rgb_of_col c2
	  and r3, g3, b3 = rgb_of_col c3
	  and r4, g4, b4 = rgb_of_col c4 in
	  let i1 = intensity r1 g1 b1
	  and i2 = intensity r2 g2 b2
	  and i3 = intensity r3 g3 b3
	  and i4 = intensity r4 g4 b4 in
	  let rv = r1 + r2 + r3 + r4
	  and gv = g1 + g2 + g3 + g4
	  and bv = b1 + b2 + b3 + b4 in
	  let idx = bv * 25 + gv * 5 + rv in
	  let vari = variance [i1; i2; i3; i4] in
	  let sorted =
	    List.sort (fun (_, i1) (_, i2) -> compare i2 i1)
		      [c1, i1; c2, i2; c3, i3; c4, i4] in
	  let only_colours = List.map fst sorted in
	  if not (List.mem (vari, only_colours) arr.(idx)) then
	    arr.(idx) <- (vari, only_colours) :: arr.(idx)
	done
      done
    done
  done;
  for i = 0 to 124 do
    arr.(i) <- List.sort
		 (fun (var1, cols1) (var2, cols2) -> compare var1 var2)
		 arr.(i)
  done;
  arr

let get_rgb img x y =
  match img with
    Images.Rgb24 i -> Rgb24.get i x y
  | Images.Rgba32 i -> (Rgba32.get i x y).Color.color
  | _ -> failwith "Not an RGB/RGBA image"

let ordered_dither x y r g b =
  let dx = x land 1 and dy = y land 1 in
  let offset = [| -96; 32 ; 96; -32 |].(dy * 2 + dx) in
  (if r + offset > 127 then 255 else 0),
  (if g + offset > 127 then 255 else 0),
  (if b + offset > 127 then 255 else 0)

let select_from altlist nth =
  List.nth altlist (min nth (List.length altlist - 1))

let ordered_dither_2 mix_arr x y r g b mixno =
  let dx = x land 1 and dy = y land 1 in
  let r' = r / 52 and g' = g / 52 and b' = b / 52 in
  let idx = b' * 25 + g' * 5 + r' in
  let (_, col_list) = select_from mix_arr.(idx) mixno in
  let offset = [| 0; 3; 2; 1 |] in
  List.nth col_list offset.(dy * 2 + dx)

let closest_colour r g b =
  let r' = r / 128 and g' = g / 128 and b' = b / 128 in
  r' lor (g' lsl 1) lor (b' lsl 2)

let clamp x =
  if x < 0 then 0 else if x > 255 then 255 else x

let group_list lst =
  let ht = Hashtbl.create 5 in
  List.iter
    (fun item ->
      try
        let prev_count = Hashtbl.find ht item in
	Hashtbl.replace ht item (succ prev_count)
      with Not_found ->
        Hashtbl.add ht item 1)
    lst;
  let grouped_list = Hashtbl.fold
    (fun k v acc -> (k, v) :: acc)
    ht
    [] in
  List.sort (fun (_, n1) (_, n2) -> compare n2 n1) grouped_list

let render_attempt palette ht_matched ?ht_besteffort orig_byte_vals section =
  let bytes_ref = ref (List.rev orig_byte_vals) in
  for row = 0 to chunksize - 1 do
    for bytepos = 0 to 79 do
      let y = section * chunksize + row in
      let byte = List.hd !bytes_ref in
      bytes_ref := List.tl !bytes_ref;
      let c1, c2, c3, c4 = byte in
      begin try
        let pixbyte =
	  try Hashtbl.find ht_matched (c1, c2, c3, c4)
	  with Not_found as e ->
	    begin match ht_besteffort with
	      Some ht -> Hashtbl.find ht (c1, c2, c3, c4)
	    | None -> raise e
	    end in
	let n1, n2, n3, n4 = lookup_cols palette pixbyte in
	let clist = ref [n1; n2; n3; n4] in
	for pix = 0 to 3 do
          let x = bytepos * 4 + pix in
	  let col = List.hd !clist in
	  clist := List.tl !clist;
	  let cr, cg, cb = col_to_rgb col in
	  Graphics.set_color (Graphics.rgb cr cg cb);
	  Graphics.fill_rect (x * 2) ((255 - y) * 2 - 1) 2 2
	done
      with Not_found ->
	Graphics.set_color (Graphics.rgb 128 128 128);
	Graphics.fill_rect (bytepos * 8) ((255 - y) * 2 - 1) 8 2
      end
    done
  done

let rgb_diff (r1, g1, b1) (r2, g2, b2) =
  (float_of_int (r1 - r2) *. 0.2126) ** 2.0
  +. (float_of_int (g1 - g2) *. 0.7152) ** 2.0
  +. (float_of_int (b1 - b2) *. 0.0722) ** 2.0

let evaluate_error palette ht_matched ht_besteffort orig_byte_vals =
  let bytes_ref = ref (List.rev orig_byte_vals)
  and acc_error = ref 0.0 in
  for row = 0 to 1 do
    for bytepos = 0 to 79 do
      let byte = List.hd !bytes_ref in
      bytes_ref := List.tl !bytes_ref;
      let c1, c2, c3, c4 = byte in
      if not (Hashtbl.mem ht_matched (c1, c2, c3, c4)) then begin
        let pixbyte = Hashtbl.find ht_besteffort (c1, c2, c3, c4) in
	let n1, n2, n3, n4 = lookup_cols palette pixbyte in
	let err = rgb_diff (rgb_of_col c1) (rgb_of_col n1)
	          +. rgb_diff (rgb_of_col c2) (rgb_of_col n2)
		  +. rgb_diff (rgb_of_col c3) (rgb_of_col n3)
		  +. rgb_diff (rgb_of_col c4) (rgb_of_col n4) in
	acc_error := !acc_error +. err
      end
    done
  done;
  !acc_error

(* LO_POINT should succeed, HI_POINT should fail. Find the highest point in
   BYTES_ARR that succeeds.  *)

let find_splitpoint bytes_arr hist_arr badness_arr lo_point hi_point best_in
                    bytevals section previous_palette =
  let rec scan lo hi best =
    let midpt = (lo + hi) / 2 in
    if lo = hi - 1 || midpt = lo then
      lo, best
    else begin
      let byte_list = Array.to_list (Array.sub bytes_arr 0 (midpt + 1)) in
      match find_palette byte_list bytes_arr hist_arr badness_arr
			 (if section > 0 then Some previous_palette
			  else None) with
	Some (p, ht_m, ht_be) as new_result ->
	  render_attempt p ht_m bytevals section;
	  scan midpt hi new_result
      | None ->
          scan lo midpt best
    end in
  scan lo_point hi_point best_in

let indexify lst =
  let ht = Hashtbl.create 5 in
  let keys, _ = List.fold_left
    (fun (keys, idx) item ->
      if Hashtbl.mem ht item then
        (keys, idx)
      else begin
        Hashtbl.add ht item idx;
	(item::keys, succ idx)
      end)
    ([], 0)
    lst in
  let idx_out = List.map (fun x -> Hashtbl.find ht x) lst in
  List.rev keys, idx_out

let add_to_front frmlist tolist outlist =
  List.fold_right (fun elem outlist' -> (elem::tolist)::outlist')
		  frmlist outlist

let iterate_product inlist =
  let rec iterate inlist out =
    match inlist with
      [] -> out
    | [e] -> add_to_front e [] out
    | e::es ->
	let rem = iterate es out in
	List.fold_right (fun elem acc -> add_to_front e elem acc) rem [] in
  iterate inlist []

let take lst n =
  let rec scan out n inp =
    if n = 0 then List.rev out
    else match inp with
      [] -> List.rev out
    | q::qs -> scan (q::out) (n - 1) qs in
  scan [] n lst

let multimix img mixes section randomness previous_palette cutoff =
  let r_randomness = (randomness * 54) / 256
  and g_randomness = (randomness * 183) / 256
  and b_randomness = (randomness * 18) / 256 in
  let things = ref [] in
  for byte = 0 to 79 do
    let idxlist = ref [] in
    for row = 0 to 1 do
      let y = section * chunksize + row in
      for pix = 0 to 3 do
	let x = byte * 4 + pix in
	let col = get_rgb img x y in
	let cr, cg, cb =
	  let bias_r =
	    if r_randomness > 0 then
	      (Random.int r_randomness) - r_randomness / 2
	    else 0
	  and bias_g =
	    if g_randomness > 0 then
	      (Random.int g_randomness) - g_randomness / 2
	    else 0
	  and bias_b =
	    if b_randomness > 0 then
	      (Random.int b_randomness) - b_randomness / 2 
	    else 0 in
	clamp (col.Color.Rgb.r + bias_r),
	clamp (col.Color.Rgb.g + bias_g),
	clamp (col.Color.Rgb.b + bias_b) in
	let r' = cr / 52 and g' = cg / 52 and b' = cb / 52 in
	let idx = b' * 25 + g' * 5 + r' in
	idxlist := !idxlist @ [idx]
      done
    done;
    let uniq, indices = indexify !idxlist in
    let uniqlst = List.map (fun idx -> take mixes.(idx) cutoff) uniq in
    let iterated = iterate_product uniqlst in
    (*Printf.printf "Byte %d: iterated length: %d " byte (List.length iterated);*)
    let pset = ref PixelPairSet.empty in
    List.iteri
      (fun i altlist ->
        let _, pix_out = List.fold_right
	  (fun pix_alt (pixno, pixout) ->
	    let _, dithercols = List.nth altlist pix_alt in
	    let dx = pixno land 1 and dy = (pixno land 4) lsr 2 in
	    let offset = [| 1; 2; 3; 0 |].(dy * 2 + dx) in
	    succ pixno, List.nth dithercols offset :: pixout)
	  indices
	  (0, []) in
	match pix_out with
	  [p1; p2; p3; p4;
	   q1; q2; q3; q4] ->
	    pset := PixelPairSet.add (p1, p2, p3, p4, q1, q2, q3, q4) !pset;
	| _ -> failwith "No pixels?")
      iterated;
    Printf.printf "%d " (PixelPairSet.cardinal !pset);
    things := (PixelPairSet.elements !pset) :: !things
    (*PixelPairSet.iter
      (fun (p1, p2, p3, p4, q1, q2, q3, q4) ->
        Printf.printf "pixel: [%d %d %d %d] [%d %d %d %d]\n" p1 p2 p3 p4
		      q1 q2 q3 q4)
      !pset;
    Printf.printf "\n"*)
  done;
  Printf.printf "\nSolving...\n";
  flush stdout;
  let uniq, index = indexify !things in
  let res = begin match find_palette2 uniq
			    (if section = 0 then None
			     else Some previous_palette) with
    Some (pal, colbytes) ->
      let index_ref = ref (List.rev index)
      and ht_m = Hashtbl.create 10
      and bytelist_compat = Array.make 160 (0, 0, 0, 0) in
      for byte = 0 to 79 do
	let col_idx = List.hd !index_ref in
	index_ref := List.tl !index_ref;
	for row = 0 to 1 do
	  let y = section * 2 + row in
	  let col = List.nth colbytes (2 * col_idx + row) in
	  let n1, n2, n3, n4 = lookup_cols pal col in
	  Hashtbl.add ht_m (n1, n2, n3, n4) col;
	  bytelist_compat.(row * 80 + byte) <- (n1, n2, n3, n4);
	  let clist = ref [n1; n2; n3; n4] in
	  for pix = 0 to 3 do
	    let x = byte * 4 + pix in
	    let col = List.hd !clist in
	    clist := List.tl !clist;
	    let cr, cg, cb = col_to_rgb col in
	    Graphics.set_color (Graphics.rgb cr cg cb);
	    Graphics.fill_rect (x * 2) ((255 - y) * 2 - 1) 2 2
	  done
	done
      done;
      Some (pal, ht_m, List.rev (Array.to_list bytelist_compat))
  | None -> None
  end in
  flush stdout;
  res

let attempt img mixes section previous_palette xmask randomness mixno fast_mode
	    these_errors_r these_errors_g these_errors_b next_errors_r 
	    next_errors_g next_errors_b ~random_dither ~plain_fs
	    ~mmix_err_threshold =
  let r_randomness = (randomness * 54) / 256
  and g_randomness = (randomness * 183) / 256
  and b_randomness = (randomness * 18) / 256 in
  let bytevals = ref [] in
  for row = 0 to chunksize - 1 do
    for byte = 0 to 79 do
      let pixlist = ref [] in
      for pix = 0 to 3 do
	let x = byte * 4 + pix
	and y = section * chunksize + row in
	let col = get_rgb img (x land (lnot xmask)) y in
	let cr, cg, cb =
	  if random_dither then begin
	    let bias_r =
	      if r_randomness > 0 then
		(Random.int r_randomness) - r_randomness / 2
	      else 0
	    and bias_g =
	      if g_randomness > 0 then
		(Random.int g_randomness) - g_randomness / 2
	      else 0
	    and bias_b =
	      if b_randomness > 0 then
		(Random.int b_randomness) - b_randomness / 2 
	      else 0 in
	    let cr = clamp (col.Color.Rgb.r + bias_r)
	    and cg = clamp (col.Color.Rgb.g + bias_g)
	    and cb = clamp (col.Color.Rgb.b + bias_b) in
	    cr, cg, cb
	  end else begin
	    let this_r =
	      clamp (col.Color.Rgb.r + int_of_float these_errors_r.(x))
	    and this_g =
	      clamp (col.Color.Rgb.g + int_of_float these_errors_g.(x))
	    and this_b =
	      clamp (col.Color.Rgb.b + int_of_float these_errors_b.(x)) in
	    let selected_r, selected_g, selected_b =
	      if not plain_fs then
	        (this_r / 52) * 52,
		(this_g / 52) * 52,
		(this_b / 52) * 52
	      else
	        (if this_r < 128 then 0 else 255),
		(if this_g < 128 then 0 else 255),
		(if this_b < 128 then 0 else 255) in
	    let error_r = float_of_int (this_r - selected_r)
	    and error_g = float_of_int (this_g - selected_g)
	    and error_b = float_of_int (this_b - selected_b) in
	    if x < 319 then begin
	      these_errors_r.(x + 1) <- these_errors_r.(x + 1)
				      +. error_r *. 7.0 /. 16.0;
	      these_errors_g.(x + 1) <- these_errors_g.(x + 1)
				      +. error_g *. 7.0 /. 16.0;
	      these_errors_b.(x + 1) <- these_errors_b.(x + 1)
				      +. error_b *. 7.0 /. 16.0;
	      next_errors_r.(x + 1) <- next_errors_r.(x + 1)
				       +. error_r *. 1.0 /. 16.0;
	      next_errors_g.(x + 1) <- next_errors_g.(x + 1)
				       +. error_g *. 1.0 /. 16.0;
	      next_errors_b.(x + 1) <- next_errors_b.(x + 1)
				       +. error_b *. 1.0 /. 16.0
	    end;
	    if x > 0 then begin
	      next_errors_r.(x - 1) <- next_errors_r.(x - 1)
				       +. error_r *. 3.0 /. 16.0;
	      next_errors_g.(x - 1) <- next_errors_g.(x - 1)
				       +. error_g *. 3.0 /. 16.0;
	      next_errors_b.(x - 1) <- next_errors_b.(x - 1)
				       +. error_b *. 3.0 /. 16.0
	    end;
	    next_errors_r.(x) <- next_errors_r.(x)
				 +. error_r *. 5.0 /. 16.0;
	    next_errors_g.(x) <- next_errors_g.(x)
				 +. error_g *. 5.0 /. 16.0;
	    next_errors_b.(x) <- next_errors_b.(x)
				 +. error_b *. 5.0 /. 16.0;
	    (this_r, this_g, this_b)
	  end in
	let dithered = if plain_fs then closest_colour cr cg cb
		       else ordered_dither_2 mixes x y cr cg cb mixno in
	let cr, cg, cb = col_to_rgb dithered in
	Graphics.set_color (Graphics.rgb cr cg cb);
	Graphics.fill_rect (x * 2) ((255 - y) * 2 - 1) 2 2;
	pixlist := !pixlist @ [dithered]
      done;
      match !pixlist with
	[a; b; c; d] ->
	  bytevals := (a, b, c, d) :: !bytevals
      | _ -> failwith "Wrong list length"
    done;
    Array.blit next_errors_r 0 these_errors_r 0 320;
    Array.blit next_errors_g 0 these_errors_g 0 320;
    Array.blit next_errors_b 0 these_errors_b 0 320;
    Array.fill next_errors_r 0 320 0.0;
    Array.fill next_errors_g 0 320 0.0;
    Array.fill next_errors_b 0 320 0.0;
  done;
  let grouped_bytelist = group_list !bytevals in
  let orig_cols = List.map fst grouped_bytelist in
  let cols_arr = Array.of_list orig_cols
  and hist_arr = Array.of_list (List.map snd grouped_bytelist) in
  let num_cols = Array.length cols_arr in
  let badness_arr = Array.make num_cols 0.0 in
  let rec search_down lwm hwm best =
    Printf.printf "Searching %d-%d:\n" lwm hwm;
    flush stdout;
    let splitpt, res
      = find_splitpoint cols_arr hist_arr badness_arr lwm hwm best !bytevals
			section previous_palette in
    let len = num_cols - splitpt - 2 in
    if len > 0 then begin
      let deleted = cols_arr.(splitpt + 1)
      and deleted_hist = hist_arr.(splitpt + 1) in
      Array.blit cols_arr (splitpt + 2) cols_arr (splitpt + 1) len;
      Array.blit hist_arr (splitpt + 2) hist_arr (splitpt + 1) len;
      cols_arr.(num_cols - 1) <- deleted;
      hist_arr.(num_cols - 1) <- deleted_hist
    end;
    if splitpt + 1 < hwm && not fast_mode then begin
      let sublength = hwm - splitpt - 1 in
      let subarr = Array.init sublength
        (fun i -> cols_arr.(i + splitpt + 1), hist_arr.(i + splitpt + 1),
		  badness_arr.(i + splitpt + 1)) in
      Array.sort
        (fun (_, h1, b1) (_, h2, b2) ->
	  compare (float_of_int h2 *. b2) (float_of_int h1 *. b1))
	subarr;
      for i = 0 to sublength - 1 do
        match subarr.(i) with
	  c1, h1, b1 ->
            cols_arr.(i + splitpt + 1) <- c1;
	    hist_arr.(i + splitpt + 1) <- h1
      done;
      search_down splitpt (hwm - 1) res
    end else
      res in
  let res = search_down 1 num_cols None in
  match res with
    Some (p, ht_m, ht_be) ->
      render_attempt p ht_m ~ht_besteffort:ht_be !bytevals section;
      let section_error = evaluate_error p ht_m ht_be !bytevals in
      Printf.printf "Error for section %d: %f\n" section section_error;
      if section_error > mmix_err_threshold then begin
	let rec retry cutoff =
	  if cutoff > 20 then
	    p, ht_m, ht_be, !bytevals
	  else begin
            Printf.printf "Trying multimix instead... (cutoff=%d)\n" cutoff;
            match multimix img mixes section randomness previous_palette
			   cutoff with
	      Some (p, ht_m, bytevals) ->
	        p, ht_m, (Hashtbl.create 1), bytevals
	    | None ->
		retry (if cutoff < 5 then cutoff + 1 else cutoff * 4)
	  end in
	retry 1
      end else
	p, ht_m, ht_be, !bytevals
  | None ->
      begin match find_palette orig_cols cols_arr hist_arr badness_arr
			       (if section > 0 then Some previous_palette
				else None) with
        Some (p, ht_m, ht_be) ->
	  render_attempt p ht_m ~ht_besteffort:ht_be !bytevals section;
	  p, ht_m, ht_be, !bytevals
      | None -> failwith "1-colour fallback failed"
      end

let _ =
  let xmask_ref = ref 0
  and fastmode_ref = ref false
  and wait_at_end = ref true
  and dither_ref = ref "ordered"
  and mixno_ref = ref 0
  and randomness_ref = ref 64
  and mmix_err_threshold = ref 2.0
  and inputfile = ref ""
  and outfile = ref "" in
  let argspec =
    ["-xmask", Arg.Set_int xmask_ref, "Set xmask for reading source image";
     "-fast", Arg.Set fastmode_ref, "Fast mode (stop search after first match)";
     "-dither", Arg.Set_string dither_ref, "Dither type (fs, ordered, ord+fs)";
     "-mixno", Arg.Set_int mixno_ref, "Use next-higher contrast mixes";
     "-random", Arg.Set_int randomness_ref, "Amount of randomness (def. 64)";
     "-errmax", Arg.Set_float mmix_err_threshold,
		"Error threshold for attempting multimix";
     "-nowait", Arg.Clear wait_at_end, "Wait at end before closing window";
     "-o", Arg.Set_string outfile, "Set output file name"] in
  Arg.parse argspec (fun inp -> inputfile := inp) "Usage: palsearch [opts]";
  let img = Images.load !inputfile [] in
  Graphics.open_graph "";
  Graphics.set_window_title "Palette search";
  Graphics.resize_window 640 512;
  let mixes = mixes () in
  (*while true; do
    for go = 0 to 1 do
      let flip = go = 1 in
      for y = 0 to 255 do
	for x = 0 to 319 do
	  let bias_r = (Random.int 32) - 16
	  and bias_g = (Random.int 8) - 4
	  and bias_b = (Random.int 64) - 32 in
	  let col = get_rgb img x (255 - y) in
	  let cr = clamp (col.Color.Rgb.r + bias_r)
	  and cg = clamp (col.Color.Rgb.g + bias_g)
	  and cb = clamp (col.Color.Rgb.b + bias_b) in
	  (*let cr, cg, cb = ordered_dither x y cr cg cb in*)
	  let cr, cg, cb
	    = col_to_rgb (ordered_dither_2 mixes x y cr cg cb flip) in
	  Graphics.set_color (Graphics.rgb cr cg cb);
	  Graphics.fill_rect (x * 2) (y * 2) 2 2
	done
      done;
      ignore (Graphics.wait_next_event [Graphics.Button_down])
    done
  done*)
  let do_multimix, random_dither, plain_fs =
    match !dither_ref with
      "ordered" -> false, true, false
    | "fs" -> false, false, true
    | "ord+fs" -> false, false, false
    | "multi" -> true, true, false
    | x ->
	Printf.fprintf stderr "Bad dither type '%s'\n" x;
	exit 1 in
  let screen_bytes = Bytes.create 20480
  and output_palettes = Bytes.create (16 + change_per_row*128) in
  let these_errors_r = Array.create 320 0.0
  and these_errors_g = Array.create 320 0.0
  and these_errors_b = Array.create 320 0.0
  and next_errors_r = Array.create 320 0.0
  and next_errors_g = Array.create 320 0.0
  and next_errors_b = Array.create 320 0.0 in
  let previous_palette = Array.create 16 (-1) in
  for section = 0 to (256 / chunksize) - 1 do
    Printf.printf "Section %d:\n" section;
    flush stdout;
    if do_multimix then
      ignore (multimix img mixes section !randomness_ref previous_palette
		       max_int)
    else begin
      let palette, ht_matched, ht_besteffort, bytevals
	= attempt img mixes section previous_palette !xmask_ref !randomness_ref
		  !mixno_ref !fastmode_ref these_errors_r these_errors_g
		  these_errors_b next_errors_r next_errors_g next_errors_b
		  ~random_dither ~plain_fs
		  ~mmix_err_threshold:!mmix_err_threshold in
      let row_start = (section / 4) * 640 + (section mod 4) * 2 in
      List.iteri
	(fun idx colours_quad ->
	  let offset = row_start + (idx mod 80) * 8 + (idx / 80) in
	  let byte =
	    try
	      Hashtbl.find ht_matched colours_quad
	    with Not_found ->
	      Hashtbl.find ht_besteffort colours_quad in
	  screen_bytes.[offset] <- Char.chr byte)
	(List.rev bytevals);
      begin if section = 0 then
	for i = 0 to 15 do
	  output_palettes.[i] <- Char.chr ((i lsl 4) + (palette.(i) lxor 7))
	done
      else
	let same_no = ref 0 and change_no = ref 0 in
	for i = 0 to 15 do
	  if !same_no >= (16 - change_per_row)
	     || palette.(i) <> previous_palette.(i) then begin
	    Printf.printf "Entry %d: %d -> %d\n" i
			  previous_palette.(i) palette.(i);
	    assert (!change_no < change_per_row);
	    output_palettes.[16 + !change_no * 128 + (section - 1)]
	      <- Char.chr ((i lsl 4) + (palette.(i) lxor 7));
	    incr change_no
	  end else begin
	    Printf.printf "Entry %d: %d == %d\n" i previous_palette.(i)
			  palette.(i);
	    incr same_no
	  end
	done
      end;
      Array.blit palette 0 previous_palette 0 16
    end
  done;
  (*let arr = Array.init 128 (fun n -> n) in
  let out = ForkWork.map_array (fun z -> multimix img mixes z
			       !randomness_ref; ()) arr in*)
  if !wait_at_end then
    ignore (Graphics.wait_next_event [Graphics.Button_down]);
  if !outfile <> "" then begin
    let f = open_out !outfile in
    output_string f (Bytes.to_string output_palettes);
    output_string f (Bytes.to_string screen_bytes);
    close_out f
  end
