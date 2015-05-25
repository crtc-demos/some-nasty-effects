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

let rnd_palette =
  [
    0, 0, 0, 0;
    0, 1, 0, 1;
    1, 0, 1, 0;
    1, 1, 1, 1;
    1, 2, 1, 2;
    2, 1, 2, 1;
    2, 2, 2, 2;
    2, 3, 2, 3;
    3, 2, 3, 2;
    3, 3, 3, 3;
    3, 4, 3, 4;
    4, 3, 4, 3;
    4, 4, 4, 4;
    4, 5, 4, 5;
    5, 4, 5, 4;
    5, 5, 5, 5;
    5, 1, 2, 3;
    5, 0, 0, 0;
    4, 3, 2, 1;
    1, 2, 3, 4;
    4, 5, 6, 7;
    7, 6, 5, 4
  ]

let pal_bit ctx pbits itype bitno entry =
  let mkint n = Arithmetic.Integer.mk_numeral_i ctx n in
  let zero = mkint 0 in
  let rec build_operands tap bitval lst =
    if bitval = 0 then
      lst
    else
      let new_op =
	if tap >= 0 then
	  Boolean.mk_ite ctx (Z3Array.mk_select ctx pbits.(tap) entry)
		  (mkint bitval) zero
	else
	  mkint bitval in
      build_operands (tap - 2) (bitval lsr 1) (new_op::lst) in
  let ops = build_operands (7 - bitno) 8 [] in
  Arithmetic.mk_add ctx ops

let byte_expr ctx pbits itype entry =
  let mkint n = Arithmetic.Integer.mk_numeral_i ctx n in
  let zero = mkint 0 in
  let rec build_operands tap bitval lst =
    if bitval = 0 then
      lst
    else
      let new_op =
        Boolean.mk_ite ctx (Z3Array.mk_select ctx pbits.(tap) entry)
		       (mkint bitval) zero in
      build_operands (pred tap) (bitval lsr 1) (new_op::lst) in
  let ops = build_operands 7 128 [] in
  Arithmetic.mk_add ctx ops

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

let find_palette pixel_bytes all_colours =
  let ctx = mk_context ["model", "true"; "proof", "false"] in
  let mkint n = Arithmetic.Integer.mk_numeral_i ctx n in
  let itype = Arithmetic.Integer.mk_sort ctx
  and btype = Boolean.mk_sort ctx in
  let pal_sym = Symbol.mk_string ctx "pal" in
  let pal = Z3Array.mk_const ctx pal_sym itype itype in
  let pbits = Array.map
    (fun nm -> Z3Array.mk_const_s ctx nm itype btype)
    [| "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h" |] in
  let solver = Solver.mk_solver_s ctx "smt" in
  ignore (List.fold_right
    (fun (c0, c1, c2, c3) num ->
      let num_exp = mkint num in
      let p0_idx = pal_bit ctx pbits itype 0 num_exp in
      let p1_idx = pal_bit ctx pbits itype 1 num_exp in
      let p2_idx = pal_bit ctx pbits itype 2 num_exp in
      let p3_idx = pal_bit ctx pbits itype 3 num_exp in
      let p0 = Z3Array.mk_select ctx pal p0_idx in
      let p1 = Z3Array.mk_select ctx pal p1_idx in
      let p2 = Z3Array.mk_select ctx pal p2_idx in
      let p3 = Z3Array.mk_select ctx pal p3_idx in
      Solver.add solver [Boolean.mk_eq ctx p0 (mkint c0);
			 Boolean.mk_eq ctx p1 (mkint c1);
			 Boolean.mk_eq ctx p2 (mkint c2);
			 Boolean.mk_eq ctx p3 (mkint c3)];
      succ num)
    pixel_bytes
    0);
  let status = Solver.check solver [] in
  match status with
    Solver.SATISFIABLE ->
      Printf.printf "sat\n";
      let ht_matched = Hashtbl.create 10
      and ht_besteffort = Hashtbl.create 10 in
      begin match Solver.get_model solver with
	Some model ->
	  let palette = Array.make 16 0 in
	  for i = 0 to 15 do
	    let pal_exp = Z3Array.mk_select ctx pal (mkint i) in
	    match Model.eval model pal_exp true with
	      Some num ->
		let ival = Arithmetic.Integer.get_int num in
		Printf.printf "  palette entry %d: %x\n" i ival;
		palette.(i) <- ival
	    | None -> failwith "Palette eval failed"
	  done;
	  let inp_length = List.length pixel_bytes in
	  for i = 0 to inp_length - 1 do
	    let byte_exp = byte_expr ctx pbits itype (mkint i) in
	    match Model.eval model byte_exp true with
	      Some num ->
		let ival = Arithmetic.Integer.get_int num in
		let cols = lookup_cols palette ival in
		Printf.printf "  byte val %d: %.2x\n" i ival;
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
		  Printf.printf
		    "  byte val %d: %.2x, using (%d,%d,%d,%d) for \
		     (%d,%d,%d,%d)\n"
		    i !min_chk a1 a2 a3 a4 a5 a6 a7 a8;
		  Hashtbl.add ht_besteffort (a5, a6, a7, a8) !min_chk
	    done
	  end;
	  Some (palette, ht_matched, ht_besteffort)
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
    for c2 = 0 to 7 do
      for c3 = 0 to 7 do
        for c4 = 0 to 7 do
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

let ordered_dither_2 mix_arr x y r g b flip =
  let dx = x land 1 and dy = y land 1 in
  let r' = r / 52 and g' = g / 52 and b' = b / 52 in
  let idx = b' * 25 + g' * 5 + r' in
  let (_, col_list) =
    if flip then List.hd (List.rev mix_arr.(idx)) else List.hd mix_arr.(idx) in
  let offset = [| 0; 3; 2; 1 |] in
  List.nth col_list offset.(dy * 2 + dx)

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
  for row = 0 to 1 do
    for bytepos = 0 to 79 do
      let y = section * 2 + row in
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

(* LO_POINT should succeed, HI_POINT should fail. Find the highest point in
   BYTES_ARR that succeeds.  *)

let find_splitpoint bytes_arr lo_point hi_point best_in
                    bytevals section =
  let rec scan lo hi best =
    let midpt = (lo + hi) / 2 in
    if lo = hi - 1 || midpt = lo then
      lo, best
    else begin
      let byte_list = Array.to_list (Array.sub bytes_arr 0 (midpt + 1)) in
      match find_palette byte_list bytes_arr with
	Some (p, ht_m, ht_be) ->
	  render_attempt p ht_m bytevals section;
	  scan midpt hi (Some (p, ht_m, ht_be))
      | None ->
          scan lo midpt best
    end in
  scan lo_point hi_point best_in

let attempt img mixes section xmask flip fast_mode =
  let bytevals = ref [] in
  for row = 0 to 1 do
    for byte = 0 to 79 do
      let pixlist = ref [] in
      for pix = 0 to 3 do
	let x = byte * 4 + pix
	and y = section * 2 + row in
	let col = get_rgb img (x land (lnot xmask)) y
	and bias_r = (Random.int 32) - 16
	and bias_g = (Random.int 8) - 4
	and bias_b = (Random.int 64) - 32 in
	let cr = clamp (col.Color.Rgb.r + bias_r)
	and cg = clamp (col.Color.Rgb.g + bias_g)
	and cb = clamp (col.Color.Rgb.b + bias_b) in
	let dithered = ordered_dither_2 mixes x y cr cg cb flip in
	let cr, cg, cb = col_to_rgb dithered in
	Graphics.set_color (Graphics.rgb cr cg cb);
	Graphics.fill_rect (x * 2) ((255 - y) * 2 - 1) 2 2;
	pixlist := !pixlist @ [dithered]
      done;
      match !pixlist with
	[a; b; c; d] ->
	  bytevals := (a, b, c, d) :: !bytevals
      | _ -> failwith "Wrong list length"
    done
  done;
  let grouped_bytelist = group_list !bytevals in
  let orig_cols = List.map fst grouped_bytelist in
  let cols_arr = Array.of_list orig_cols in
  let rec search_down lwm hwm best =
    Printf.printf "Searching %d-%d:\n" lwm hwm;
    flush stdout;
    let splitpt, res
      = find_splitpoint cols_arr lwm hwm best !bytevals section in
    let len = Array.length cols_arr - splitpt - 2 in
    if len > 0 then begin
      let deleted = cols_arr.(splitpt + 1) in
      Array.blit cols_arr (splitpt + 2) cols_arr (splitpt + 1) len;
      cols_arr.(Array.length cols_arr - 1) <- deleted
    end;
    if splitpt + 1 < hwm && not fast_mode then
      search_down splitpt (hwm - 1) res
    else
      res in
  let res = search_down 1 (Array.length cols_arr) None in
  match res with
    Some (p, ht_m, ht_be) ->
      render_attempt p ht_m ~ht_besteffort:ht_be !bytevals section;
      p, ht_m, ht_be, !bytevals
  | None ->
      begin match find_palette orig_cols cols_arr with
        Some (p, ht_m, ht_be) ->
	  render_attempt p ht_m ~ht_besteffort:ht_be !bytevals section;
	  p, ht_m, ht_be, !bytevals
      | None -> failwith "1-colour fallback failed"
      end

let _ =
  let xmask_ref = ref 0
  and fastmode_ref = ref false
  and contrastmix_ref = ref false
  and inputfile = ref ""
  and outfile = ref "" in
  let argspec =
    ["-xmask", Arg.Set_int xmask_ref, "Set xmask for reading source image";
     "-fast", Arg.Set fastmode_ref, "Fast mode (stop search after first match)";
     "-contrastmix", Arg.Set contrastmix_ref, "Use high-constrast colour mixes";
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
  let screen_bytes = Bytes.create 20480
  and output_palettes = Bytes.create 2048 in
  for section = 0 to 127 do
    Printf.printf "Section %d:\n" section;
    flush stdout;
    let palette, ht_matched, ht_besteffort, bytevals
      = attempt img mixes section !xmask_ref !contrastmix_ref !fastmode_ref in
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
    for i = 0 to 15 do
      output_palettes.[section * 16 + i] <- Char.chr palette.(i)
    done
  done;
  ignore (Graphics.wait_next_event [Graphics.Button_down]);
  if !outfile <> "" then begin
    let f = open_out !outfile in
    output_string f (Bytes.to_string output_palettes);
    output_string f (Bytes.to_string screen_bytes);
    close_out f
  end
