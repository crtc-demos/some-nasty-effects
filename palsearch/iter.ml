let q = [[1;2;3]; [4;5;6]; [7;8;9]]

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
