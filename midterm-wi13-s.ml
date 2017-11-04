(********** 1 **********)

type 'a maybe =
  | None
  | Some of 'a

(* a *)

let first f l =
  let base = None in
  let fold_fn acc elmt =
    if acc != None then acc
    else (if (f elmt) then (Some elmt) else acc)
  in List.fold_left fold_fn base l;;

(* Type them yourself! :-) *)

(********** 2 **********)

(* a *)

let rec zip l1 l2 =
  match (l1, l2) with
  | (h1::t1, h2::t2) -> (h1, h2)::(zip t1 t2)
  | _ -> [];;

(* b *)

let map2 f l1 l2 =
  let temp = zip l1 l2 in
  List.map (fun (a, b) -> f a b) temp;;

(* c *)

let map3 f l1 l2 l3 =
  let temp = zip (zip l1 l2) l3 in
  List.map (fun ((a, b), c) -> f a b c) temp;;

(********** 3 **********)

let rec unzip l =
  match l with
  | (m, n)::t -> let (mt, nt) = unzip t in (m::mt, n::nt)
  | _ -> ([],[]);;
