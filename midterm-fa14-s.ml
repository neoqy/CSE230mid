(********* 1 **********)

type expr =
| Const of int
| Var of string
| Op of string * expr * expr;;

(* a *)

let rec rename_var e n1 n2 =
  match e with
  | Const i -> Const i
  | Var x -> if x = n1 then Var n2 else Var x
  | Op (o, e1, e2) -> Op (o, rename_var e1 n1 n2, rename_var e2 n1 n2);;

(* b *)

let to_str e =
  let rec str_helper e top_level =
    match e with
    | Const i -> string_of_int i
    | Var x -> x
    | Op (o, e1, e2) -> if top_level
      then (str_helper e1 false) ^ o ^ (str_helper e2 false)
      else "(" ^ (str_helper e1 false) ^ o ^ (str_helper e2 false) ^ ")"
  in str_helper e true;;

(********* 2 **********)

let average_if f l =
  let folding_fn (acc, count) elem =
    if (f elem) then (acc + elem, count + 1)
    else (acc, count)
  in let base = (0, 0) in
  let (sum, times) = List.fold_left folding_fn base l in
  if (times != 0) then (sum / times) else 0;; (* !!! times can be 0 !!! *)

(********* 3 **********)

(* a *)

let length_2 l =
  List.fold_left (+) 0 (List.map List.length l);;

(* b *)

let length_3 l =
   List.fold_left (+) 0 (List.map length_2 l);;

(********* 4 **********)

(* Type them yourself! :-) *)

(* final-sp13-2 *)
(* type 'a tree =
  | Empty
  | Node of 'a * 'a tree list;;

let rec zip l1 l2 =
  match (l1,l2) with
  | ([],[]) -> []
  | (h1::t1, h2::t2) -> (h1,h2)::(zip t1 t2)
  | _ -> [];;

let rec tree_zip t1 t2 =
  match (t1,t2) with
  | (Empty, Empty) -> Empty
  | (Node(v1, l1), Node(v2, l2)) ->
  	let tl = zip l1 l2 in
  	let l = List.map (fun (t1, t2) -> tree_zip t1 t2) tl in
  	Node ((v1, v2), l);; 