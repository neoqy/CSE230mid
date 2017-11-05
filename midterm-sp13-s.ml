(********** 1 **********)

(* a *)

let length l =
  let f acc elem = acc + 1 in
List.fold_left f 0 l;;

(* b *)

let remove l x =
  let f acc elem = if elem == x then acc else (acc@[elem]) in
List.fold_left f [] l;;

(********** 2 **********)

(* a *)

let rec ith l i d =
  match l with
  | [] -> d
  | h::t -> if i == 0 then h else ith t (i - 1) d;;

(* b *)

let rec update l i n =
  match l with
  | [] -> []
  | h::t -> if i == 0 then n::t else h::(update t (i - 1) n);;

(* c *)

let rec update2 l i n d =
  match l with
  | [] -> if i == 0 then [n] else d::update2 l (i - 1) n d
  | h::t -> if i == 0 then n::t else h::(update2 t (i - 1) n d);;

(********** 3 **********)

(* let categorize f l =
  let base = [] in
  let fold_fn acc elmt =
    let rec update3 l i n d = match l with
    | [] -> if i == 0 then [[n]] else d::(update3 l (i - 1) n d)
    | h::t -> if i == 0 then (h@[n])::t else h::(update3 t (i - 1) n d)
  in update3 acc (f elmt) elmt []
in List.fold_left fold_fn base l;; *)

(* Use function ith and update2 *)

let categorize f l =
  let base = [] in
  let fold_fn acc elmt =
    let i = (f elmt) in
    let n = (ith acc i [])@[elmt] in
    update2 acc i n []
  in List.fold_left fold_fn base l;;

(* (* TIPS FOR TEST *)
 * Remember 'rec'
 * Is the number of arguments right?
 * Type 'a list list is tricky
*)
