(********** 1 **********)

(* a *)

let rec split l =
  let half = (List.length l) / 2 in
  let base = (0, [], []) in
  let fold_fn (i, l1, l2) elmt =
    if i < half then (i + 1, l1 @ [elmt], l2)
    else (i + 1, l1, l2 @ [elmt])
  in let (_, l1, l2) = List.fold_left fold_fn base l in
  (l1, l2);;

(* b *)

let rec merge l1 l2 =
  match (l1, l2) with
  | ([], l) -> l
  | (l, []) -> l
  | (h1::t1, h2::t2) ->
    if h1 < h2 then h1::(merge t1 (h2::t2))
    else h2::(merge (h1::t1) t2);;

(* c *)

let rec merge_sort l =
  if List.length l < 2 then l
  else let (l1, l2) = split l in
  let l1s = merge_sort l1 in
  let l2s = merge_sort l2 in
  merge l1s l2s;;

(********** 2 **********)

(* href="https://caml.inria.fr/pub/old_caml_site/Examples/oc/basics/explode.ml" *)

let explode s =
  let rec expl i l =
    if i < 0 then l else
    expl (i - 1) (s.[i]::l) in
  expl (String.length s - 1) [];;

let implode l =
  let result = String.create (List.length l) in
  let rec imp i = function
  | [] -> result
  | c :: l -> result.[i] <- c; imp (i + 1) l in
  imp 0 l;;
(* I don't know why bytes are returned *)

let replace s =
  let l = explode s in
  let f x = if x = '-' then ' ' else x in
  let l' = List.map f l in
  implode l';;

(********** 3 **********)

(* a *)

let app l x = List.map (fun f -> f x) l;;

(* b *)

(* Type them yourself! :-) *)

(********** 4 **********)

(* Way too hard! Email him to get the answer! *)
