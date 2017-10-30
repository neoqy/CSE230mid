(********** 1 **********)

(* a *)
let count l x =
  let f acc elem = if elem == x then (acc + 1) else acc in
List.fold_left f 0 l;;

(* b *)
let make_palyndrome l =
  let f acc elem = elem::acc in
List.fold_left f l l;;

(********** 2 **********)

(* a *)
let fold_2 f b l =
  let rec helper acc remain index = match remain with
  | [] -> acc
  | h::t -> helper (f acc h index) t (index + 1)
in helper b l 0;;
(* (* TEST *)
 * # let f base elem i = base ^ elem ^ (string_of_int i);;
 * val f : string -> string -> int -> string = <fun>
 * # fold_2 f "" ["a"; "b"; "c"];;
 * - : string = "a0b1c2"
 *)

(* b *)
let rec ith l i d =
  let f acc remain index = if index == i then remain else acc in
fold_2 f d l;;

(********** 3 **********)

(* DEFINITION OF TREE *)
type 'a fun_tree =
  | Leaf of ('a -> 'a)
  | Node of ('a fun_tree) * ('a fun_tree)

(* a *)
let rec apply_all t x =
  match t with
  | Leaf f -> f x
  | Node (t1, t2) -> let temp = apply_all t1 x in apply_all t2 temp;;

(* b_1 *)
let f1 = (+) 1;;
let f2 = (-) 2;;
let f3 = (+) 3;;
let t = Node(Node(Leaf f1, Leaf f2), Leaf f3);;
apply_all t 0;;
(* ANSWER: 4 *)

(* b_2 *)
let f1 = (^) "a";;
let f2 x = x ^ "b";;
let f3 x = x ^ "ab";;
let t = Node(Leaf f1, Node(Leaf f2, Leaf f3));;
apply_all t "123";;
(* ANSWER: "a123bab" *)

(* b_3 *)
let f1 = List.fold_left (fun x y -> (y*2)::x) [];;
let f2 = List.fold_left (fun x y -> x@[y]) [];;
let t = Node(Node(Leaf f1, Leaf f1), Node(Leaf f1, Leaf f2));;
apply_all t [1;2;3];;
(* ANSWER: [4; 8; 12] *)

(* c *)
let rec compose t1 t2 =
  match (t1, t2) with
  | (Leaf f1, Leaf f2) -> Leaf (fun x -> f1 (f2 x))
  | (Node (t1l, t1r), Node (t2l, t2r)) ->
      Node (compose t1l t2l, compose t1r t2r)
  | _ -> Leaf (fun x -> x)
(* (* TEST *)
 * let f1 = (+) 1;;
 * let f2 = (-) 2;;
 * let f3 = (+) 3;;
 * let f4 = (-) 4;;
 * let t1 = Node(Leaf f1, Leaf f2);;
 * let t2 = Node(Leaf f3, Leaf f4);;
 * let t3 = compose t1 t2;;
 * apply_all t3 0;;
 * - : int = 2
 *)
