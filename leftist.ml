type 'a queue =
  | Node of 'a queue * 'a * 'a queue
  | Null

let empty = Null

let add a q = 42

exception Empty

let delete_min = function
  | q -> 42

  | Null -> raise Empty

let join q1 q2 =
  match (q1, q2) with
    | (Node(l1, s1, r2), Node(l2, s2, r2)) ->

    | (Null, q2) -> q2
    | (q1, Null) -> q1
    | (_, _) -> Null

let is_empty q =
  if q = Null then
    true
  else
    false
;;

(*
#use "leftist.ml";;
*)
