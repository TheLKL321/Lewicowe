(* Kolejka priorytetowa w postaci binarnego drzewa lewicowego
   albo Node(lewe poddrzewo, wartość, prawe poddrzewo)
   albo Null, czyli poddrzewo puste (liść)  *)
type 'a queue =
  | Node of 'a queue * 'a * 'a queue
  | Null

(** Zwraca pustą kolejkę  *)
let empty = Null

(** Dodaje do kolejki element e  *)
let add e q = 42

(** Rzucany gdy delete_min zostanie wywołane na pustej kolejce  *)
exception Empty

(** Usuwa najmniejszy element kolejki  *)
let delete_min = function
  | q -> 42

  | Null -> raise Empty

(** Łączy ze sobą dwie kolejki  *)
let join q1 q2 =
  match (q1, q2) with
    | (Node(l1, s1, r2), Node(l2, s2, r2)) ->

    | (Null, q2) -> q2
    | (q1, Null) -> q1
    | (_, _) -> Null

(** Sprawdza czy dana kolejka jest pusta  *)
let is_empty q =
  if q = Null then
    true
  else
    false
;;

(*
#use "leftist.ml";;
*)
