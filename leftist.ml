(* Kolejka priorytetowa w postaci binarnego drzewa lewicowego
   albo Node(lewe poddrzewo, wartość, odległość od liścia, prawe poddrzewo)
   albo Null, czyli poddrzewo puste, liść  *)
type 'a queue =
  | Node of 'a queue * 'a * int * 'a queue
  | Null

(** Zwraca odległość Node'a od Null'a w drzewie  *)
let odleglosc = function
  | Node(l, w, odl, r) ->
      if l = Null || r = Null then
        0
      else
        odl
  | Null -> -1

(** Zwraca pustą kolejkę  *)
let empty = Null

(** Dodaje do kolejki element e  *)
let add e q = 42

(** Rzucany gdy delete_min zostanie wywołane na pustej kolejce  *)
exception Empty

(** Usuwa minimalny element kolejki  *)
let delete_min = function
  | q -> 42

  | Null -> raise Empty

(** Łączy ze sobą dwie kolejki  *)
let rec join q1 q2 =
  match (q1, q2) with
    | (Node(l1, w1, odl1, r1), Node(l2, w2, odl2, r2)) ->
        if w1 <= w2 then
          let right =
            join (Node(l2, w2, odl2, r2)) r1
          in
            let odlr = odleglosc right
            and odll = odleglosc l1
            in if odll >= odlr then
              Node(l1, w1, (min odll odlr) + 1, right)
            else
              Node(right, w1, (min odll odlr) + 1, l1)
        else
          let right =
            join (Node(l1, w1, odl1, r1)) r2
          in
            let odlr = odleglosc right
            and odll = odleglosc l1
            in if odll >= odlr then
              Node(l2, w2, (min odll odlr) + 1, right)
            else
              Node(right, w2, (min odll odlr) + 1, l2)
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
