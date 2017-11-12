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

(** Rzucany gdy delete_min zostanie wywołane na pustej kolejce  *)
exception Empty

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

(** Dodaje do kolejki element e  *)
let add e q = join q (Node(Null, e, 0, Null))

(** Zwraca minimalny element kolejki oraz tę samą kolejkę bez owego elementu *)
let delete_min = function
  | Node(l, w, _, r) ->
      (w, join l r)
  | Null -> raise Empty

(** Sprawdza czy dana kolejka jest pusta  *)
let is_empty q =
  if q = Null then
    true
  else
    false
;;

(** TESTY  *)

(* simple tests *)
let a = empty;;
let b = add 1 empty;;

assert (is_empty a = true);;
assert (try let _=delete_min a in false with Empty -> true);;
assert (is_empty b <> true);;

let b = join a b ;;
assert (is_empty b <> true);;

let (x,y) = delete_min b;;

assert (x = 1);;
assert (is_empty y = true);;
assert (try let _=delete_min y in false with Empty -> true);;

(* delete_min integer tests *)
let b = add 1 empty;;
let b = add 3 b;;
let b = add (-1) b;;
let b = add 2 b;;
let b = add 1 b;;

let (a,b) = delete_min b;;
assert (a = -1);;

let (a,b) = delete_min b;;
assert (a = 1);;

let (a,b) = delete_min b;;
assert (a = 1);;

let (a,b) = delete_min b;;
assert (a = 2);;

let (a,b) = delete_min b;;
assert (a = 3);;

assert(is_empty b = true);;

(* delete_min string tests *)
let b = add "a" empty;;
let b = add "aca" b;;
let b = add "nzbzad" b;;
let b = add "nzbza" b;;
let b = add "bxbxc" b;;

let (a,b) = delete_min b;;
assert (a = "a");;

let (a,b) = delete_min b;;
assert (a = "aca");;

let (a,b) = delete_min b;;
assert (a = "bxbxc");;

let (a,b) = delete_min b;;
assert (a = "nzbza");;

let (a,b) = delete_min b;;
assert (a = "nzbzad");;

assert(is_empty b = true);;
assert (try let _=delete_min b in false with Empty -> true);;

(* join tests *)

let b = add 1 empty;;
let b = add 3 b;;
let b = add (-1) b;;
let b = add 2 b;;
let b = add 1 b;;

let c = add 10 empty;;
let c = add (-5) c;;
let c = add 1 c;;
let c = add 4 c;;
let c = add 0 c;;

let b = join b c;;

let (a,b) = delete_min b;;
assert (a = (-5));;

let (a,b) = delete_min b;;
assert (a = (-1));;

let (a,b) = delete_min b;;
assert (a = 0);;

let (a,b) = delete_min b;;
assert (a = 1);;

let (a,b) = delete_min b;;
assert (a = 1);;

let (a,b) = delete_min b;;
assert (a = 1);;

let (a,b) = delete_min b;;
assert (a = 2);;

let (a,b) = delete_min b;;
assert (a = 3);;

let (a,b) = delete_min b;;
assert (a = 4);;

let (a,b) = delete_min b;;
assert (a = 10);;

assert (try let _=delete_min b in false with Empty -> true);;

let b = add 1 empty;;
let b = add 3 b;;
let b = add (-1) b;;
let b = add 2 b;;
let b = add 1 b;;

let c = add 10 empty;;
let c = add (-5) c;;
let c = add 1 c;;
let c = add 4 c;;
let c = add 0 c;;

let b = join c b;;

let (a,b) = delete_min b;;
assert (a = (-5));;

let (a,b) = delete_min b;;
assert (a = (-1));;

let (a,b) = delete_min b;;
assert (a = 0);;

let (a,b) = delete_min b;;
assert (a = 1);;

let (a,b) = delete_min b;;
assert (a = 1);;

let (a,b) = delete_min b;;
assert (a = 1);;

let (a,b) = delete_min b;;
assert (a = 2);;

let (a,b) = delete_min b;;
assert (a = 3);;

let (a,b) = delete_min b;;
assert (a = 4);;

let (a,b) = delete_min b;;
assert (a = 10);;

assert (try let _=delete_min b in false with Empty -> true);;

(* num jest numerem operacji (delete_min) na której wystąpił błąd *)
let test a b num msg =
if a = b then print_endline "ok"
else (print_int num; print_endline msg);;

let rec zwin l q num msg =
try
match l with
| [] -> test q empty num msg
| h::t -> let (mn,r) = delete_min q in test mn h num msg; zwin t r (num+1) msg
with Empty -> (print_int num; print_string "Empty"; print_endline msg);;

let a = add 0. empty;;        (* 0.*)
let b = add 1. empty;;        (* 1. *)
let c = add (-0.1) empty;;    (* -0.1 *)
let d = add 7. a;;            (* 0., 7. *)
let e = add (-3.) d;;         (* -3., 0., 7. *)
let f = add (-0.5) c;;        (* -0.5, -0.1 *)
let g = join b c;;            (* -0.1, 1.*)
let h = join d e;;            (* -3., 0., 0., 7., 7. *)
let i = join f e;;            (* -3., -0.5, -0.1, 0., 7. *)
let j = join h i;;            (* -3., -3., -0.5, -0.1, 0., 0., 0., 7., 7., 7. *)

let la = [0.];;
let lb = [1.];;
let lc = [-0.1];;
let ld = la @ [7.];;
let le = -3.::ld;;
let lf = -0.5::lc;;
let lg = lc @ lb;;
let lh = [-3.; 0.; 0.; 7.; 7.];;
let li = [-3.; -0.5; -0.1; 0.; 7.];;
let lj = [-3.; -3.; -0.5; -0.1; 0.; 0.; 0.; 7.; 7.; 7.];;

test (join empty empty) empty (-1) ": empty + empty";;
zwin la a 0 ": a";;
zwin lb b 0 ": b";;
zwin lc c 0 ": c";;
zwin ld d 0 ": d";;
zwin le e 0 ": e";;
zwin lf f 0 ": f";;
zwin lg g 0 ": g";;
zwin lh h 0 ": h";;
zwin li i 0 ": i";;
zwin lj j 0 ": j";;
