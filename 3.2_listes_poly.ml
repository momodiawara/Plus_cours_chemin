(* GRADE:  100% *)
(* Toutes les fonctions sont ici définies par des "let rec". Il est
   possible que dans vos solutions certaines fonctions ne soient pas
   récursives, car elles utilisent une fonction récursive auxiliaire
   définie localement. À vous d'enlever le mot clef "rec" dans ce
   cas. *)

let rec map f l = match l with
    []-> []
  |a::b -> f a :: (map f b);;
"Remplacer cette chaîne par votre code" ;;

let rec filter p l = match l with
    []-> []
  |a::b -> 
      if(p a) then a:: filter p b
      else filter p b;;

let rec append l1 l2 = match l1 with
    []-> l2
  |a::b -> a:: append b l2;;
"Remplacer cette chaîne par votre code" ;;
let rec ajout_fin(l,e) = match l with
    []   -> [e]
  |p:: r -> p:: (ajout_fin(r,e));;

let rec rev l = match l with
    []->[]
  |a::b -> ajout_fin (rev b,a);; 

let rec flatten l = match l with
    []->[]
  |a::b-> append a (flatten b);;

let rotation_d l =
  let rec aux l1 acc = match l1 with
      [] -> acc 
    | [x]-> [x] @ acc
    |a::q -> aux q (acc @[a])
  in aux l [] ;; 

