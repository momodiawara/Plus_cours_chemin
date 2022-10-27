(* GRADE:  100% *)
(* Toutes les fonction sont ici définies par des "let rec". Il est
   possible que dans vos solutions certaines fonctions ne soient pas
   récursives, car elles utilisent une fonction recursive auxiliaire
   définie localement. À vous d'enlever le mot clef "rec" dans ce
   cas. *)

let rec list_length l = match l with
    []-> 0
  | a::b -> 1+list_length b;;
  
let rec list_product l = match l with
    [] -> 1
  |a::b-> a*list_product b;;
let rec mem x l = match l with
    [] -> false
  | a::b -> if(a==x) then true else mem x b;;

let min x y = if(x<y) then x else y;;

let rec list_min l = match l with
    []-> failwith " Liste vide"
  |[x]-> x
  |a::b -> min a (list_min b) ;;

let rec last l = match l with
    []-> failwith "La liste ide n'a pas d'element"
  |[a]-> a
  |a::b -> last b;;

let rec is_sorted l = match l with
    []->true
  |[a]-> true
  |a::b::c-> if(a>b) then false else is_sorted (b::c);;
let rec sum l = match l with
    []->0
  |a::b -> a+sum b;;
let average l =
  if( list_length l== 0 ) then failwith " la Liste est vide " 
  else sum l / list_length l;; 

"pour parcourir la Liste qu'une seule fois"
let rec nth l k =
  match l with
    []-> failwith" La liste est vide"
  |a::b -> if(k==0) then a else nth b (k-1);;
"Remplacez cette chaîne par votre code" ;;

let rec range n m = 
  if(n=m) then [n]
  else if(n<m) then n::(range (n+1) m)
  else n ::(range (n-1) m);;  


