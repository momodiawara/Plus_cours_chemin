(* GRADE:  100% *)
let rec string_of_formula f = match f with
  |Var x -> x
  |Neg f -> " Neg " ^  (string_of_formula f )
  |And(f1,f2) -> "(" ^ (string_of_formula f1)^ " And "^(string_of_formula f2)^")"
  |Or(f1,f2) ->"("^(string_of_formula f1)^" Or " ^(string_of_formula f2)^")"
;;
 
"Remplacer cette chaîne par votre code"
let rec union_s l1 l2 = 
  if List.length l1 =0 then l2
  else if List.length l2=0 then l1
  else if List.hd l1 < List.hd l2 then List.hd l1 :: union_s (List.tl l1) l2
  else if List.hd l1 = List.hd l2 then union_s (List.tl l1) l2
  else List.hd l2 :: union_s l1 (List.tl l2);;

let rec list_of_vars f = match f with
  |Var x -> [x] 
  |Neg f -> list_of_vars f
  |And (f1,f2) -> union_s (list_of_vars f1) (list_of_vars f2)
  |Or(f1,f2) -> union_s (list_of_vars f1) (list_of_vars f2);;
"Remplacer cette chaîne par votre code"

let rec eval_formula f l = match f with
    Var s -> (List.assoc s l)
  |Neg f ->not( eval_formula f l)
  |Or(f1,f2) -> (eval_formula f1 l) || (eval_formula f2 l)
  |And (f1,f2) -> (eval_formula f1 l) && (eval_formula f2 l);;
"Remplacer cette chaîne par votre code"

