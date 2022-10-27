(* GRADE:  42% *)
let add_to_all x ll = List.map ( fun l -> x::l) ll;;
  
let rec assignments_vars l =  match l with
    [] -> [[]]
  |a::q ->
      let t = assignments_vars q in 
      (add_to_all (a,false) t)@(add_to_all (a, true)t);;

(*
  let rec concat l1 l2 = 
   if List.length l1 =0 then l2
   else if List.length l2=0 then l1
   else if List.hd l1 < List.hd l2 then List.hd l1 :: concat (List.tl l1) l2
   else if List.hd l1 = List.hd l2 then concat (List.tl l1) l2
   else List.hd l2 :: concat l1 (List.tl l2);;


 let  rec list_of_vars f = match f with
     Var x -> [x] 
   |Neg f1 -> list_of_vars f1
   |Or(f1,f2) -> concat (list_of_vars f1) (list_of_vars f2)
   |And(f1,f2) ->concat (list_of_vars f1) (list_of_vars f2);;
  
 let assignments f = assignments_vars (list_of_vars f);;
 "";;
let rec eval_formula f l = match f with
      Var s -> (List.assoc s l)
    |Neg f ->not( eval_formula f l)
    |Or(f1,f2) -> (eval_formula f1 l) || (eval_formula f2 l)
    |And (f1,f2) -> (eval_formula f1 l) && (eval_formula f2 l);;
                           
 "";;
 let satisfiable f =
   let le = assignments f in
   List.exists (eval_formula f ) le;;
 "Remplacer cette chaîne par votre code" ;;

 let tautology f =
  (* let le = assignments f in
    List.for_all (eval_formula f) le;;
   *)    "Remplacer cette chaîne par votre code" ;;*)

