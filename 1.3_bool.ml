(* GRADE:  100% *)
let is_zero x =
  if(x==0) then true
  else  false;;

let msg_zero x =
  if (x==0) then "zero"
  else "not zero";;


let my_max a b =
  if(a>b) then a
  else b;;

let max_triple a b c =
  if(a>b) then my_max a c
  else my_max b c;;

let max_quadruple a b c d =
  let z= max_triple a b c in my_max z d;;
"Remplacer cette chaîne par votre code"

