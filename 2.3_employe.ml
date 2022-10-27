(* GRADE:  100% *)
let rec pasA n =
  if (n=0) then 0
  else 1+pasB (n-1) 
and pasB n =
  if (n<=0) then 0
  else if(n mod 2=0) then 1+pasA(n-2)
  else 1+pasA(n-1);;
"Remplacer cette chaîne par votre code"

