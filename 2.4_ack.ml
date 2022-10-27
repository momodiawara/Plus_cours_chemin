(* GRADE:  100% *)
let rec ack m n =
  if(m=0 && n>=0) then n+1
  else if(m>0 && n=0) then ack (m-1) 1
  else ack(m-1) (ack m (n-1));;
"Remplacez cette chaîne par votre code"

