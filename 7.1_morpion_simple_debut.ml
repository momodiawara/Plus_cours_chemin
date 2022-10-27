(* GRADE:  100% *)
let plateau_initial (taille : int) : plateau =
  Matrix.init taille (fun x -> None);;
  
let coup_legal (plateau : plateau) (coup : coup) : bool =
  Matrix.get plateau(coup.position)= None;;
  
let coup_des_coordonnees_absolues
    (plateau : plateau)
    (dernier_coup : coup option)
    (nb_joueurs : int)
    (x , y : int*int)
  : coup option =
  if ( x<0 || x>= Matrix.size(plateau) || y<0 || y>= Matrix.size(plateau))
  then None
  else 
    match dernier_coup with
    |None-> Some{joueur =0; position= (x,y)}
    |Some s-> 
        if(s.joueur+1>= nb_joueurs ) then Some ({joueur=0 ;position=(x,y)})
        else Some({ joueur= (s.joueur+1); position=(x,y)})
  
;;

