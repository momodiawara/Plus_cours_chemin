(* GRADE:  68% *)
let rec desc_neg f = match f with
  | Neg(Neg g) -> desc_neg g
  | Neg(Or(g,h)) -> And(desc_neg(Neg g), desc_neg(Neg f))
  | Neg(And(g,h)) -> Or(desc_neg(Neg g), desc_neg(Neg f))
  | Or(g,h) -> Or(desc_neg g, desc_neg h)
  | And(g,h) -> And(desc_neg g, desc_neg h)
  | f -> f;; 

let rec desc_or f = match f with
    And(g,h) -> And( desc_or g, desc_or h)
  |Or(g,h)-> let g1= desc_or g and h1 = desc_or h in 
      (
        match g1,h1 with
          _,And(g,h) -> And( desc_or (Or(g1,g)),desc_or (Or(g1,h)))
        | And(f,g),_ -> And( desc_or (Or(f,h1)),desc_or (Or(g,h1)))
        |_ -> Or(g1,h1)
      )
  |f->f;;

let cnf f = desc_or( desc_neg f);;

