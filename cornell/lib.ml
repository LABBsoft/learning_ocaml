(* Tools useful in solving CS3110 Exercises *)
(*
  Define a new operator with ()
  now 1 ^^ 50 returns 1 max 50 = 50
*)
let (^^) x y = max x y

(* extremely bad power function *)
let rec pow x y =
  if y=0 then 1 
  else x * pow x (y-1)
