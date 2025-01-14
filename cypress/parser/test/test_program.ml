open! Core

let short = {|
module Test : sig type t end = struct type t = int end
|}

let full_program =
  {| 
module Test : sig 
  type t 
  
  val x_coord : t -> int 
  val y_coord : t -> int 
  val apply : t -> int 
  val coord_name : string 
  
end = struct 

  type t = { x : int ; y : int  ; f : unit -> int} 


  let x_coord t = t.x
  ;;

  let y_coord = fun t -> t.y
  ;;

  let apply t = t.f ()
  ;; 

  let coord_name = "my name" 
  ;;
end
|}
;;
