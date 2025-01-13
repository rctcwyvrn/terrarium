open! Core

let input =
  {| 
module Test = struct 

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
