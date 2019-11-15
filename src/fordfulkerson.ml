open Graph
open Gfile
open Tools


let rec findpath gr id1 id2 =
  let rec parcourProfond acuV = function
    | [] -> false
    | x :: rest -> if x = id2 then true
      else 
        let rec addOutArcPV acu = function
          | [] -> acu
          | (x,y) :: rest -> addOutArcPV (x::acu) rest
        in
        parcourProfond (x::acuV) (addOutArcPV rest (out_arcs gr x))
  in 
  parcourProfond [] [id1]

