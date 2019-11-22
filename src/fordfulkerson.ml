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


type 'a path = 'a list 

let rec printChemin acu = function
  | [] -> Printf.printf "%s\n%!" acu
  | x :: rest -> printChemin ((string_of_int x)^" "^acu) rest

let rec getPath gr id1 id2 =
  let rec parcourProfond acuV = function
    | [] -> []
    | (x::chemin_to_x) :: rest -> if x = id2 then x::chemin_to_x
      else 
        let rec addOutArcPV acu = function
          | [] -> acu
          | (a,y) :: rest -> 
            if y == 0 
            then addOutArcPV acu rest
            else addOutArcPV ((a::x::chemin_to_x)::acu) rest
        in
        parcourProfond (x::acuV) (addOutArcPV rest (out_arcs gr x))
    | [] :: _ -> failwith "aie"
  in 
  parcourProfond [] [[id1]]

let init_zero gr = 
  let init_zero_in gr id1 id2 lab = add_arc gr id2 id1 0 in
  let gr = e_fold gr init_zero_in gr in
  gr

let rec modif_arcs gr value = function
  | [] -> gr
  | x :: [] -> gr
  | x :: y :: rest -> modif_arcs (add_arc gr x y value) value (y::rest)

let rec flow_val gr acu = function
  | [] -> acu
  | x :: [] -> acu 
  | x :: y :: rest -> let diff = (Option.get (find_arc gr x y)) - 
                                 (Option.get (find_arc gr y x)) 
    in
    if diff < acu 
    then flow_val gr diff (y::rest)
    else flow_val gr acu (y::rest)


let rec flotMax gr id1 id2 = 
  let grB =  gmap gr int_of_string in
  let gr = gmap gr int_of_string in
  let gr = init_zero gr in
  let chemin = getPath grB id1 id2 in
  let rec loop_chemin gr grB = function
    | [] -> gr
    | x :: [] -> gr
    | l -> 
      let value_flow = flow_val gr max_int (List.rev l) in
      let grB = modif_arcs grB ((-) 0 value_flow) (List.rev l) in
      let gr = modif_arcs gr value_flow l in 
      let pat = getPath grB id1 id2 in
      loop_chemin gr grB pat
  in
  let gr = loop_chemin gr grB chemin in
  gmap gr string_of_int 