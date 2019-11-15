open Graph
open Gfile
open String
open Stdlib

let clone_nodes gr = 
  n_fold gr new_node empty_graph 

let gmap gr f = 
  e_fold gr (fun grA id1 id2 l -> new_arc grA id1 id2 (f l)) (clone_nodes gr)

let add_arc gr i j k = 
  match find_arc gr i j with
  |Some x -> new_arc gr i j (k+x)
  |None -> new_arc gr i j k




(* il parcourt l'ensemble des arcs mais si l'arc n'existe pas, il ne vas pas
   faire la fonction et donc l'arc n'est pas crée *)


let gr2 = from_file "graphs/graph1";;

export (gmap (add_arc (gmap gr2 (fun x -> (int_of_string x))) 0 4 99) string_of_int) "./test";;