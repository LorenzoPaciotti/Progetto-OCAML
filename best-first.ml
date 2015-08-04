(* 
Problema del postino di campagna (best first):
Siano G=(V,E) un grafo in cui ogni arco ha associata una lunghezza, un sottoinsieme E' degli archi ed un intero K.
Dato un vertice V0 determinare, se esiste, un circuito (cammino chiuso da V0 a V0) che include tutti gli archi di E’ ed ha lunghezza inferiore a K.
Si risolva il problema utilizzando una ricerca best first.
Come funzione di valutazione si può usare il numero di archi di E’ presenti nel circuito.
*)
(* rappresentazione esplicita del grafo *)
let f = function
1 -> [2;5]
| 2 -> [1;3;5]
| 3 -> [2;4;6]
| 4 -> [3]
| 5 -> [1;2;6]
| 6 -> [3;5;7]
|7 -> [6]
| _ -> [];;

type 'a graph = Graph of ('a -> 'a list);;

let rec stampalista = function [] -> print_newline()
| x::rest -> print_int(x); print_string("; "); stampalista rest;;

let coordinate = [ (1, (0,3)); (2, (4,6)); (3, (7,6)); (4, (11,6)); (5, (3,0)); (6, (6,0)); (7, (11,3))];;

let distanza nodo1 nodo2 =
let x1 = float (fst(List.assoc nodo1 coordinate))
in let y1 = float (snd(List.assoc nodo1 coordinate))
in let x2 = float (fst(List.assoc nodo2 coordinate))
in let y2 = float (snd(List.assoc nodo2 coordinate))
in sqrt ( (x1 -. x2)**2. +. (y1 -. y2)**2.);;

let piuvicino (cammino1, cammino2, meta) =
(distanza (List.hd cammino1) meta) < (distanza (List.hd cammino2) meta);;

let confrontacammino cammino1 cammino2 meta=
if List.hd cammino1= List.hd cammino2
then 0
else if piuvicino (cammino1, cammino2, meta)
then -1
else 1;;

exception NotFound;;

let searchbf inizio fine (Graph succ)=
let estendi cammino = stampalista cammino;
List.map (function x -> x::cammino)
(List.filter (function x -> not (List.mem x cammino)) (succ (List.hd cammino)))
in let confronta c1 c2 =
confrontacammino c1 c2 fine
in let rec search_aux fine = function
[] -> raise NotFound
| cammino::rest -> if fine = List.hd cammino
then List.rev cammino
else search_aux fine (List.sort
confronta
(rest @ (estendi cammino)))
in search_aux fine [[inizio]];;
