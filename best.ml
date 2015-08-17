(* 
Problema del postino di campagna (best first):
Siano G=(V,E) un grafo in cui ogni arco ha associata una lunghezza, un sottoinsieme E' degli archi ed un intero K.
Dato un vertice V0 determinare, se esiste, un circuito (cammino chiuso da V0 a V0) che include tutti gli archi di E’ ed ha lunghezza inferiore a K.
Si risolva il problema utilizzando una ricerca best first.
Come funzione di valutazione si può usare il numero di archi di E’ presenti nel circuito.
*)
(*
DA FARE:
-presentare almeno 3 casi 
-calcolare complessità
-studiare tutti gli algoritmi di ricerca
*)
(*funzione euristica: metto in testa gli archi che fanno parte di insiemeArchiObiettivo, minimizzando però il numero di passaggi*)

(* rappresentazione esplicita del grafo *)
let f = function
1 -> [2;5]
| 2 -> [1;3;5]
| 3 -> [2;4;6]
| 4 -> [3]
| 5 -> [1;2;6]
| 6 -> [3;5;7]
| 7 -> [6]
| _ -> [];;

type 'a graph = Graph of ('a -> 'a list);;
let g = Graph f;;

(*let grafo = [(1,2);(1,5);(2,1);(2,3);(2,5);(3,2);(3,4);(3,6);(4,3);(5,1);(5,2);(5,6);(6,3);(6,5);(6,7);(7,6)];;*)

(*devo passare per questi archi*)
let insiemeArchiObiettivo = [(3,4),(3,6),(5,6)];;

(*lunghezza massima*)
let k = 10;;

(*controllo obiettivo*)
let checkFinito x listaAttraversati = 
	if x<=k
	then 1 else 0;;

print_int (checkFinito 11 insiemeArchiObiettivo);;
(*lunghezze degli archi*)
let lunghezze = [ ((1,2),3); ((1,5),6); ((2,1),4); ((2,3),5); ((2,5),6); ((3,2),5); ((3,4),4); ((3,6),3); ((4,3),4); ((5,1),5); ((5,2),6); ((5,6),5); ((6,3),4); ((6,5),5); ((6,7),6); ((7,6),5) ];;

let rec stampalista = function [] -> print_newline()
| x::rest -> print_int(x); print_string("; "); stampalista rest;;

let cercaLunghezza listaLunghezze x y =
	let ris = -1
	in let rec aux sottoLista = match sottoLista with
 		[] -> ris
 		| z::rest ->
 			if (fst z = (x,y)) then
				snd z
 			else
 				aux(rest)
	in aux(listaLunghezze);;


let piuvicino (cammino1, cammino2, meta) =
(cercaLunghezza lunghezze (List.hd cammino1) meta) < (cercaLunghezza lunghezze (List.hd cammino2) meta);;

let confrontacammino cammino1 cammino2 meta=
if List.hd cammino1= List.hd cammino2
then 0
else if piuvicino (cammino1, cammino2, meta)
then -1
else 1;;


exception NotFound;;
let searchbf inizio fine (Graph succ)=
	let estendi cammino = stampalista cammino;
		List.map (function x -> x::cammino) (List.filter (function x -> not (List.mem x cammino)) (succ (List.hd cammino)))
	in let confronta c1 c2 = confrontacammino c1 c2 fine
		in let rec search_aux fine = function
				[] -> raise NotFound
				| cammino::rest -> if fine = List.hd cammino
				then List.rev cammino
				else search_aux fine (List.sort confronta (rest @ (estendi cammino)))
		in search_aux fine [[inizio]];;


(*test*)
(*searchbf 1 7 g;;*)