(* Mikolaj Grzywacz
Code Review: Andrzej Gluszak*)

(* lewe poddrzewo * wartosc * prawe poddrzewo * prawa wyskosc *)
type 'a queue =
	| Leaf
	| Node of 'a queue * 'a * 'a queue * int
	
exception Empty

(* konstruktory *)
let singleton x = Node (Leaf, x, Leaf, 1)

let empty = Leaf

(*selektory*)
(*mierzy najmniejsza odleglosc od korzenia do liscia najbardziej z prawej, prawa wyskosc*)
let rank = 
	function
		| Leaf -> 0
		| Node(_,_,_,r) -> r

let is_empty =
	function
		| Leaf -> true
		| Node (_,_,_,_) -> false
		
(*modyfikatory*)
let rec join q_a q_b = 
	match q_a, q_b with
		| Leaf, q -> q
		| q, Leaf -> q
		| Node (l, k_a, r, _), Node (_, k_b, _, _) -> 
			if k_a > k_b then join q_b q_a
			else 
				(*rekurencyjnie laczy poddrzewa*)
				let joined = join r q_b in
				(*bierze najkrotsza droge z lewego poddrzewa*)
				let rank_left = rank l in
				(*bierze najkrotsza droge z lewego poddrzewa*)
				let rank_right = rank joined in
				(*po sklejeniu ustawiamy poddrzewa na "dobra strone"*)
				if rank_left >= rank_right then
					Node (l, k_a, joined, rank_right + 1)
				else
					Node (joined, k_a, l, rank_left + 1)

(*dodaje singleton x do aktualnego drzewa*)
let add x q =
	join (singleton x) q
		
(*usuwa min_element (zwracajac go) i joinuje dwa poddrzewa*)
let delete_min = 
	function
		| Leaf -> raise Empty
		| Node (l,k,r,_) -> (k, (join l r))