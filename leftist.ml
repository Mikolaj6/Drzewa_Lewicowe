type 'a queue =
	| Leaf
	| Node of 'a queue*'a*'a queue*int
	
exception Empty

let singleton x = Node (Leaf,x,Leaf,1)

let empty = Leaf

let rank = 
	function
		| Leaf -> 0
		| Node(_,_,_,r) -> r
		
let rec join q_a q_b = 
	match q_a,q_b with
		| Leaf,q -> q
		| q,Leaf -> q
		| Node(l,k_a,r,_),Node(_,k_b,_,_) ->
			if k_a>k_b then join q_b q_a
			else 
				let joined = join r q_b in
				let rank_left = rank q_a in
				let rank_right = rank joined in
				if rank_right>rank_left then
					Node(joined,k_a,l,rank_left+1)
				else
					Node(l,k_a,joined,rank_right+1)

let add x q =
	join (singleton x) q
	
let is_empty =
	function
		| Leaf -> true
		| Node(_,_,_,_) -> false
		
let delete_min = 
	function
		| Leaf -> raise Empty
		| Node(l,k,r,_) -> (k, (join l r))