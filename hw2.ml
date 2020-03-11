(*Siddhanth Patel *)
(*I pledge my honor that I have abided by the Stevens Honor System *)

type dTree = Leaf of int | Node of char * dTree * dTree

let  tLeft = Node('w',
					Node('x',Leaf(2),Leaf(5)),
					Leaf(8))
let  tRight = Node('w',
					Node('x',Leaf(2),Leaf(5)),
					Node('y',Leaf(7),Leaf(5)))

let  texample = Node('w',
					Node('x',Node('x',Leaf(2),Leaf(5)),Node('x',Leaf(2),Leaf(5))),
					Node('y',Node('x',Leaf(2),Leaf(5)),Leaf(5)))

let  chart1 = (['w';'x'], [([0;0], 200);([0;1], 500)])
let chart2 = [([0;0], 200);([0;1], 500)]
let chart3 = (['w';'x';'y'])

let rec dTree_height = function
	| Leaf x -> 0
	| Node(data, left, right) -> 1 + max (dTree_height(left)) (dTree_height(right))

let rec dTree_size = function
	| Leaf x -> 1
	| Node(data, left, right) -> 1 + dTree_size left + dTree_size right

let append x y = x :: y

let rec dTree_paths = function
	| Leaf x -> [[]]
	| Node(data, left, right) -> (List.map (append 0) (dTree_paths left)) @ (List.map (append 1) (dTree_paths right))

let rec dTree_is_perfect = function
	| Leaf x -> true
	| Node(data, left, right) -> if (dTree_height left < 2) || (dTree_height right < 2)
								then (List.length (dTree_paths left)) = (List.length (dTree_paths right))
								else (dTree_is_perfect left) && (dTree_is_perfect right)

let rec dTree_map (f:char->char) (g:int->int) (t:dTree) : dTree = match t with
	| Leaf x -> Leaf (g x)
	| Node(data, left, right) -> Node(f data, dTree_map f g left, dTree_map f g right)

let rec list_to_tree = function
	| [] -> Leaf(0)
	| h::t -> Node(h, (list_to_tree t), (list_to_tree t))

let rec replacett (p:dTree) (l:int list) (z: int) : dTree = match p, l with
	| Leaf x, [] -> Leaf z
	| Node(data, left, right), h::t -> if h = 0
									   then Node(data, replacett left t z, right)
									   else Node(data, left, replacett right t z)
	| Node (_,_,_),[] -> failwith "error"
	| Leaf _, _::_ -> failwith "error"

let rec replace_leaf_at(p:dTree) (z:(int list * int) list) : dTree = match p, z with
	| p, [] -> p
	| p, (a,b)::t -> replace_leaf_at (replacett p a b) t

(* let replace_leaf_at (p:dTree) ((a,b): char list * (int list * int) list) : dTree = replacet p b *)

let bf_to_dTree (a,b) = replace_leaf_at (list_to_tree a) b
