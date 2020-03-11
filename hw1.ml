(*Siddhanth Patel *)
(*I pledge my honor that I have abided by the Stevens Honor System *)
type coord = int*int
type coded_pic = coord list

let cp1:coded_pic = [(0,0);(2,0);(2,2);(0,2);(0,0)]
let cp2:coded_pic = [(0,0);(4,0);(4,4);(0,0)]
let cp3:coded_pic = [(2,2);(0,2);(0,0);(2,0);(2,2)]
let cp4:coded_pic = [(0,0);(1,0);(2,0);(2,2);(0,2);(0,0)]
let cp5:coded_pic = [(0,1);(5,1);(0,2)]


let intmult ((x,y):coord) (fact:int) : coord = (x * fact, y * fact)

let rec stretch (p:coded_pic) (factor:int) : coded_pic = match p with
	| [] -> []
	| h :: t -> intmult h factor :: stretch t factor

let rec map: (int -> int -> int) -> int -> coord -> coord = fun f factor l ->
    match l with
    | (x,y) -> (f factor x, f factor y)

let intmult2 x y = x * y
let intmult' = map intmult2

let rec stretchm (p:coded_pic) (factor:int) : coded_pic = match p with
	| [] -> []
	| h :: t -> intmult' factor h :: stretch t factor

let rec firstquad (cx,cy) (nx,ny) =
    if compare cx nx = 0 then [(cx,cy)] else (cx,cy) :: firstquad (cx+1,cy+1) (nx,ny) 
    
let rec firstquad (cx,cy) (nx,ny) =
    if compare cx nx = 0 then [(cx,cy)] else (cx,cy) :: firstquad (cx+1,cy+1) (nx,ny) 
    
let rec secondquad (cx,cy) (nx,ny) =
    if compare cx nx = 0 then [(cx,cy)] else (cx,cy) :: secondquad (cx-1,cy+1) (nx,ny) 
    
let rec thirdquad (cx,cy) (nx,ny) =
    if compare cx nx = 0 then [(cx,cy)] else (cx,cy) :: thirdquad (cx-1,cy-1) (nx,ny) 
    
let rec fourthquad (cx,cy) (nx,ny) =
    if compare cx nx = 0 then [(cx,cy)] else (cx,cy) :: fourthquad (cx+1,cy-1) (nx,ny)

let rec rightx (cx,cy) (nx,ny) =
    if compare cx nx = 0 then [(cx,cy)] else (cx,cy) :: rightx (cx+1,cy) (nx,ny)

let rec leftx (cx,cy) (nx,ny) =
    if compare cx nx = 0 then [(cx,cy)] else (cx,cy) :: leftx (cx-1,cy) (nx,ny)

let rec northy (cx,cy) (nx,ny) =
    if compare cy ny = 0 then [(cx,cy)] else (cx,cy) :: northy (cx,cy+1) (nx,ny)

let rec southy (cx,cy) (nx,ny) =
    if compare cy ny = 0 then [(cx,cy)] else (cx,cy) :: southy (cx,cy-1) (nx,ny)
    
let segment (cx,cy) (nx,ny) =
    if compare cx nx < 0 then (if compare cy ny < 0 then firstquad (cx+1,cy+1) (nx,ny) else (if compare cy ny > 0 then fourthquad (cx+1,cy-1) (nx,ny) else rightx (cx+1,cy) (nx,ny))) else (if compare cx nx > 0 then (if compare cy ny < 0 then secondquad (cx-1,cy+1) (nx,ny) else (if compare cy ny > 0 then thirdquad (cx-1,cy-1) (nx,ny) else leftx (cx-1,cy) (nx,ny))) else (if compare cy ny < 0 then northy (cx, cy+1) (nx,ny) else southy (cx, cy-1) (nx,ny)))
    
let rec secondcov (p:coded_pic) (x:coord) : coded_pic = match p with
    | [] -> []
    | h :: t -> segment x h @ secondcov t h
	
let coverage (p:coded_pic):coded_pic = match p with
    | [] -> []
    | h :: t -> h :: secondcov t h

let covfun (x:coord) (p:coded_pic) : coded_pic = match p with
    | [] -> []
    | h::t -> x :: segment x h

let covfun' = foldr covfun

let rec foldr: ('a -> 'b  -> 'b)  -> 'b -> 'a list -> 'b = fun f a l ->  match l with  | [] -> a  | h::t -> f h (foldr f a t)

let coverage_f (p:coded_pic):coded_pic = match p with
    | [] -> []
    | h :: t -> h :: secondcov t h

let lexicographic_compare (x,y) (x',y') =
  let compare_fst = compare x x' in
  if compare_fst <> 0 then compare_fst
  else compare y y'

let rec remove_dup (start:coord) (p:coded_pic) : coded_pic = match p with
	| [] -> []
	| h::t -> if start = h
			  then remove_dup h t
			  else start :: remove_dup h t

let feeder (p:coded_pic): coded_pic = match p with
	| [] -> []
	| h::t -> remove_dup h t

let equivalent_pics (cp1:coded_pic) (cp2:coded_pic):bool = List.sort lexicographic_compare(feeder (coverage cp1)) = List.sort lexicographic_compare((feeder (coverage cp2)))

let heighth ((cx,y1):coord) ((nx,y2):coord) : int = if compare y1 y2 < 0
                                  then y2 - y1
                                  else (if compare y1 y2 > 0 then y1 -y2 else 0)

let rec feeder: (int*int) list -> (int*int) list -> int = fun list1 list2 -> match list1,list2 with
    | [],[] -> failwith "error"
    | _::_, [] -> failwith "error"
    | [], _::_ -> failwith "error"
    | h::t,z::l -> heighth h z

let height (p:coded_pic): int = match p with
    | [] -> failwith "error"
    | [(x,y)] -> 0
    | h::t -> feeder (List.sort lexicographic_compare p) (List.rev (List.sort lexicographic_compare p))

let widtht ((cx,y1):coord) ((nx,y2):coord) : int = if compare cx nx < 0
                                  then nx - cx
                                  else (if compare cx nx > 0 then cx - nx else 0)

let rec feeder': (int*int) list -> (int*int) list -> int = fun list1 list2 -> match list1,list2 with
    | [],[] -> failwith "error"
    | _::_, [] -> failwith "error"
    | [], _::_ -> failwith "error"
    | h::t,z::l -> widtht h z

let width (p:coded_pic): int = match p with
    | [] -> failwith "error"
    | [(x,y)] -> 0
    | h::t -> feeder' (List.sort lexicographic_compare p) (List.rev (List.sort lexicographic_compare p))


let rec tilet' (z:int) (p:coded_pic list) : coded_pic list list= match p with
    | [] -> failwith "error"
    | h::t -> if z = 0
              then []
              else p :: tilet' (z-1) p

let rec tilet (z:int) (p:coded_pic) : coded_pic list = match p with
    | [] -> failwith "error"
    | h::t -> if z = 0
              then []
              else p :: tilet (z-1) p

let tile ((dx,dy):coord) (p:coded_pic) : coded_pic list list = tilet' dy (tilet dx p)

let checksigns ((cx,cy):coord) ((nx,ny):coord):int = if compare cx nx < 0 then (if compare cy ny < 0 then 1 else (if compare cy ny > 0 then 4 else 8)) else (if compare cx nx > 0 then (if compare cy ny < 0 then 2 else (if compare cy ny > 0 then 3 else 7)) else (if compare cy ny < 0 then 5 else 6))

let tri_aligned ((x1,y1):coord) ((x2,y2):coord) ((x3,y3):coord):bool =
	if ((checksigns (x1,y1) (x2,y2)) = 5 || (checksigns (x1,y1) (x2,y2)) = 6 || (checksigns (x1,y1) (x3,y3)) = 5 || (checksigns (x1,y1) (x3,y3)) = 6)
	then (x1 = x2) && (x1 = x3)
	else (if ((checksigns (x1,y1) (x2,y2)) = 7 || (checksigns (x1,y1) (x2,y2)) = 8 || (checksigns (x1,y1) (x3,y3)) = 7 || (checksigns (x1,y1) (x3,y3)) = 8) then (y1 = y2) && (y1 = y3) else (((heighth (x1,y1) (x2,y2)) / (widtht (x1,y1) (x2,y2))) = ((heighth (x1,y1) (x3,y3)) / (widtht (x1,y1) (x3,y3)))) && (checksigns (x1,y1) (x2,y2)) = (checksigns (x1,y1) (x3,y3)))

let rec feed' (z:int) (a:coord) (b:coord) (c:coord) (p:coded_pic):bool = match p with
    | [] -> false
    | h::t -> if z = 1
              then tri_aligned a b h
              else (if z = 3 then (feed' 2 h b c t) else (feed' 1 a h c t))
              
let removecase (a:coord) (p:coded_pic):coded_pic = match p with
	| [] -> []
	| h::t -> [a] @ t

let rec compress (p:coded_pic):coded_pic = match p with
    | [] -> []
    | h::t -> if feed' 3 (0,0) (0,0) (0,0) p
              then compress (removecase h t)
              else h :: compress t