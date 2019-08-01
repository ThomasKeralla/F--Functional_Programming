
// ------- Question 1 -------

  type OrderedList<'a when 'a : equality> =
    {front: 'a list;
     rear: 'a list}

let ol1 = {front = ["Hans"; "Brian"; "Gudrun"]; rear = []}
let ex = {front = ['x']; rear = ['z';'y']}

let ol2 = {front = ["Hans"; "Brian"]; rear = ["Gudrun"]}
let ol3 = {front = ["Hans"]; rear = ["Gudrun";"Brian"]}

let ol = {front = [3;4;5;6]; rear = [1;2]}

// 4 permutations

let canonical ol = 
    match ol with
    | {front=f;rear=r} -> {front = f  @ r; rear = []}
    | _ -> failwith "error"

let toList ol = 
    match ol with 
    | {front=f;rear=r} -> f @ List.rev r
    | _ -> failwith "error"
 
let newOl = {front = []; rear = []}

let isEmpty ol =
    match ol with 
    | {front=f;rear=r} ->
        match f,r with 
        |[],[] -> true
        |_,_ -> false 
    | _ -> failwith "error" 


let addFront v  = function {front=f;rear=r} -> {front = v::f; rear =r}

let ex = addFront "what" ol1;;

let removeFront ol =
    match ol with
    | {front=f;rear=r} ->
        match f with
        | [] -> failwith "empty front"
        | x::f -> x ,{front=f;rear =r}
let ex2 = removeFront ol1;;

let peekFront ol =
    match ol with
    | {front=f;rear=r} ->
        match f with
        | [] -> failwith "empty front"
        | x::f -> x ,{front=x::f;rear =r}

let ex3 = peekFront ol1;;
        

let append ol1 ol2 =
    match ol1,ol2 with
    |{front=f1;rear=r1},{front=f2;rear=r2} -> 
        match f1,r1,f2,r2 with
        |[],[],[],[] -> newOl
        |x,[],[],[] -> {front=x;rear=[]}
        |x,y,[],[] -> ol1
        |x,y,z,[] -> {front=x@z;rear=y}
        |x,y,z,q -> {front=x@z;rear=y@q}

let ex4 = append ol2 ol2;;

let map f ol = 
    if isEmpty ol then newOl else 
        match ol with
        | {front=f1;rear=r1} -> {front = f1|> List.map f; rear = r1|> List.map f}
        | _ -> failwith "error"

let ex5 = map (fun e -> e) ex

let fold f acc ol =
    List.fold f acc (toList ol)
    
let ex6 = fold (fun acc e -> acc + e.ToString()) "" ex

let multiplicity ol =
    List.fold (fun m x -> if Map.containsKey x m then Map.add x ((Map.find x m)+1) m else Map.add x 1 m ) Map.empty (toList ol)
let ex7 = multiplicity (addFront 'x' ex)
(*
let multiplicity ol = 
  let addMult m e = 
    match Map.tryFind e m with
    | Some i -> Map.add e (i+1) m
    | None -> Map.add e 1 m
  fold addMult Map.empty ol
let ex7 = multiplicity (addFront 'x' ex)
*)

// ------ Question 2  ------

// 2.1

  let rec f i = function
    | [] -> [i]
    | x::xs -> i+x :: f (i+1) xs

let ex = f 10 [0;1;2;3]
// f computes the summation of the value const out of the list given and the value i, where i is incremented 
// for each recursive call

// it can never be the empty list as the basecase is [i]

// if f is given a finite list it can never go into a infinite loop as one element is removed at every call
// until the basecase [] is reached

let fA i list = 
    let rec inn i' list' acc =
        match list' with
        | [] -> List.rev (i'::acc )
        | x::xs -> inn (i'+1) xs ((x+i')::acc)
    inn i list []

let ex2 = fA 10 [0;1;2;3]

let fC i list =
    let rec inn i' list' c =
        match list' with
        | [] -> c [i']
        | x::xs -> inn (i'+1) xs (fun acc -> c((x+i')::acc))
    inn i list id

let ex3 = fC 10 [0;1;2;3]

// ----- Question 3 -----

// 3.1
// the seq returns m values starting at n and and adding n every time,  -> myFinSeq 2 4 = 2,4,6,8

let mySeq n =
    Seq.initInfinite (fun x -> n+n*x)
//let ex4 = mySeq 2;;

let multTable N M =
    Seq.init (if N < M then M else N) (fun x -> (N-x,M-x,(N-x)*(M-x)))
//let ex5 = multTable 5 4;;

let ppMultTable N M = Seq.map (fun (n,m,nm) -> sprintf "%d * %d is %d" n m (n*m)) (multTable N M)
let ex6= Seq.take 4 (ppMultTable 10 10)


// ----- Question 4 -------

type opr = MovePenUp
         | MovePenDown
         | TurnEast
         | TurnWest
         | TurnNorth
         | TurnSouth
         | Step

type plot =
            | Opr of opr
            | Seq of plot * plot

let side = Seq(Opr MovePenDown, Seq(Opr Step, Seq(Opr Step, Opr Step)))

  let rect = Seq(Seq(Opr TurnEast, side),
               Seq(Opr TurnNorth, Seq(side,
                 Seq(Opr TurnWest, Seq(side,
                   Seq(Opr TurnSouth, side))))))

let rec ppOpr o =
        match o with
        | MovePenUp -> "MovePenUp"
        | MovePenDown -> "MovePenDown"
        | TurnEast -> "TurnEast"
        | TurnWest -> "TurnWest"
        | TurnNorth -> "TurnNorth"
        | TurnSouth -> "TurnSouth"
        | Step -> "Step"
        | _ -> failwith "wrong input"

let ppOprPlot p =
    let rec inn p' acc =
        match p' with 
        | Opr(o) -> (ppOpr o) + " => " + acc
        | Seq(x,_) -> inn x acc
        | Seq(_,y) -> inn y acc
    inn p ""

let ppOprPlot p =
    let rec inn p' acc =
        match p' with 
        | Opr(o) -> (ppOpr o) + " => " + acc
        | Seq(x,y) -> inn x acc + inn y acc
    inn p ""

// Question 4.2

type dir = North
           | South
           | West
           | East
  type pen = 
           | PenUp
           | PenDown
  type coord = int * int
  type state = coord * dir * pen

let goStep (s: state) = 
    match s with
    | (coord,dir,pen) ->
         match (coord,dir,pen) with  
         | (x,y),North, pen ->  ((x,y+1), North, pen) |> state
         | (x,y),West, pen ->  ((x-1,y), West, pen) |> state
         | (x,y),South, pen ->  ((x,y-1), South, pen) |> state
         | (x,y),East, pen ->  ((x+1,y), East, pen) |> state
  
      
let initialState = ((0,0),East,PenUp)


let addDot s l o =
  match s,o with
  | (coord,dir,pen),MovePenUp -> (l, (coord,dir,PenUp) |> state) 
  | (coord,dir,pen),MovePenDown -> (coord::l, (coord,dir,PenDown) |> state) 
  | (coord,dir,pen),_ -> 
    match o with
    |TurnEast -> (l, (coord,East,pen) |> state)
    |TurnWest -> (l, (coord,West,pen) |> state) 
    |TurnNorth -> (l, (coord,North,pen) |> state)
    |TurnSouth -> (l, (coord,South,pen) |> state)
    |Step -> let (c,dir',pen') = (goStep (coord,dir,pen)) 
             (if pen'=PenDown then (c::l ,((c,dir',pen') |> state)) else (l, ((c,dir',pen') |> state)))
    |_-> failwith "error"

let (coords1,s1) = addDot initialState [] MovePenDown
let (coords2,s2) = addDot s1 coords1 Step
let (coords3,s3) = addDot s2 coords2 TurnSouth
let (coords4,s4) = addDot s3 coords3 Step

let dotCoords p =
  let rec dotCoords' p (acc,((x,y),dir,pen)) =
    match p with
      | Opr opr -> addDot ((x,y),dir,pen) acc opr
      | Seq(p1, p2) -> let (acc,((x,y),dir,pen)) = dotCoords' p1 (acc,((x,y),dir,pen)) 
                       dotCoords' p2 (acc,((x,y),dir,pen))
  dotCoords' p ([],((0,0),East,PenUp)) 

let uniqueDotCoords p = 
  let (x,r) = dotCoords p 
  match x,r with
  | x,s -> ((Set.ofList x),s)
  | _ -> failwith "error"

