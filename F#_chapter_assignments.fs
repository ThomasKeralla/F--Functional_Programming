open System 
open System.Xml.Xsl
// F# chapter exercises

//chapter 1

//1.1
let g n =  n + 4;;

//1.2
let h (x:float,y:float) = System.Math.Sqrt((x*x) + (y*y));;

//1.3
let g2 = fun n -> n + 4;;
let h2 = fun (x,y) -> System.Math.Sqrt((x*x) + (y*y));; 

//1.4
let rec f = function
    | 0 -> 0
    | x -> x + f (x - 1)
    ;;

let a = f 3;;

//1.5
let rec fib = function
        | 0 -> 0
        | 1 -> 1
        | n -> (fib (n-1)) + (fib (n-2));;
 
 let rec sum = function 
    | (m,0) -> m
    | (m,n) -> m + sum ((m+1),(n-1))


// chapter 2

//2.1
let f2 x = 
    if (x % 2 = 0 && x % 3 = 0) && x % 5 <> 0 then true else false;;

//2.2
let rec pow (x,y):string = 
    match (x,y) with
    | (x,0) -> ""
    | (x,y) -> x + pow (x,y-1)

//2.3
let isIthChar (str:string,i,ch) = if str.[i] = ch then true else false;;

// 2.4 
let rec occFromIth (str:string,i,ch) =
    let s = String.length str
    let rec inner i j s = 
        match i with
        | 0 -> j
        | i -> if i >= str.Length then j elif str.[i] = ch then inner (i+1) (j+1) s else inner (i+1) j s
    inner i 0 s

let res = occFromIth ("abbbcdbb",5,'b');;

// 2.5
let rec occInString = function
    | ("",ch) -> 0
    | (str,ch) -> if str.[0] = ch then 1 + occInString (str.Substring 1, ch) 
                    else 0 + occInString (str.Substring 1, ch)

let testt = occInString ("abccbc",'c');;

// 2.6
let notDivisible (x,y) = y % x <> 0 

// 2.7

let rec test (a,b,c) = 
    match a with
    | _ when a >= b -> notDivisible (b,c)
    | a -> notDivisible(a,c) && test ((a+1),b,c)

let t = test (3,5,12);; // false
let t2 = test (3,5,11);; // true

let prime n = 
    match n with
    | 1 -> true
    | 2 -> true
    | 3 -> true
    | _ when n < 1 -> false
    | _ -> test (2,(n/2),n)

let rec nextPrime n = if prime (n + 1) then (n + 1) else nextPrime (n + 1) 

// 2.8

// 3.1

type AMPM = AM | PM

type Time = {
    AmPm : AMPM;
    Hour : int;
    Minute : int;
}

let t1 = {AmPm = AM; Hour = 3; Minute = 5}
let t2 = {AmPm = AM; Hour = 5; Minute = 3}
let t3 = {AmPm = PM; Hour = 5; Minute = 3}

let r1 = t1 < t2;; // True
let r2 = t2 < t3;; // True
let r3 = t3 < t1;; // false

// just for fun pattern match solution for records, not needed as records can be compared using operators
// meaning this function is absolutely redundent 
let compareTime x y =
    match x,y with
    | {AmPm=AM;Hour=_;Minute=_},{AmPm=PM;Hour=_;Minute=_} -> true
    | {AmPm=PM;Hour=_;Minute=_},{AmPm=AM;Hour=_;Minute=_} -> false
    | {AmPm=AM;Hour=H;Minute=M},{AmPm=AM;Hour=H';Minute=M'} -> if H < H' then true else H = H' && M < M' 
    | {AmPm=PM;Hour=H;Minute=M},{AmPm=PM;Hour=H';Minute=M'} -> if H < H' then true else H = H' && M < M' 

// solution for comparing time with triples

let compareTime2 x y = 
    match x,y with
    | ("AM",_,_),("PM",_,_) -> true
    | ("PM",_,_),("AM",_,_) -> false
    | ("AM",a,b),("AM",a',b') -> 
        match a with
        | a when a < a' -> true
        | a when a > a' -> false
        | a when a = a' ->
            match b with
            | b -> b < b'
            | _ -> failwith "not comparable input"
    | ("PM",a,b),("PM",a',b') -> 
        match a with
        | a when a < a' -> true
        | a when a > a' -> false
        | a when a = a' ->
            match b with
            | b -> b < b'
            | _ -> failwith "not comparable input"
    | _,_ -> failwith "Not valid input"

// same as above implemented with if statments. Polymorphic as it is comparables

let compareTime3 x y =
    let (ampm,hour,minute) = x
    let (ampm',hour',minute') = y
    if ampm < ampm' then true
    elif ampm = ampm' && hour < hour' then true
    elif ampm = ampm' && hour < hour' && minute < minute' then true
    else false

// infix notation 
let (.<.) t1 t2 = compareTime3 t1 t2

let t4 = ("AM",5,5);;
let t5 = ("AM",5,6);;

let r4 = (.<.) t4 t5;; // false

// For both records and triples it is possible to compare the given arguments directly
// as long as they have the same form eg. t4 < t5 equels true. This is because f#
// uses the order in which the arguments are defined to do 

// 3.2 - add and subtract pounds

// poor readability but works
let addPoundx x y = 
    match x,y with
    | (a,b,c), (a',b',c') -> (a+a'+(b+b'+(c+c'/12))/20,(b+b'+((c+c')/12))%20,(c+c')%12)
//recursive version 
let rec addPoundsRec x y = 
    match x,y with
    | (a,b,c), (a',0,0) -> (a+a',b,c)
    | (a,b,c), (a',b',0) -> addPoundsRec (a+(b+b')/20, (b+b')%20,c) (a',0,0)
    | (a,b,c), (a',b',c') -> addPoundsRec (a,b+(c+c')/12,(c+c')%12) (a',b',0)

let rec subtractPounds x y = 
    match x,y with
    | (a,b,c), (0,0,0) -> (a+(-((b+ c/12)/20)),((b+ c/12)%20),(c%12))
    | (a,b,c), (a',0,0) -> if a'> a then subtractPounds (a-(a'-1),b+(-19),c+(-12)) (0,0,0)
                                    else (a-a',b,c)
    | (a,b,c), (a',b',0) -> if b' > b then subtractPounds (a-1,(b+20)-b',c) (a',0,0) 
                                      else subtractPounds (a,b-b',c) (a',0,0)
    | (a,b,c), (a',b',c') -> if c' > c then subtractPounds (a,b-1,(c+12)-c') (a',b',0) 
                                       else subtractPounds (a,b,c-c') (a',b',0)

// 3.3

let (.+.) x y = 
    match x,y with
    | (a,b),(c,d) -> (a+c,b+d)

let (.*.) x y =
    match x,y with 
    | (a,b),(c,d) -> (a*c-b*d,b*c + a*d)


let (.-.) x y =
    match x,y with
    | (a,b), (c,d) -> (a,b) .+. (-c,-d)

let (./.) x y = 
    match x,y with
    | (0,0),(c,d) -> failwith "a and b cannot both be 0"
    | (a,b),(c,d) ->  let div = a*a+b*b 
                      (c*a/div,d*(-b)/div) 
// do not think above is correct, please correct me if my math is of


let complex1 = (2,3);;
let complex2 = (6,6);;

let resAdd = (.+.) complex1 complex2;;
let resMul = (.*.) complex1 complex2;;
let resSub = (.-.) complex1 complex2;;
let resDivError = (./.) (0,0);;
let resDiv = (./.) complex1 complex2;;

// MISSING LAST COUPLE OF EXERCISES

// 4.1
let upto n =
    let rec inner n l =
        match n with
        | 0 -> l
        | x -> inner  (x-1) (x::l)
    inner n []

let res1 = upto 10;;

let downto1 n =
    let rec downto1' n' l =
        match n' with
        | 0 -> l
        | x -> downto1' (x-1) ((n-x+1)::l)
    downto1' n []

let res2 = downto1 3;;


let downtocheat n = List.rev (upto n)
let res2e = downtocheat 3;;

// 4.3
let evenN n =
    let rec inner n' ac l =
        match n' with
        | 0 -> l
        | x -> inner (n'-1) (ac-2) (ac::l)
    inner n (n*2) []

let res4 = evenN 10;;

// 4.4
let rec altsum = function
    | [0] -> 0
    | xs -> if List.length xs > 1 then xs.Head - xs.Tail.Head + altsum (xs.Tail.Tail)
                                  else xs.Head  
let res5 = altsum [2;-1;3];;

// 4.5 -> error FS0193: Specified method is not supported.
let rmodd x =
    let rec inner x' c acc =
        match x' with
        | [] -> acc
        | xs -> if c%2 = 0 then inner (xs.Tail) (c+1) (acc@[xs.Head]) else inner xs.Tail (c+1) acc
    inner x 2 []
let res6 = rmodd [1;2;3;4;5;6];;

// 4.6
let rec removeEven = function
    | [] -> []
    | x::xs -> if x%2 = 0 then removeEven xs else [x] @ (removeEven xs)

let res8 = removeEven [1;2;3;4;5];; 

// 4.7 
let multiplicity x xs = 
    let rec inner y' ys' acc =
        match ys' with
        | [] -> acc
        | y::ys -> if y = y' then inner y' ys (acc+1) else inner y' ys acc 
    inner x xs 0
let res7 = multiplicity 2 [1;2;4;2;12;-1;2;8];;

//4.8
let split x =
    let rec split' xs' l1 l2 =
        match xs' with
        | [] -> (l1, l2)
        | x::[] -> (l1@[x] ,l2)
        | x::y::xs -> split' xs (l1@[x]) (l2@[y])
    split' x [] []
let res8 = split [1;2;4;2;12;-1;2;8;9];;

// 4.9 
let rec zip (a, b) =
    match a, b with
    | ([],[]) -> []
    | (x::[], y::[]) -> [(x, y)]
    | (x::xs, y::ys) when xs.Length = ys.Length -> 
        (x, y)::zip (xs,ys)
    | _ -> failwith "Length of lists not equal"
let res9 = zip ([2;3;4],[9;8;7]);;

//4.10
let rec prefix x y =
    match x,y with
    | [],[] -> true
    | x::xs,[] -> true
    | [],y::ys -> false
    | x::xs,y::ys -> if x = y then prefix xs ys else false
let res10 = prefix [1;2;3;4] [1;2;3;4;5];;

//4.11
let count (xs,x) =
    let rec inner xs' x' ac c =
        match xs' with
        | [] -> ac
        | x::xs when x >= c -> if x = x' then inner xs x' (ac+1) x else inner xs x' ac x
        | _ -> failwith "Not a ascending list"
    inner xs x 0 0
let res11a = count ([1;2;1;3],2);;

// alternatively below, which takes advantages of it being an ascending list making it more effective with a large datastructure
//however it does not check whether it actually is an ascending list
let count2 (xs,x) =
    let rec inner xs' x' ac =
        match xs' with
        | [] -> ac
        | x::xs -> if x = x' then inner xs x' (ac+1) elif x < x' && ac = 0 then inner xs x' ac 
                             elif x < x' && ac > 0 then failwith "Not a ascending list" else ac
    inner xs x 0 
let res11a2 = count2 ([1;2;2;3;1],2);;

//4.11b
let insert (xs,x) =
    let rec inner xs' (x':int) l =
        match xs' with
        | [] -> l
        | x::xs -> if x < x' then inner xs x' else l  
    inner xs x [] 


let rec insert (xs, x) =
    match xs with
    | [] -> [x]
    | y::[] -> if x <= y then [x] @ [y] else [y] @ [x]
    | y::ys -> if  x <= y then  x::[y] @ ys else y::(insert (ys, x))
    | _ -> failwith "Get a f.... grip - %A" xs

let list = [1..15];;
let list2 = [1;2;3;4;2;7;8];;

let res11b = insert (list2, 5 );;

let rec intersect (xs,xs') =  
    match xs,xs' with
    | _, [] -> []
    | [], _ -> []
    | x::xs, x'::xs' -> if x=x' then x::intersect (xs,xs') elif x > x' then intersect ((x::xs), xs') else intersect (xs,(x'::xs')) 

let res11c = intersect ([1;1;1;2;2],[1;1;2;4]);; 

let rec plus (xs,xs') =
    match xs,xs' with
    | [],[] -> []
    | _,[] -> xs
    | [],_ -> xs'
    | x::xs,x'::xs' -> if x=x' then x::x'::plus (xs,xs') elif x > x' then x'::plus ((x::xs), xs') else x::plus (xs,(x'::xs')) 

let res11d = plus ([1;1;2;8;10],[1;2;4;5;9]);;

let rec minus (xs,xs') =
    match xs,xs' with
    | [], _ -> []
    | _, [] -> xs 
    | x::xs,x'::xs' -> if x=x' then minus (xs,xs') elif x > x' then minus (x::xs,xs') else x::minus (xs,x'::xs') 

let res11e = minus ([1;1;1;2;2],[1;1;2;3]);;

// 4.12 
let rec sum (p,xs) =
    match xs with 
    | [] -> 0
    | x::xs -> if p x then x+sum (p,xs) else sum (p,xs)

let p x = x > 4;;
let res12 = sum (p,[1;3;5;8])

// 4.13
let smlElement xs =
    match xs with
    | [] -> failwith "given empty list"
    | x::xs -> 
                let rec inner x r =
                    match x with
                    | [] -> r 
                    | x'::xs' -> if x' < r then inner xs' x' else inner xs' r
                inner xs x 
let res13a = smlElement [8;2;4;7;1;12;3];;

let delete (a,xs) = 
    let rec inner (a',xs') acc =
        match xs' with
        | [] -> List.rev acc
        | x::xs' -> if x=a then (List.rev acc) @ xs' else inner (a',xs') (x::acc)
    inner (a,xs) []
let res13b = delete (5,[1;2;3;4;5;5;6]);;

let sorts xs =
        match xs with
        | [] -> failwith "Given empty list"
        | x::[] -> [x]
        | _ -> 
                    let rec inner xs' acc =
                        match xs' with
                        | [] -> acc
                        | _ -> let element = smlElement xs'
                               inner (delete (element,xs')) (acc @ [element])
                    inner xs [] 
let re13c = sorts [8;2;4;7;1;12;3];;

// 4.14
let smlElementOption xs =
    match xs with
    | [] -> None 
    | x::xs -> 
                let rec inner x r =
                    match x with
                    | [] -> Some r 
                    | x'::xs' -> if x' < r then inner xs' x' else inner xs' r
                inner xs x 
let res14 = smlElementOption [8;2;4;7;1;12;3];;

// 4.15
let rec revrev xs = 
    match xs with 
    | [] -> []
    | x::xs -> 
                let rec inner x' =
                    match x' with
                    | [] -> []
                    | y::ys -> (inner ys)@[y]
                revrev xs @ [inner x]
               
let res15 = revrev [[1;2];[1;2;3];[10;12;13]];; 

let revrevSimple x = List.rev (x |> List.map (fun x' -> List.rev x'))
let res15extra = revrevSimple [[1;2];[1;2;3]];;  

// Chapter 5
// 5.1 
let filter f xs = List.foldBack (fun x acc -> if f x then x::acc else acc) xs [] 
let res5a = filter (fun x -> x%2 = 0) [1..8];;

// 5.2
let rec foldRevrev list = List.fold (fun ac x -> (List.fold (fun ac' x' -> x'::ac') [] x)::ac) [] list 
let res5b = foldRevrev [[1;2];[1;2;3];[6;7;8]]; 

// 5.3
let foldSum (p,list) = List.fold (fun acc x -> if p x then acc + x else acc) 0 list

let p x = x > 4;;
let res5c = foldSum (p,[1;3;5;8])

// 5.4
let downto1 f n e = if n > 0 then List.foldBack f [1..n] e else e

let foldFact n = downto1 (*) n 1
let res5d = foldFact 5;;

// not quite right - fix this
let gList g n = downto1 g n []
let res5d2 = gList (fun x -> x) 5;; 

// 5.5
let map = [("a","b");("c","d");("d","a")];;

let areNb c1 c2 m = List.exists (fun (x,y) -> (x=c1 && y=c2) || (x=c2 && y=c1)) m

let res5e = areNb "a" "b" map;;
let res5e2 = areNb "a" "c" map;;

let canBeExtBy m col c =  List.forall (fun x -> not (areNb x c m)) col
let res5e3 = canBeExtBy map ["c"] "a"
let res5e4 = canBeExtBy map ["a";"c"] "b"
let test = canBeExtBy map [] "a";;
let test2 = canBeExtBy map ["b"] "a";;

let extColouring m cols c = List.fold (fun acc x -> if canBeExtBy m x c then ([c]@x) ::acc else x::[c]::acc) [] cols
let res5e5 = extColouring map [] "a" // not working for this case
let res5e6 = extColouring map [["c"]] "a";;
let res5e7 = extColouring map [["b"]] "a";;


    
