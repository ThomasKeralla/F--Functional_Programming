open System;;

let val1 = "ab"

let val2 = "d"

compare val1 val2;;

let ordText x y = 
    match compare x y with
    | t when t > 0 -> "greater"
    | 0 -> "equal"
    | _ -> "less"

ordText "abc" "Abc";;

let square x = x*x

let rec unzip = function
| [] -> ([],[])
| (x,y)::rest -> let (xs,ys) = unzip rest 
                 (x::xs,y::ys);;

unzip [(1,"a");(2,"b");(3,"3")];;

let rec findPosI p x = function
| y::_ when x=y -> Some p
| _::ys -> findPosI (p+1) x ys
| [] -> None;;

let val22 = Option.get (findPosI 0 6 [2 .. 6]);;

let v = [6 .. -1 .. 2];;

[2 .. 6] @ [2 .. -1 .. -2];;

let rec sumProd = function
| []   -> (0,1)
| x::rest ->
            let (rSum,rProd) = sumProd rest 
            (x+rSum,x*rProd);;

sumProd [2;1];;

let rec split = function
| [] -> ([],[])
| [x] -> ([x],[])
| x::y::xs -> 
             let (xs1,xs2) = split xs
             (x::xs1,y::xs2);;

split [1;2;3;4;5];;
split [1];;

