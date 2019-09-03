open System
module e4

// 4.1
let explode (x:string) = List.ofArray (x.ToCharArray())
let res1 = explode "star"

let rec explode2 (x:string) = if x = "" then [] else x.[0]::explode2 (x.Remove(0,1))
let res2 = explode2 "star";;

//4.2
let implode (cs:List<char>) = List.foldBack (fun e a ->  e.ToString()+(a:string)) cs ""
let res3 = implode ['a';'b';'c'];;
let implodeRev (cs:List<char>) = List.fold (fun (a:string) e ->  e.ToString()+a) "" cs
let res4 = implodeRev ['a';'b';'c'];;

// 4.3 
let toUpper x = implode (List.map System.Char.ToUpper (explode x))
let res5 = toUpper "star";;
let toUpper2 x = explode x |> List.map System.Char.ToUpper |> implode
let res6 = toUpper2 "star";;

// 4.4
let palindrome x = if explode x |> implodeRev = x then true else false
let res7 = palindrome "anna";;

// 4.5
let rec ack (m,n) =
    match m,n with
    | x when m = 0 -> n+1
    | x when m > 0 && n = 0 -> ack (m-1,1)
    | x when m > 0 && n > 0 -> ack(m-1,ack(m,n-1))
    |_ -> failwith "error"
let res8 = ack (3,11);;