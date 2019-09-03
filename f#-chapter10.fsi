open System;;
open System.IO;;
//open TextProcessing;;
open System.Text.RegularExpressions;;

let wordCount (input:strin) (output:string) =
    use inStream = File.OpenText input
    use outStream = File.CreateText output
    let reg = Regex @"G\s*\^([^\s]+)\s+" 
    let rec inner rl map =
        match rl with
        | null when inStream.EndOfStream -> inStream.Close |> ignore &&  map
        | null -> inner inStream.ReadLine map
        | s -> let res = captureList (reg.Match s) 0 // maybe 0
               let newMap = List.iter (fun x -> if map.containsKey x then Map.add ((Map.find x map) +1) map) res
               inner inStream.ReadLine newMap
    let map = inner inStream.ReadLine Map.empty
    Map.toList map |> List.iter (fun (x,y) -> (outStream.WriteLine "%s has : %d ocurrences" x y) outStream.Flush )
    outStream.Close

let testString = "jeg har en flot cat w"
let reg = Regex @"\b(\w+)\b+" 
let ma = reg.Match testString
let next = ma.NextMatch()
let nextNext = next.NextMatch()
let res = Capture next 1
let list = [for i in ma.Groups -> i.Value]
let isSuccess = ma.Success
let res = reg.Match testString
    let list = CaptureCollection res 1
    List.iter (fun x -> printfn x) list

let regOuter = Regex @"\G(\s*[a-zA-Z]+(?:\s+\d+)*)*\s*$";;

let reg3 = Regex @"\G(\S+)\s*$";;
let t3 = reg3.Match testString
let boolt3 = t3.Success
let single = 

[<EntryPoiny>]
let main (args: string[]) =
    wordCount args.[0] args.[1]


1.1 : Declare the function hanoi : int -> int where
          hanoi n = 1              if n = 1
          hanoi n = 2*hanoi(n-1)+1 if n > 1

let rec hanoi n = 
    match n with
    | x when x < 1 -> failwith "input has to be greater than 0"
    | 1 -> 1
    | x -> (2 * hanoi (x-1))+1       
let resHanoi = hanoi 4

let rec tailHanoi (n:int) c =
    match n with
    //| x when x < 1 -> failwith "input has to be greater than 0"
    | 1 -> c 1
    | x -> tailHanoi (n-1) (fun x -> (2 * c x)+1 )
let resTail = tailHanoi 4 id

type tree<'a> = 
         L of 'a
       | N of list<tree<'a>>

let v1 = N [L 'A'; L 'B'; L 'C']
let v2 = N [L 1; v1; L 11; L 34] 
let v3 = N [L 1; L 11; L 34]   
let v4 = N [L 'A'; L 'B'; L 'C';v1]
let v4 = L (4) 

(*
  2.2 : Declare a F# function 
          flattenTr tr : tree<'a> -> list<'a>, 
        that returns a list with all elements in the tree.
        Example: flattenTr v1 = ['A'; 'B'; 'C']
*)

let flattenTr t =
  let rec inner t' acc =
    match t' with
    | L x -> x::acc
    | N [] -> acc
    | N x::xs -> (inner (x:tree<'a>) acc) @ (inner ((N xs):tree<'a>) acc) 
  inner t []  

let flattenTr t =
  let rec inner t' acc =
    match t' with
    | L x -> acc @ [x]
    | N ([]) -> acc
    | N (x::[]) -> inner x acc
    | N (x::xs) -> inner (N xs)  (inner x acc) 
  inner t []  

let res = flattenTr v4

let rec ft = 
          function
            L v -> L v
          | N vs -> N (List.rev (List.map ft vs))
let resft = ft v4         

