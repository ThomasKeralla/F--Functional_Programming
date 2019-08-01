// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.
module assignment1
//Exercise 1.1 
let sqr x = x*x;; 
//Exercise 1.2
let pow x n = System.Math.Pow(x,n);;

//Exercise 1.3
let g n = n+4;;

//Exercise 1.4
let h x y = System.Math.Sqrt(x*x+y*y);;

//Exercise 1.5
let rec f = 
  function
    | n when n < 1 -> 0
    | n -> n + f (n - 1) ;;

//Exercise 1.6
let rec fib n = 
  match n with
    | 0 -> 0
    | 1 | 2 -> 1
    | n -> fib(n-1) + fib(n-2)
;;

//Exercise 1.7
let rec sum = 
  function
    | (m,0) -> m 
    | (m,n) -> (m + n) + sum(m, n-1)  
;;

//Exercise 1.8
let rec fact = function
      | 0 -> 1
      | n -> n * fact(n-1);;

let rec power = 
  function
    | (x,0) -> 1.0                //  (1)
    | (x,n) -> x * power(x,n-1);; //  (2)

(*
let b = (System.Math.PI, fact -1);;
//b is bound to a tuple as the pair (float, int) as types
let c = fact(fact 4)
//c is bound to an int as fact 4 is an int being used as argument in fact
let d = power(System.Math.PI, fact 2) 
//d is bound to type float as the function power takes a float and int as arguments and returns a float
let e = (power, fact)
//e is bound to a tuple of functions, hence the type will be function. 
//The functions returns float and int, so with arguments the tuple would be of type (float, int)
*)
//Exercise 1.9
(*
let a = 5;;
let f a = a + 1;;
let g b = (f b) + a;;
*)
(* 
f 3;;
 val it : int = 4

g 3;;
 val it : int = 9
 *)

 //Exercise 1.10
 let dup s :string = s + s
 
 
 //Exercise 1.11
let rec dupn (s:string) n = 
  match n with
   |0 -> ""
   |_ -> s + dupn s (n-1)


[<EntryPoint>]
let main (param: string[]) =
  printfn "Hello world!!";
  printfn "Solutions for assignment 1";
  printfn "<Exercise num> * <function call> -> <result>";

  // 1.1
  printfn "1.1 - sqr 16 = %i" (sqr 16);

  // 1.2
  printfn "1.2 - pow 7.0 4.0 = %.2f" (pow 7.0 4.0);

  // 1.3
  printfn "1.3 - g 8 = %i" (g 8);

  // 1.4
  printfn "1.4 - h 3.0 9.0 = %.4f" (h 3.0 9.0);

  // 1.5
  printfn "1.5 - f 7 = %i" (f 7);

  // 1.6
  printfn "1.6 - fib 9 = %i" (fib 9);

  // 1.7
  printfn "1.7 - sum (4,5) = %i" (sum (4,5)) ;

  // 1.8
  printfn "1.8 -";
  printfn "\t(System.Math.PI, fact -1)";
  printfn "\t(float * int)\n";

  printfn "\tfact (fact 4)";
  printfn "\tint -> int -> int\n";

  printfn "\tpower (System.Math.PI, fact 2)";
  printfn "\tfloat * int -> float\n";

  printfn "\t(power, fact)";
  printfn "\t(float * int)";

  // 1.9
  printfn "1.9 - after making the declarations in an interactive environment:";
  printfn "\tf 3;; returns 'val it : int = 4'";
  printfn "\tg 3;; returns 'val it : int = 9'";

  // 1.10
  printfn "1.10 - dup \"Hi \" = %s" (dup "Hi ");

  // 1.11
  printfn "1.11 - dupn \"Hi \" 3 = %s" (dupn "Hi " 3);

  0;; // return an int
  