

let rec slowfib n =
    if n<2 then 1.0 else slowfib(n-1) + slowfib(n-2);;

#time;;
let fib42 = slowfib(42);;
#time;;

(*
    --> Timing now on

> let fib42 = slowfib(42);;
Real: 00:00:02.423, CPU: 00:00:02.375, GC gen0: 0, gen1: 0
val fib42 : float = 433494437.0

> #time;;;;
*)

#time;;
let list1 = [0 .. 43];;
let list2 = List.map slowfib list1;;
#time;;

(*
    > let list1 = [0 .. 43];;
Real: 00:00:00.000, CPU: 00:00:00.000, GC gen0: 0, gen1: 0
val list1 : int list =
  [0; 1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17; 18; 19; 20;
   21; 22; 23; 24; 25; 26; 27; 28; 29; 30; 31; 32; 33; 34; 35; 36; 37; 38; 39;
   40; 41; 42; 43]

> let list2 = List.map slowfib list1;;
Real: 00:00:10.074, CPU: 00:00:09.932, GC gen0: 0, gen1: 0
val list2 : float list =
  [1.0; 1.0; 2.0; 3.0; 5.0; 8.0; 13.0; 21.0; 34.0; 55.0; 89.0; 144.0; 233.0;
   377.0; 610.0; 987.0; 1597.0; 2584.0; 4181.0; 6765.0; 10946.0; 17711.0;
   28657.0; 46368.0; 75025.0; 121393.0; 196418.0; 317811.0; 514229.0; 832040.0;
   1346269.0; 2178309.0; 3524578.0; 5702887.0; 9227465.0; 14930352.0;
   24157817.0; 39088169.0; 63245986.0; 102334155.0; 165580141.0; 267914296.0;
   433494437.0; 701408733.0]

> #time;;
*)

#time;;
let f i = async { return slowfib(i) }
let list3 = [0 .. 43];;
let tasks = List.map async ;;
#time;;
let fib42Async = async { return slowfib(41) };;