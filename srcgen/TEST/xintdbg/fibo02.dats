(* ****** ****** *)
(*
Sat Apr  2 10:42:53 EDT 2022
*)
(* ****** ****** *)
#staload _ =
"prelude\
/DATS/CATS/Xint/basics.dats"
(* ****** ****** *)
//
fun
fibo(n0: int): int =
let
//
fun
loop
( i0: int
, f1: int
, f2: int): int =
if
(i0 >= 2)
then loop(i0-1, f2, f1+f2) else f2
//
in
if
(n0 >= 1)
then loop(n0, 0(*f1*), 1(*f2*)) else 0
end
//
(* ****** ****** *)

val fibo10 = fibo(10)

(* ****** ****** *)

(* end of [fibo02.dats] *)
