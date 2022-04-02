(* ****** ****** *)
(*
Fri Apr  1 14:33:16 EDT 2022
*)
(* ****** ****** *)
#staload _ =
"prelude\
/DATS/CATS/Xint/basics.dats"
(* ****** ****** *)

fun
fibo(n: int): int =
if
(n >= 2)
then fibo(n-1)+fibo(n-2) else n

(* ****** ****** *)

val fibo10 = fibo(10)

(* ****** ****** *)

(* end of [fibo01.dats] *)
