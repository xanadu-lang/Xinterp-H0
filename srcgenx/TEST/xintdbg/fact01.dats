(* ****** ****** *)
(*
Sat Feb 19 14:33:31 EST 2022
*)
(* ****** ****** *)
#staload _ =
"srcgen1\
/prelude\
/DATS/CATS/Xint/basics0.dats"
(* ****** ****** *)

fun
fact(n: int): int =
if n > 0 then n * fact(n-1) else 1

(* ****** ****** *)

val fact10 = fact(10)

(* ****** ****** *)

(* end of [fact01.dats] *)
