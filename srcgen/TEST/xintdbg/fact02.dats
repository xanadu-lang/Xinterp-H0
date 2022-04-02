(* ****** ****** *)
(*
Sat Apr  2 10:12:59 EDT 2022
*)
(* ****** ****** *)
#staload _ =
"prelude\
/DATS/CATS/Xint/basics.dats"
(* ****** ****** *)

fun
fact(n: int): int =
loop(0, 1) where
{
fun
loop(i: int, r: int): int =
if i < n then loop(i+1, (i+1)*r) else r
}

(* ****** ****** *)

val fact10 = fact(10)

(* ****** ****** *)

(* end of [fact02.dats] *)
