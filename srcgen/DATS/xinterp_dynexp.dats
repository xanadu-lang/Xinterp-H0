(***********************************************************************)
(*                                                                     *)
(*                         Applied Type System                         *)
(*                                                                     *)
(***********************************************************************)

(*
** ATS/Xanadu - Unleashing the Potential of Types!
** Copyright (C) 2022 Hongwei Xi, ATS Trustful Software, Inc.
** All rights reserved
**
** ATS is free software;  you can  redistribute it and/or modify it under
** the terms of  the GNU GENERAL PUBLIC LICENSE (GPL) as published by the
** Free Software Foundation; either version 3, or (at  your  option)  any
** later version.
** 
** ATS is distributed in the hope that it will be useful, but WITHOUT ANY
** WARRANTY; without  even  the  implied  warranty  of MERCHANTABILITY or
** FITNESS FOR A PARTICULAR PURPOSE.  See the  GNU General Public License
** for more details.
** 
** You  should  have  received  a  copy of the GNU General Public License
** along  with  ATS;  see the  file COPYING.  If not, please write to the
** Free Software Foundation,  51 Franklin Street, Fifth Floor, Boston, MA
** 02110-1301, USA.
*)

(* ****** ****** *)
//
// Author: Hongwei Xi
// Start Time: February, 2022
// Authoremail: gmhwxiATgmailDOTcom
//
(* ****** ****** *)
//
#include
"share\
/atspre_staload.hats"
#staload
UN =
"prelude/SATS/unsafe.sats"
//
(* ****** ****** *)
//
#include
"./../HATS/libxatsopt.hats"
//
(* ****** ****** *)
//
#staload
"{$XATSOPT}/SATS/intrep0.sats"
//
#staload "./../SATS/xinterp.sats"
//
(* ****** ****** *)
//
implement
fprint_val<irval> = fprint_irval
//
(* ****** ****** *)
//
extern
fun
xinterp_h0pat_ck0
(h0p0: h0pat, irv1: irval): bool
extern
fun
xinterp_h0patlst_ck0
( h0p0
: h0patlst, irv1: irvalist): bool
//
extern
fun
xinterp_h0pat_ck1
( env0: irenv
, h0p0: h0pat, irv1: irval): void
extern
fun
xinterp_h0patlst_ck1
( env0: irenv
, h0ps
: h0patlst, irv1: irvalist): void
//
(* ****** ****** *)

extern
fun
xinterp_h0exp
( env0
: irenv, h0e0: h0exp): irval
extern
fun
xinterp_h0explst
( env0
: irenv, h0es: h0explst): irvalist

(* ****** ****** *)

local

(* ****** ****** *)

fun
auxi00
( h0e0
: h0exp): irval =
IRVint(int) where
{
val-
H0Ei00(int) = h0e0.node()
} (* end of [auxi00] *)

fun
auxs00
( h0e0
: h0exp): irval =
IRVstr(str) where
{
val-
H0Es00(str) = h0e0.node()
} (* end of [auxs00] *)

(* ****** ****** *)

fun
auxint
( h0e0
: h0exp): irval =
(
IRVint(token2dint(tok))
) where
{
val-
H0Eint(tok) = h0e0.node()
} (* end of [auxint] *)

(* ****** ****** *)

fun
auxbtf
( h0e0
: h0exp): irval =
(
IRVbtf(token2dbtf(tok))
) where
{
val-H0Ebtf(tok) = h0e0.node()
} (* end of [auxbtf] *)

(* ****** ****** *)

fun
auxchr
( h0e0
: h0exp): irval =
(
IRVchr(token2dchr(tok))
) where
{
val-H0Echr(tok) = h0e0.node()
} (* end of [auxchr] *)

(* ****** ****** *)

fun
auxflt
( h0e0
: h0exp): irval =
(
IRVflt(token2dflt(tok))
) where
{
val-H0Eflt(tok) = h0e0.node()
} (* end of [auxflt] *)

fun
auxstr
( h0e0
: h0exp): irval =
(
IRVstr(token2dstr(tok))
) where
{
val-H0Estr(tok) = h0e0.node()
} (* end of [auxstr] *)

(* ****** ****** *)

fun
auxtop
( h0e0
: h0exp): irval =
(
  IRVtop( h0e0.type() )
) where
{
val-H0Etop(tok) = h0e0.node()
} (* end of [auxtop] *)

(* ****** ****** *)

fun
auxlam
( env0
: irenv
, h0e0
: h0exp): irval =
let
val-
H0Elam
( knd0
, args
, body ) = h0e0.node()
in
  IRVlam1(env0, args, body)
end

fun
auxfix
( env0
: irenv
, h0e0
: h0exp): irval =
let
val-
H0Efix
( knd0
, nam1
, args
, body ) = h0e0.node()
in
IRVfix1(env0, nam1, args, body)
end

(* ****** ****** *)

fun
auxift1
( env0
: irenv
, h0e0: h0exp): irval =
let
val-
H0Eift1
( h0e1
, h0e2
, opt3 ) = h0e0.node()
val
irv1 =
xinterp_h0exp(env0, h0e1)
in
//
case- irv1 of
|
IRVbtf(test) =>
if
test
then
(
  xinterp_h0exp(env0, h0e2)
)
else
(
case+ opt3 of
| None() => IRVnil(*void*)
| Some(h0e3) =>
  xinterp_h0exp(env0, h0e3)
)
//
end (*let*) // end of [auxif1]

(* ****** ****** *)

fun
auxtrcd1
( env0
: irenv
, h0e0: h0exp): irval =
let
val-
H0Etrcd1
( knd0
, npf1
, h0es ) = h0e0.node()
//
fun
auxlst
( env0
: irenv
, npf1: int
, h0es
: h0explst): irvalist =
(
case+ h0es of
|
list_nil() =>
list_nil()
|
list_cons
(h0e1, h0es) =>
if
npf1 > 0
then
(
auxlst
(env0, npf1-1, h0es)
)
else
let
val irv1 =
xinterp_h0exp(env0, h0e1)
in
list_cons
(
irv1
,
auxlst(env0, npf1-1, h0es))
end
) (*case*) // end of [auxlst]
//
in
IRVtrcd1
( knd0
, auxlst(env0, npf1, h0es))
end (*let*) // end of [auxtrcd1]

(* ****** ****** *)

in(*in-of-local*)

(* ****** ****** *)
//
implement
xinterp_h0exp
(env0, h0e0) =
(
case+
h0e0.node() of
//
| H0Ei00 _ => auxi00(h0e0)
| H0Es00 _ => auxs00(h0e0)
//
| H0Eint _ => auxint(h0e0)
| H0Ebtf _ => auxbtf(h0e0)
| H0Echr _ => auxchr(h0e0)
| H0Eflt _ => auxflt(h0e0)
| H0Estr _ => auxstr(h0e0)
//
| H0Etop _ => auxtop(h0e0)
//
| H0Elam _ => auxlam(env0, h0e0)
| H0Efix _ => auxfix(env0, h0e0)
//
| H0Eift1 _ => auxift1(env0, h0e0)
//
|
H0Etrcd1 _ => auxtrcd1(env0, h0e0)
//
|
_(*rest-of-h0exp*) => IRVnone1(h0e0)
) where
{
val () =
println!("xinterp_h0exp: h0e0 = ", h0e0)
} (*where*) // end of [xinterp_h0exp]
//
(* ****** ****** *)

end // end of [local]

(* ****** ****** *)

implement
xinterp_h0pat_ck0
  (h0p0, irv0) =
let
//
(*
val () =
println!
("xinterp_h0pat_ck0: h0p0 = ", h0p0)
val () =
println!
("xinterp_h0pat_ck0: irv0 = ", irv0)
*)
//
in
//
case-
h0p0.node() of
//
|
H0Pany _ => true
|
H0Pvar _ => true
//
|
H0Pint(int1) =>
(
case- irv0 of
|
IRVint(int0) =>
( int0=int1 ) where
{
val int1=token2dint(int1)
}
)
|
H0Pbtf(btf1) =>
(
case- irv0 of
|
IRVbtf(btf0) =>
( btf0=btf1 ) where
{
val btf1=token2dbtf(btf1)
}
)
|
H0Pchr(chr1) =>
(
case- irv0 of
|
IRVchr(chr0) =>
( chr0=chr1 ) where
{
val chr1=token2dchr(chr1)
}
)
|
H0Ptrcd1
(knd0, npf1, h0ps) =>
(
case- irv0 of
|
IRVtrcd1(knd1, irvs) =>
let
val () =
assertloc(knd0=knd1)
val h0ps =
(
  auxtail(npf1, h0ps)
) where
{
fun
auxtail
( npf1: int
, h0ps
: h0patlst): h0patlst =
if
npf1 <= 0
then h0ps else
(
case- h0ps of
| list_cons
  (_, h0ps) =>
  auxtail(npf1-1, h0ps)
)
} (*where*) // end-of-val
in
xinterp_h0patlst_ck0(h0ps, irvs)
end
) (* end of [H0Ptrcd1] *)
//
end (*end*) // end of [xinterp_h0pat_ck0]

(* ****** ****** *)
implement
xinterp_h0patlst_ck0
  (h0ps, irvs) =
(
case+ h0ps of
|
list_nil() => true
|
list_cons(h0p0, h0ps) =>
let
val-
list_cons(irv0, irvs) = irvs
val ans =
xinterp_h0pat_ck0(h0p0, irv0)  
in(*in-of-let*)
//
if ans
then xinterp_h0patlst_ck0(h0ps, irvs)
else false
//
end // end of [list_cons]
) (*case*)//end of [xinterp_h0patlst_ck0]
(* ****** ****** *)

implement
xinterp_h0pat_ck1
(env0, h0p0, irv0) =
let
//
(*
val () =
println!
("xinterp_h0pat_ck0: h0p0 = ", h0p0)
val () =
println!
("xinterp_h0pat_ck0: irv0 = ", irv0)
*)
//
in
end (*end*) // end of [xinterp_h0pat_ck1]

(* ****** ****** *)

implement
xinterp_h0patlst_ck1
  (env0, h0ps, irvs) =
(
case+ h0ps of
|
list_nil() => ()
|
list_cons(h0p0, h0ps) =>
let
val-
list_cons(irv0, irvs) = irvs
val () =
xinterp_h0pat_ck1(env0, h0p0, irv0)  
in
  xinterp_h0patlst_ck1(env0, h0ps, irvs)
end // end of [list_cons]
) (*case*) // end of [xinterp_h0patlst_ck1]

(* ****** ****** *)

(* end of [xint_xinterp_dynexp.dats] *)
