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
xinterp_h0exp
( env0:
! intenv, h0e0: h0exp): irval
extern
fun
xinterp_h0explst
( env0:
! intenv, h0es: h0explst): irvalist
extern
fun
xinterp_h0expopt
( env0:
! intenv, opt0: h0expopt): irvalopt
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
( env0:
! intenv
, h0p0: h0pat, irv1: irval): void
extern
fun
xinterp_h0patlst_ck1
( env0:
! intenv
, h0ps
: h0patlst, irv1: irvalist): void
//
(* ****** ****** *)

extern
fun
xinterp_hvaldecl
( env0:
! intenv, hvd0: hvaldecl): void
extern
fun
xinterp_hvaldeclist
( env0:
! intenv, hvds: hvaldeclist): void

(* ****** ****** *)

extern
fun
xinterp_hvardecl
( env0:
! intenv, hvd0: hvardecl): void
extern
fun
xinterp_hvardeclist
( env0:
! intenv, hvds: hvardeclist): void

(* ****** ****** *)

implement
xinterp_program
  (p0kg) =
let
//
val () =
xinterp_initize()
//
val
env0 =
intenv_make_nil()
//
local
val+
H0COMPED(rcd0) = p0kg
val-Some(dcls) = rcd0.comped
in
val () =
xinterp_h0dclist(env0, dcls)
end // end of [local]
//
val () = intenv_free_nil(env0)
//
in
  // nothing
end // end of [xinterp_program]

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
} (*where*) // end of [auxi00]

fun
auxs00
( h0e0
: h0exp): irval =
IRVstr(str) where
{
val-
H0Es00(str) = h0e0.node()
} (*where*) // end of [auxs00]

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
  IRVtop(h0e0.type())
) (* end of [auxtop] *)

(* ****** ****** *)

fun
auxvar
( env0:
! intenv
, h0e0: h0exp): irval =
let
val-
H0Evar(x0) = h0e0.node()
//
(*
val () =
println!("auxvar: x0 = ", x0)
*)
//
val
opt =
xinterp_search_hdvar(env0,x0)
//
in
case-
opt of ~Some_vt(irv1) => irv1
end (*let*) // end of [auxvar]

(* ****** ****** *)

fun
auxkvar
( env0:
! intenv
, h0e0: h0exp): irval =
let
val-
H0Ekvar(k0,x0) = h0e0.node()
//
(*
val () =
println!("auxkvar: k0 = ", k0)
val () =
println!("auxkvar: x0 = ", x0)
*)
//
val
opt =
(
if
(k0 >= 0)
then
xinterp_search_hdvar(env0,x0)
else
xinterp_search_hdvtp(env0,x0)
) : Option_vt(irval)
//
in
case-
opt of ~Some_vt(irv1) => irv1
end (*let*) // end of [auxkvar]

(* ****** ****** *)

fun
auxfcst
( env0:
! intenv
, h0e0
: h0exp): irval =
let
//
(*
val () =
println!
("auxfcst: h0e0 = ", h0e0)
*)
//
val-
H0Efcst(hdc) = h0e0.node()
//
in
//
if
hdcst_fcastq(hdc)
then
IRVfun
(
lam(vs) =>
let
val-
list_cons(v0, _) = vs in v0 
end
) (* end of [then] *)
else
(
case-
opt of ~Some_vt(irf) => irf
) where
{
val
opt =
xinterp_search_hdcst(env0, hdc)
} (* end of [else] *)
//
end // end of [auxfcst]

(* ****** ****** *)

fun
auxtimp
( env0:
! intenv
, h0e0: h0exp): irval =
let
//
val-
H0Etimp
( stmp
, h0e1, targ
, h0cl, tsub) = h0e0.node()
val-
H0Etcst
( hdc0, ti3a) = h0e1.node()
//
(*
val () =
println!("auxtimp: h0e0 = ", h0e0)
val () =
println!("auxtimp: h0e1 = ", h0e1)
*)
//
fun
auxfixs
(
hfds
:
hfundeclist
) : h0explst =
(
case+
hfds of
|
list_nil() =>
list_nil()
|
list_cons
(hfd0, hfds) =>
let
val+
HFUNDECL
  (rcd) = hfd0
//
val nam = rcd.nam
val hdc = rcd.hdc
val hag = rcd.hag
val def = rcd.def
//
in
//
case+ hag of
|
None() =>
auxfixs(hfds)
|
Some(hfas) =>
(
case+ def of
|
None() =>
auxfixs(hfds)
|
Some(body) =>
(
case+ hfas of
|
list_nil _ =>
(
case+
body.node() of
|
H0Elam
(knd, hfas, h0e2) =>
let
val h0e1 =
h0exp_make_node
(
body.loc()
,
nam.type((*void*))
,
H0Efix(knd, nam, hfas, h0e2)
) (* end of [val] *)
in
list_cons(h0e1, auxfixs(hfds))
end // end of [H0Elam]
//
|
_(*rest-of-h0exp*) =>
list_cons(body, auxfixs(hfds))
)
|
list_cons _ =>
let
val
loc = rcd.loc
val
knd =
token_make_node
(loc, T_FIX(1))
val h0e1 =
h0exp_make_node
(
loc
,
nam.type()
,
H0Efix(knd, nam, hfas, body)
) (* end of [val] *)
in
  list_cons(h0e1, auxfixs(hfds))
end 
) (* end of [Some(body)] *)
) (* end of [Some(hfas)] *)
//
end (* end of [list_cons] *) ) (*auxfixs*)
//
fun
auxhfd0
( fenv
: irenv
, hfd0
: hfundecl): irval =
let
//
val-
HFUNDECL
  (rcd) = hfd0
//
val nam = rcd.nam
//
val-
Some(hfas) = rcd.hag
val-
Some(body) = rcd.def
//
in
//
case+ hfas of
|
list_nil _ =>
(
case-
body.node() of
|
H0Elam
(knd, hfas, hexp) =>
IRVfix1(fenv, nam, hfas, hexp)
)
|
list_cons _ =>
IRVfix1(fenv, nam, hfas, body)
//
end // end of [auxhfd0]
//
fun
auxhfds
( fenv
: irenv
, hdfs
: h0explst
, hfds
: hfundeclist
) : irval =
(
case-
hfds of
(*
|
list_nil() =>
IRVerror()
*)
|
list_cons
(hfd0, hfds) =>
let
val+
HFUNDECL
  (rcd) = hfd0
in
//
if
(hdc0 = rcd.hdc)
then let
//
val nam = rcd.nam
//
val-
Some(hfas) = rcd.hag
val-
Some(body) = rcd.def
//
in
//
case+
hfas of
|
list_nil() =>
(
case-
body.node() of
H0Elam
(knd, hfas, body) =>
IRVfixs
(fenv, nam, hfas, body, hdfs)
) (* end of [list_nil] *)
|
list_cons _ =>
IRVfixs
(fenv, nam, hfas, body, hdfs)
//
end // end of [then]
else auxhfds(fenv, hdfs, hfds)
//
end // end of [list_cons]
) (*case*) // end of [auxhfds]
//
in
//
case-
h0cl.node() of
|
H0Cfundecl
( knd0, mopt
, tqas, hfds) =>
let
//
val
fenv =
intenv_take_irenv(env0)
//
val-
list_cons(hfd0, xs) = hfds
//
in
//
case- xs of
|
list_nil _ =>
auxhfd0(fenv, hfd0)
|
list_cons _ =>
let
  val
  hdfs = auxfixs(hfds)
in
  auxhfds(fenv, hdfs, hfds)
end
//
end
|
H0Cimpdecl3
( knd0, mopt
, sqas, tqas
, hdc1, ti3a, hfas, body
) =>
(
//
case+ hfas of
|
list_nil _ =>
xinterp_h0exp(env0, body)
|
list_cons _ =>
let
val
fenv =
intenv_take_irenv(env0)
in
IRVlam1(fenv, hfas, body)
end
//
) (* IRCimpdecl3 *)
//
end (*let*) // end of [auxtimp]

(* ****** ****** *)

fun
auxdapp
( env0:
! intenv
, h0e0: h0exp): irval =
let
//
(*
//
val loc0 = h0e0.loc()
//
val () =
println!
("auxdapp: loc0 = ", loc0)
val () =
println!
("auxdapp: h0e0 = ", h0e0)
*)
//
val-
H0Edapp
( h0f0
, npf1
, h0es) = h0e0.node()
//
val
irf0 =
auxdfun(env0, h0f0)
val
irvs =
auxdarg(env0, npf1, h0es)
//
val () =
println!
("auxdapp: h0f0 = ", h0f0)
val () =
println!
("auxdapp: irf0 = ", irf0)
val () =
println!
("auxdapp: irvs = ", irvs)
//
in
//
case- irf0 of
//
|
IRVfun(fopr) => fopr(irvs)
//
(*
|
IRVlam1(_, _, _) =>
xinterp_fcall_lam1(irf0, irvs)
|
IRVfix1(_, _, _, _) =>
xinterp_fcall_fix1(irf0, irvs)
|
IRVfixs(_, _, _, _, _) =>
xinterp_fcall_fixs(irf0, irvs)
*)
//
end (*let*) // end of [auxdapp]

and
auxdfun
( env0:
! intenv
, h0f0: h0exp): irval = 
(
xinterp_h0exp(env0, h0f0)
)

and
auxdarg
( env0:
! intenv
, npf1: int
, h0es
: h0explst): irvalist = 
(
case+ h0es of
|
list_nil() => list_nil()
|
list_cons(h0e1, h0es) =>
(
if
(npf1 >= 1)
then
(
  auxdarg(env0, npf1-1, h0es)
)
else let
  val irv1 =
  xinterp_h0exp(env0, h0e1)
in
  list_cons
  ( irv1
  , auxdarg(env0, npf1, h0es))
end // end of [else]    
)
) (*case*) (* end of [auxdarg] *)

(* ****** ****** *)

fun
auxlam
( env0:
! intenv
, h0e0: h0exp): irval =
let
//
val-
H0Elam
( knd0
, args
, body ) = h0e0.node()
val
fenv =
intenv_take_irenv(env0)
in
IRVlam1(fenv, args, body)
end

fun
auxfix
( env0:
! intenv
, h0e0: h0exp): irval =
let
//
val-
H0Efix
( knd0
, nam1
, args
, body ) = h0e0.node()
//
val
fenv =
intenv_take_irenv(env0)
in
IRVfix1(fenv, nam1, args, body)
end

(* ****** ****** *)

fun
auxift1
( env0:
! intenv
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
| None() =>
  IRVnil((*void*))
| Some(h0e3) =>
  xinterp_h0exp(env0, h0e3)
)
//
end (*let*) // end of [auxif1]

(* ****** ****** *)

fun
auxtrcd1
( env0:
! intenv
, h0e0: h0exp): irval =
let
//
val-
H0Etrcd1
( knd0
, npf1
, h0es ) = h0e0.node()
//
fun
auxlst
( env0:
! intenv
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
|
H0Evar _ => auxvar(env0, h0e0)
|
H0Ekvar _ => auxkvar(env0, h0e0)
//
|
H0Efcst _ => auxfcst(env0, h0e0)
//
|
H0Etimp _ => auxtimp(env0, h0e0)
//
|
H0Edapp _ => auxdapp(env0, h0e0)
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
xinterp_h0explst
  (env0, h0es) =
(
case+ h0es of
|
list_nil() =>
list_nil()
|
list_cons(h0e1, h0es) =>
list_cons(irv1, irvs) where
{
val irv1 =
xinterp_h0exp(env0, h0e1)
val irvs =
xinterp_h0explst(env0, h0es)
}
) (*case*) // end of [xinterp_h0explst]

(* ****** ****** *)

implement
xinterp_h0expopt
  (env0, opt0) =
(
case+ opt0 of
|
None() => None()
|
Some(h0e0) =>
Some(xinterp_h0exp(env0, h0e0))
) (*case*) // end of [xinterp_h0expopt]

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
//
val () =
assertloc(knd0=knd1)
//
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
("xinterp_h0pat_ck1: h0p0 = ", h0p0)
val () =
println!
("xinterp_h0pat_ck1: irv0 = ", irv0)
*)
//
in
//
case-
h0p0.node() of
//
|
H0Pnil() =>
(
case- irv0 of
|
IRVnil() => ()
)
//
|
H0Pany() => ()
//
|
H0Pvar(hdv0) =>
{
val () =
xinterp_insert_hdvar
( env0, hdv0, irv0 )
} (* end of [H0Pvar] *)
//
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

local

(* ****** ****** *)

fun
aux_include
( env0:
! intenv
, dcl0: h0dcl): void =
let
//
val-
H0Cinclude
( tok
, src1(*d1exp*)
, knd2(*stadyn*)
, opt3(*fpathopt*)
, opt4) = dcl0.node()
//
in
case+ opt4 of
| None() => ()
| Some(dcls) =>
  xinterp_h0dclist(env0, dcls)
end // end of [aux_include]

(* ****** ****** *)

fun
aux_valdecl
( env0:
! intenv
, dcl0: h0dcl): void =
let
val-
H0Cvaldecl
( knd0
, mopt
, hvds) = dcl0.node()
in
xinterp_hvaldeclist(env0, hvds)
end // end of [aux_valdecl]

(* ****** ****** *)

fun
aux_vardecl
( env0:
! intenv
, dcl0: h0dcl): void =
let
val-
H0Cvardecl
( knd0
, mopt
, hvds) = dcl0.node()
in
xinterp_hvardeclist(env0, hvds)
end // end of [aux_vardecl]

(* ****** ****** *)

in(*in-of-local*)

implement
xinterp_h0dcl
  (env0, dcl0) =
(
case+
dcl0.node() of
//
|
H0Cvaldecl _ =>
aux_valdecl(env0, dcl0)
//
| _ (*rest-of-h0dcl*) => ()
//
) where
{
(*
val () =
println!
("xinterp_h0dcl: dcl0 = ", dcl0)
*)
//
} (*where*) // end of [xinterp_h0dcl]

end // end of [local]

(* ****** ****** *)

implement
xinterp_h0dclist
  (env0, dcls) =
(
case+ dcls of
|
list_nil() => ()
|
list_cons(dcl1, dcls) =>
{
val () =
xinterp_h0dcl(env0, dcl1)
val () =
xinterp_h0dclist(env0, dcls)
}
) (*case*) // end of [xinterp_h0dclist]

(* ****** ****** *)

implement
xinterp_hvaldecl
  (env0, x0) =
let
//
val+
HVALDECL
( rcd ) = x0
//
val pat = rcd.pat
val def = rcd.def
//
val def =
xinterp_h0expopt(env0, def)
//
val ( ) =
println!
("xinterp_hvaldecl: def = ", def)
//
in
//
case+ def of
|
None() => ()
|
Some(h0v) =>
xinterp_h0pat_ck1(env0, pat, h0v)
//
end (*let*) // end of [xinterp_hvaldecl]

implement
xinterp_hvaldeclist
  (env0, xs) =
(
case+ xs of
|
list_nil() => ()
|
list_cons(x0, xs) =>
(
  xinterp_hvaldeclist(env0, xs)
) where
{
val () = xinterp_hvaldecl(env0, x0)
}
) (*case*) // end of [xinterp_hvaldeclist]

(* ****** ****** *)

implement
xinterp_hvardecl
  (env0, x0) =
let
//
val+
HVARDECL(rcd) = x0
//
val hdv = rcd.hdv
val ini = rcd.ini
//
val ini =
xinterp_h0expopt(env0, ini)
//
(*
val ( ) =
println!
("xinterp_hvardecl: hdv = ", hdv)
val ( ) =
println!
("xinterp_hvardecl: ini = ", ini)
*)
//
val irv0 = 
IRVlft(IRLFTref(ref(ini)))
//
in
//
xinterp_insert_hdvar(env0, hdv, irv0)
//
end // end of [xinterp_hvardecl]

implement
xinterp_hvardeclist
  (env0, xs) =
(
case+ xs of
|
list_nil() => ()
|
list_cons(x0, xs) =>
(
  xinterp_hvardeclist(env0, xs)
) where
{
val () = xinterp_hvardecl(env0, x0)
}
) (*case*) // end of [xinterp_hvardeclist]

(* ****** ****** *)

(* end of [xint_xinterp_dynexp.dats] *)
