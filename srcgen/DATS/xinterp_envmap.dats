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
#include
"share\
/atspre_staload_libats_ML.hats"
#staload
UN = "prelude/SATS/unsafe.sats"
//
(* ****** ****** *)
//
#define
XATSOPT_targetloc
"./../../xatsopt/srcgen/xats"
//
(* ****** ****** *)
//
#staload
LAB =
"{$XATSOPT}/SATS/xlabel0.sats"
#staload
STM =
"{$XATSOPT}/SATS/xstamp0.sats"
#staload
SYM =
"{$XATSOPT}/SATS/xsymbol.sats"
//
typedef sym_t = $SYM.symbol
//
overload
= with $SYM.eq_symbol_symbol
overload
.stamp with $SYM.symbol_get_stamp
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
extern
fun
the_hdctpdef_search
(k0: sym_t): Option_vt(irval)
extern
fun
the_hdctpdef_insert
(k0: sym_t, def: irval): void
//
(* ****** ****** *)
//
extern
fun
the_hdcstdef_search
(k0: hdcst): Option_vt(irval)
extern
fun
the_hdcstdef_insert
(k0: hdcst, def: irval): void
//
extern
fun
the_hdvardef_search
(k0: hdvar): Option_vt(irval)
extern
fun
the_hdvardef_insert
(k0: hdvar, def: irval): void
//
(* ****** ****** *)
//
datatype
h0key =
|
H0Kcst of hdcst // let-fun
|
H0Kvar of hdvar // arg and let-var
//
(* ****** ****** *)

local
//
absimpl
irenv_tbox =
List0(@(h0key, irval))
//
datavtype
intenv =
|
INTENV of
(int(*level*), intstk)
//
and
intstk =
|
intstk_nil of ()
|
intstk_fun of ()
//
|
intstk_let1 of intstk
|
intstk_try1 of intstk
//
|
intstk_cons of
(h0key, irval, intstk)
//
absimpl intenv_vtbox = intenv
//
in(*in-of-local*)

(* ****** ****** *)
implement
irenv_make_nil
((*void*)) = list_nil()
implement
intenv_make_nil
((*void*)) =
INTENV(0, intstk_nil())
(* ****** ****** *)

implement
intenv_free_nil
  (env0) =
{
val-~intstk_nil() = xs
} where
{
val+~INTENV(l0, xs) = env0
} (* intenv_free_nil *)

(* ****** ****** *)
//
implement
intenv_make_irenv
  ( kxs ) = let
//
fun
intstk_make_irenv
(kxs: irenv): intstk =
(
auxlst
(kxs, intstk_fun())
) where
{
//
fun
auxlst
( kxs: irenv
, env: intstk): intstk =
(
case+ kxs of
|
list_nil() => env
|
list_cons(kx0, kxs) =>
(
  auxlst(kxs, env)
) where
{
  val env =
  intstk_cons(kx0.0, kx0.1, env)
}
) (* end of [auxlst] *)
//
} (* end of [intstk_make_irenv] *)
in
INTENV
(1(*lev*), intstk_make_irenv(kxs))
end (*let*) // end of [intenv_make_irenv]
//
(* ****** ****** *)
//
implement
intenv_take_irenv(env) =
let
val+
INTENV(lev, stk) = env
in
  intstk_take_irenv(stk)
end where
{
fun
intstk_take_irenv
(env: !intstk): irenv =
(
list_vt2t
(
auxenv(env, list_vt_nil())
)
) where
{
vtypedef
res =
List0_vt(@(h0key, irval))
fun
auxenv
( env:
! intstk, res: res): res =
(
case+ env of
//
| intstk_nil() => res
| intstk_fun() => res
//
| intstk_let1
    (env) => auxenv(env, res)
| intstk_try1
    (env) => auxenv(env, res)
//
(*
| intstk_loc1
    (env) => auxenv(env, res)
| intstk_loc2
    (env) => auxenv(env, res)
*)
//
|
intstk_cons(k0, x0, env) =>
(
  auxenv
  (env, list_vt_cons((k0, x0), res))
)
)
} (* end of [intstk_take_irenv] *)
} (*where*) // end of [intenv_take_irenv]

(* ****** ****** *)

implement
xinterp_search_hdcst
  (env0, hdc0) =
  (auxlst(xs)) where
{
//
vtypedef
res = Option_vt(irval)
val+INTENV(l0, xs) = env0
//
fun
auxlst
(xs: !intstk): res =
(
case+ xs of
| intstk_nil() =>
  the_hdcstdef_search(hdc0)
| intstk_fun() =>
  the_hdcstdef_search(hdc0)
//
| intstk_let1(xs) => auxlst(xs)
| intstk_try1(xs) => auxlst(xs)
//
(*
| intplst_loc1(xs) => auxlst(xs)
| intplst_loc2(xs) => auxlst(xs)
*)
| intstk_cons
  (h0k1, irv1, xs) =>
  (
  case+ h0k1 of
  | H0Kcst(hdc1) =>
    if
    (hdc0 = hdc1)
    then Some_vt(irv1) else auxlst(xs)
  | H0Kvar(hdv1) => auxlst(xs)
  )
) (* end of [auxlst] *)
//
} (* end of [xinterp_search_hdcst] *)

(* ****** ****** *)
//
implement
xinterp_search_hdvtp
  (env0, hdv0) =
(
the_hdvardef_search(hdv0)
)
//
(* ****** ****** *)

implement
xinterp_search_hdvar
  (env0, hdv0) =
(
  auxlst(xs)) where
{
//
vtypedef
res = Option_vt(irval)
val+INTENV(l0, xs) = env0
//
fun
auxlst
(xs: !intstk): res =
(
case+ xs of
|
intstk_nil() =>
the_hdvardef_search(hdv0)
|
intstk_fun() =>
the_hdvardef_search(hdv0)
//
|
intstk_let1(xs) => auxlst(xs)
|
intstk_try1(xs) => auxlst(xs)
//
(*
|
intplst_loc1(xs) => auxlst(xs)
|
intplst_loc2(xs) => auxlst(xs)
*)
|
intstk_cons
(h0k1, irv1, xs) =>
(
case+ h0k1 of
| H0Kcst(hdc1) => auxlst(xs)
| H0Kvar(hdv1) =>
  if
  (hdv0 = hdv1)
  then Some_vt(irv1) else auxlst(xs)
)
) (* end of [auxlst] *)
//
} (* end of [xinterp_search_hdvar] *)

(* ****** ****** *)

implement
xinterp_insert_hdvar
  (env0, hdv0, irv0) =
let
//
val+
@INTENV(l0, xs) = env0
//
in
//
case xs of
|
intstk_nil() =>
(
fold@(env0);
the_hdvardef_insert(hdv0, irv0)
)
|
_(*non-intplst_nil*) =>
(
fold@(env0)
) where
{
val () =
(
xs :=
intstk_cons(H0Kvar(hdv0), irv0, xs)
)
} (* non-intplst_nil *)
//
end // end of [xinterp_insert_hdvar]

(* ****** ****** *)

end // end of [local]

(* ****** ****** *)

local

val
the_flag = ref<int>(0)

in(*in-of-local*)

implement
xinterp_initize() =
let
val n0 = the_flag[]
val () = the_flag[] := n0+1
in
if
(n0 = 0)
then
{
(*
val () = xinterp_initize_gint()
*)
}
end // end of [xinterp_initize]

end // end of [local]

(* ****** ****** *)

local
//
typedef key = sym_t
typedef itm = irval
//
#define HDCSTMAPSZ 1024
//
implement
hash_key<key>(k0) =
let
fun
fhash
( k0
: uint)
: ulint = hash_key<uint>(k0)
in
$effmask_all(fhash(k0.stamp()))
end
implement
equal_key_key<key>
(k1, k2) = $effmask_all(k1 = k2)
//
val
the_hdctpdef_map =
let
val
size =
i2sz(HDCSTMAPSZ)
in
hashtbl_make_nil<key,itm>(size)
end
//
in (*in-of-local*)

(* ****** ****** *)
(*
implement
xinterp_fprint_the_hdctpmap
  (out) =
(
fprint_hashtbl(out, the_hdctpdef_map)
)
*)
(* ****** ****** *)

implement
the_hdctpdef_search
  (k0) =
hashtbl_search<key,itm>(the_hdctpdef_map, k0)

(* ****** ****** *)

implement
the_hdctpdef_insert
  (k0, x0) =
{
val-
~None_vt() =
hashtbl_insert<key,itm>(the_hdctpdef_map, k0, x0)
} (* end of [the_hdcstdef_insert] *)

(* ****** ****** *)

end // end of [local]

(* ****** ****** *)

local
//
typedef key = hdcst
typedef itm = irval
//
#define HDCSTMAPSZ 1024
//
implement
hash_key<key>(k0) =
let
fun
fhash
( k0
: uint): ulint = hash_key<uint>(k0)
in
$effmask_all
(fhash($STM.stamp2uint(k0.stamp())))
end
implement
equal_key_key<key>(k1, k2) =
$effmask_all
(
$STM.eq_stamp_stamp(k1.stamp(), k2.stamp())
)
//
val
the_hdcstdef_map =
let
val
size =
i2sz(HDCSTMAPSZ)
in
hashtbl_make_nil<key,itm>(size)
end
//
in (*in-of-local*)

(* ****** ****** *)

(*
implement
xinterp_fprint_the_hdcstmap
  (out) =
(
fprint_hashtbl(out, the_hdcstdef_map)
)
*)

(* ****** ****** *)

implement
the_hdcstdef_search
  (k0) =
(
case+ opt of
|
Some_vt _ => opt
| ~
None_vt _ =>
the_hdctpdef_search(k0.sym())
) where
{
val opt =
hashtbl_search<key,itm>(the_hdcstdef_map, k0)
} (* end of [the_hdcstdef_search] *)

(* ****** ****** *)

implement
the_hdcstdef_insert
  (k0, x0) =
{
val-
~None_vt() =
hashtbl_insert<key,itm>(the_hdcstdef_map, k0, x0)
} (* end of [the_hdcstdef_insert] *)

(* ****** ****** *)

end // end of [local]

(* ****** ****** *)

local
//
typedef key = hdvar
typedef itm = irval
//
#define HDVARMAPSZ 1024
//
implement
hash_key<key>(k0) =
let
fun
fhash
( k0
: uint): ulint = hash_key<uint>(k0)
in
$effmask_all
(fhash($STM.stamp2uint(k0.stamp())))
end
implement
equal_key_key<key>(k1, k2) =
$effmask_all
(
$STM.eq_stamp_stamp(k1.stamp(), k2.stamp())
)
//
val
the_hdvardef_map =
let
val
size =
i2sz(HDVARMAPSZ)
in
hashtbl_make_nil<key,itm>(size)
end
//
in (*in-of-local*)

(* ****** ****** *)

(*
implement
xinterp_fprint_the_hdvarmap
  (out) =
(
fprint_hashtbl(out, the_hdvardef_map)
)
*)

(* ****** ****** *)

implement
the_hdvardef_search
  (k0) =
hashtbl_search<key,itm>(the_hdvardef_map, k0)

implement
the_hdvardef_insert
  (k0, x0) =
{
val-
~None_vt() =
hashtbl_insert<key,itm>(the_hdvardef_map, k0, x0)
} (* end of [the_hdvardef_insert] *)

(* ****** ****** *)

end // end of [local]

(* ****** ****** *)

fun
gint_add_sint_sint
( x: irval
, y: irval): irval =
let
val-IRVint(x) = x
val-IRVint(y) = y in IRVint(x+y)
end

fun
gint_sub_sint_sint
( x: irval
, y: irval): irval =
let
val-IRVint(x) = x
val-IRVint(y) = y in IRVint(x-y)
end

fun
gint_mul_sint_sint
( x: irval
, y: irval): irval =
let
val-IRVint(x) = x
val-IRVint(y) = y in IRVint(x*y)
end

(* ****** ****** *)

local

(* ****** ****** *)

fun
firfun2
(
f2:
( irval
, irval) -> irval
)
: irvalist -<cloref1> irval =
lam(vs) =>
let
val-
list_cons(v1,vs) = vs
val-
list_cons(v2,vs) = vs in f2(v1, v2)
end

(* ****** ****** *)

overload
symbol with $SYM.symbol_make

(* ****** ****** *)

in(*in-of-local*)

(* ****** ****** *)

val () =
the_hdctpdef_insert
(
symbol
"XINTERP_gint_add_sint_sint"
,
IRVfun(firfun2(gint_add_sint_sint)))

val () =
the_hdctpdef_insert
(
symbol
"XINTERP_gint_sub_sint_sint"
,
IRVfun(firfun2(gint_sub_sint_sint)))

val () =
the_hdctpdef_insert
(
symbol
"XINTERP_gint_mul_sint_sint"
,
IRVfun(firfun2(gint_mul_sint_sint)))

(* ****** ****** *)

end // end of [local]

(* ****** ****** *)

(* end of [xint_xinterp_envmap.dats] *)
