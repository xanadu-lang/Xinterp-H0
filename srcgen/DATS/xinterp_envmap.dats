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
"{$XATSOPT}/SATS/intrep0.sats"
//
#staload "./../SATS/xinterp.sats"
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
(d2c: hdcst, def: irval): void
//
extern
fun
the_hdvardef_search
(k0: hdvar): Option_vt(irval)
extern
fun
the_hdvardef_insert
(d2v: hdvar, def: irval): void
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
intenv_make_irenv(kxs) =
let
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
(env: !intstk, res: res): res =
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
| intstk_cons(k0, x0, env) =>
  (
    auxenv
    (env, list_vt_cons((k0, x0), res))
  )
)
} (* end of [intstk_take_irenv] *)
} (*where*) // end of [intenv_take_irenv]

(* ****** ****** *)

end // end of [local]

(* ****** ****** *)

(* end of [xint_xinterp_envmap.dats] *)
