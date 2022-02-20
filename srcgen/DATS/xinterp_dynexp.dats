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

(* end of [xint_xinterp_dynexp.dats] *)
