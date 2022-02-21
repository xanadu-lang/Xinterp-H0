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
#include
"./../HATS/libxatsopt.hats"
(* ****** ****** *)
//
#staload
"{$XATSOPT}/SATS/intrep0.sats"
#staload
"{$XATSOPT}/DATS/intrep0.dats"
//
(* ****** ****** *)
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
implement
fprint_val<hdcst> = fprint_hdcst
implement
fprint_val<hdcon> = fprint_hdcon
implement
fprint_val<hdvar> = fprint_hdvar
//
(* ****** ****** *)
//
implement
fprint_val<h0pat> = fprint_h0pat
implement
fprint_val<h0exp> = fprint_h0exp
implement
fprint_val<hfarg> = fprint_hfarg
//
(* ****** ****** *)
//
implement
print_irval(x0) = 
fprint_irval(stdout_ref, x0)
implement
prerr_irval(x0) = 
fprint_irval(stderr_ref, x0)
//
(* ****** ****** *)

implement
fprint_irval(out, x0) =
(
case+ x0 of
//
| IRVnil() =>
  fprint!(out, "IRVnil()")
//
| IRVint(i0) =>
  fprint!(out, "IRVint(", i0, ")")
| IRVptr(p0) =>
  fprint!(out, "IRVptr(", p0, ")")
//
| IRVbtf(b0) =>
  fprint!(out, "IRVbtf(", b0, ")")
| IRVchr(c0) =>
  fprint!(out, "IRVchr(", c0, ")")
//
| IRVflt(f0) =>
  fprint!(out, "IRVflt(", f0, ")")
| IRVstr(s0) =>
  fprint!(out, "IRVstr(", s0, ")")
//
| IRVtop(tok) =>
  fprint!(out, "IRVtop(", tok, ")")
//
|
IRVlam0
(args, h0e1) =>
fprint!
( out
, "IRVlam1(", args, "; ", h0e1, ")")
|
IRVlam1
(env0, args, h0e1) =>
fprint!
( out
, "IRVlam1(", args, "; ", h0e1, ")")
|
IRVfix0
(hdv0, args, h0e1) =>
fprint!
( out
, "IRVfix0("
, hdv0, "; ", args, "; ", h0e1, ")")
|
IRVfix1
(env0, hdv0, args, h0e1) =>
fprint!
( out
, "IRVfix1("
, hdv0, "; ", args, "; ", h0e1, ")")
//
|
IRVtrcd1(knd0, irvs) =>
fprint!
( out
, "IRVtrcd1(", knd0, "; ", irvs, ")")
//
|
IRVnone0() =>
(
  fprint!(out, "IRVnone0(", ")")
)
|
IRVnone1(h0e1) =>
(
  fprint!(out, "IRVnone1(", h0e1, ")")
)
//
) (*case*) // end of [fprint_irval]

(* ****** ****** *)

(* end of [xint_xinterp_print.dats] *)
