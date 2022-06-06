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
"./../HATS/libxinterp.hats"
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

#symload
fprint with $LAB.fprint_label

(* ****** ****** *)
//
implement
fprint_val<irval> = fprint_irval
//
(* ****** ****** *)
//
implement
fprint_val<h0cst> = fprint_h0cst
implement
fprint_val<h0con> = fprint_h0con
implement
fprint_val<h0var> = fprint_h0var
//
(* ****** ****** *)
//
implement
fprint_val<h0pat> = fprint_h0pat
implement
fprint_val<h0fag> = fprint_h0fag
//
(* ****** ****** *)
//
implement
fprint_val<h0exp> = fprint_h0exp
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
fprint_irval
( out, irv0 ) =
let
//
fun
auxmain
(irv0: irval): void =
(
case+ irv0 of
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
| IRVlft(lvl) =>
  fprint!(out, "IRVlft(", lvl, ")")
//
|
IRVfun(fopr) =>
fprint!(out, "IRVfun(", "...", ")")
(*
|
IRVlam0
(args, h0e1) =>
fprint!
( out
, "IRVlam1(", args, "; ", h0e1, ")")
*)
|
IRVlam1
(env0, args, h0e1) =>
fprint!
( out
, "IRVlam1(", args, "; ", h0e1, ")")
(*
|
IRVfix0
(hdv0, args, h0e1) =>
fprint!
( out
, "IRVfix0("
, hdv0, "; ", args, "; ", h0e1, ")")
*)
|
IRVfix1
(env0, hdv0, args, h0e1) =>
fprint!
( out
, "IRVfix1("
, hdv0, "; ", args, "; ", h0e1, ")")
//
|
IRVfixs
(fenv, hdv0, hfas, h0e1, h0es) =>
fprint!
( out
, "IRVfixs("
, hdv0, "; ", hfas, "; ", h0e1, "; ", "...", ")")
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
) (*case*) // end of [auxmain]
//
in
  auxmain(irv0)
(*
; fprint!(out, "@(", irv0.type(), ")")
*)
end (*let*) // end of [fprint_irval]

(* ****** ****** *)
//
implement
print_irlftval(x0) = 
fprint_irlftval(stdout_ref, x0)
implement
prerr_irlftval(x0) = 
fprint_irlftval(stderr_ref, x0)
//
(* ****** ****** *)
//
implement
fprint_irlftval
( out, irlv ) =
(
case+ irlv of
|
IRLFTref(r0) =>
fprint!
( out
, "IRLFTref(", ref_get_ptr(r0), ")")
//
|
IRLFTpcon
(x1, lab) =>
fprint!
(out, "IRLFTpcon(", x1, "; ", lab, ")")
//
|
IRLFTpbox
(x1, lab, idx) =>
fprint!
( out
, "IRLFTpbox(", x1, "; ", lab, "; ", idx, ")")
|
IRLFTpflt
(x1, lab, idx) =>
fprint!
( out
, "IRLFTpflt(", x1, "; ", lab, "; ", idx, ")")
//
) (* end of [fprint_irlftval] *)
//
(* ****** ****** *)

(* end of [xint_xinterp_print0.dats] *)
