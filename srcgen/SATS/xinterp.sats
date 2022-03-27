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
#define
XATSOPT_targetloc
"./../../xatsopt/srcgen/xats"
//
(* ****** ****** *)
//
#staload
LAB =
"{$XATSOPT}/SATS/xlabel0.sats"
(* ****** ****** *)
#staload
H0E =
"{$XATSOPT}/SATS/intrep0.sats"
(* ****** ****** *)
typedef label = $LAB.label
(* ****** ****** *)
//
typedef h0typ = $H0E.h0typ
//
(* ****** ****** *)
//
typedef hdcon = $H0E.hdcon
typedef hdcst = $H0E.hdcst
typedef hdvar = $H0E.hdvar
//
typedef h0pat = $H0E.h0pat
typedef h0patlst = $H0E.h0patlst
typedef hfarg = $H0E.hfarg
typedef hfarglst = $H0E.hfarglst
//
typedef h0exp = $H0E.h0exp
typedef h0explst = $H0E.h0explst
typedef h0dcl = $H0E.h0dcl
typedef h0dclist = $H0E.h0dclist
//
typedef h0comped = $H0E.h0comped
(* ****** ****** *)

abstype irenv_tbox = ptr
typedef irenv = irenv_tbox

(* ****** ****** *)
//
datatype irval =
//
| IRVnil of ()
//
| IRVint of int
| IRVptr of ptr
//
| IRVbtf of bool
| IRVchr of char
//
| IRVflt of double
| IRVstr of string
//
| IRVtop of (h0typ)
//
| IRVlft of irlftval
//
|
IRVlam0 of
(hfarglst, h0exp)
|
IRVlam1 of
(irenv, hfarglst, h0exp)
|
IRVfix0 of
( hdvar, hfarglst, h0exp)
|
IRVfix1 of
( irenv
, hdvar, hfarglst, h0exp)
//
|
IRVtrcd1 of
(int(*knd*), irvalist)//tuple
(*
|
IRVtrcd2 of
(int(*knd*), labh0explst)//record
*)
//
| IRVnone0 of () // HX: error0
| IRVnone1 of (h0exp) // HX: error1
//
and
irlazval =
| IRLVval of irval(*value*)
| IRLVexp of (irenv, h0exp) // thunk
//
and
irlftval =
| IRLVref of ref(irvalopt)
//
| IRLVpcon of (irval, label)
//
| IRLVpbox of
  (irval, label, int(*index*))
| IRLVpflt of
  (irlftval, label, int(*index*))
//
where
//
irvalist = List0(irval)
and
irvalopt = Option(irval)
and
irvalfun = (irvalist -<cloref1> irval)
//
(* ****** ****** *)
//
fun
print_irval: print_type(irval)
fun
prerr_irval: prerr_type(irval)
fun
fprint_irval: fprint_type(irval)
//
overload print with print_irval
overload prerr with prerr_irval
overload fprint with fprint_irval
//
(* ****** ****** *)
//
fun
xinterp_initize(): void
//
(* ****** ****** *)
//
fun
xinterp_program
  (p0kg: h0comped): void
//
(* ****** ****** *)
absvtype intenv_vtbox = ptr
vtypedef intenv = intenv_vtbox
(* ****** ****** *)
fun
irenv_make_nil(): irenv
fun
intenv_make_nil(): intenv
fun
intenv_free_nil(intenv): void
//
(* ****** ****** *)
//
fun
xinterp_h0dcl
( env0:
! intenv, dcl0: h0dcl): void
fun
xinterp_h0dclist
( env0:
! intenv, dcls: h0dclist): void
//
(* ****** ****** *)
fun
intenv_make_irenv(irenv): intenv
(* ****** ****** *)
//
(*
HX: copying out the stack
*)
fun
intenv_take_irenv(!intenv): irenv
(* ****** ****** *)
//
fun
xinterp_insert_hdvar
( env0:
! intenv
, hdv0: hdvar, irv1: irval): void
//
(* ****** ****** *)
//
fun
the_XATSHOME_get((*void*)): string
//
(* ****** ****** *)
//
fun
xinterp_main0
{n:int | n >= 1}
(argc: int(n), argv: !argv(n)): void
//
(* ****** ****** *)

(* end of [xint_xinterp.sats] *)
