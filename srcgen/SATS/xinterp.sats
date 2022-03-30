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

abstype irenv_tbox = ptr
typedef irenv = irenv_tbox

(* ****** ****** *)
//
abstype irval_tbox = ptr
typedef irval = irval_tbox
//
(*
abstype irlazv_tbox = ptr
typedef irlazv = irlazv_tbox
abstype irlftv_tbox = ptr
typedef irlftv = irlftv_tbox
*)
//
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
//
datatype
irval_node =
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
| IRVlft of (irlftval)
//
| IRVfun of (irvalfun)
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
|
IRVfixs of
( irenv
, hdvar(*f*)
, hfarglst, h0exp, h0explst)
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
| IRLAZval of irval(*value*)
| IRLAZexp of (irenv, h0exp) // thunk
//
and
irlftval =
|
IRLFTref of ref(irvalopt)
//
|
IRLFTpcon of (irval, label)
//
|
IRLFTpbox of
(irval, label, int(*index*))
|
IRLFTpflt of
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
irval_nil(h0typ): irval
fun
irval_top(h0typ): irval
//
fun
irval_make_node
( h0t0: h0typ
, node: irval_node): irval
//
(* ****** ****** *)
//
fun
irval_get_type
( irv0: irval ): h0typ
overload .type with irval_get_type
fun
irval_get_node
( irv0: irval ): irval_node
overload .node with irval_get_node
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
print_irlazval(irlazval): void
fun
prerr_irlazval(irlazval): void
fun
fprint_irlazval: fprint_type(irlazval)
//
overload print with print_irlazval
overload prerr with prerr_irlazval
overload fprint with fprint_irlazval
//
(* ****** ****** *)
//
fun
print_irlftval(irlftval): void
fun
prerr_irlftval(irlftval): void
fun
fprint_irlftval: fprint_type(irlftval)
//
overload print with print_irlftval
overload prerr with prerr_irlftval
overload fprint with fprint_irlftval
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
xinterp_search_hdcst
( env:
! intenv
, hdc: hdcst): Option_vt(irval)
//
fun
xinterp_search_hdvtp
( env:
! intenv
, hdv: hdvar): Option_vt(irval)
fun
xinterp_search_hdvar
( env:
! intenv
, hdv: hdvar): Option_vt(irval)
//
(* ****** ****** *)
//
fun
xinterp_insert_hdcst
( env:
! intenv
, hdc: hdcst, irv: irval): void
//
fun
xinterp_insert_hdvar
( env:
! intenv
, hdv: hdvar, irv: irval): void
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
