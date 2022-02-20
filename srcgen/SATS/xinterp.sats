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

#staload
LAB =
"{$XATSOPT}/SATS/xlabel0.sats"
#staload
D2E =
"{$XATSOPT}/SATS/dynexp2.sats"

(* ****** ****** *)
#staload
// H0E = // HX: opened
"{$XATSOPT}/SATS/intrep0.sats"
(* ****** ****** *)

typedef label = $LAB.label

(* ****** ****** *)

typedef d2var = $D2E.d2var
typedef d2con = $D2E.d2con
typedef d2cst = $D2E.d2cst

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
| IRVnone0 of () // HX: error0
| IRVnone1 of (h0exp) // HX: error1
//
and
irlazval =
| IRLVval of irval(*value*)
| IRLVexp of (irenv, h0exp) // thunk
//
where
//
irvalist = List0(irval)
and
irvalopt = Option(irval)
and
irvalfun =
(irvalist -<cloref1> irval)
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
xinterp_program(h0dclist): void
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
