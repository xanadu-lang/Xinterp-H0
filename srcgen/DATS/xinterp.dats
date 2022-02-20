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
#staload
"./../SATS/xinterp.sats"
//
(* ****** ****** *)
//
#include
"./../HATS/libxinterp.hats"
//
(* ****** ****** *)
#dynload
"./../DATS/xinterp_main0.dats"
#dynload
"./../DATS/xinterp_envmap.dats"
#dynload
"./../DATS/xinterp_dynexp.dats"
(* ****** ****** *)
//
implement
main0(argc, argv) =
(
//
if
(argc >= 2)
then
(
xinterp_main0(argc, argv)
)
else
{
val () =
prerrln!
("Hello from ATS3(xinterp)!")
//
val
XATSHOME = the_XATSHOME_get()
val
((*void*)) =
prerrln!
("xinterp: XATSHOME=",XATSHOME)
//
} (* else *) // end of [if]
) where
{
// (*
// (*
val out = stderr_ref
val ( ) =
$XATSOPT.echo_argc_argv(out, argc, argv)
// *)
} (* end of [main0] *)
//
(* ****** ****** *)

(* end of [xint_xinterp.dats] *)