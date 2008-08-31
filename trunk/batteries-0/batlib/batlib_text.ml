(*
 * Batlib.Text - Text stuff, including parsing and printing
 * Copyright (C) 2008 David Teller
 * 
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Lesser General Public
 * License as published by the Free Software Foundation; either
 * version 2.1 of the License, or (at your option) any later version,
 * with the special exception on linking described in file LICENSE.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public
 * License along with this library; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *)

(**
   Everything text related, from characters to parsing and printing.

   {b Note} Unicode-related operations will fall into this module.
*)

(** {1 Characters}*)
module Char            = Batlib_Baselib_Char
module String          = Batlib_Extlib_String (*formerly Batlib_Baselib_String*)
module StringLabels    = Batlib_Baselib_StringLabels

(** {1 Parsing} *)
module Genlex          = Batlib_Baselib_Genlex
module Lexing          = Batlib_Baselib_Lexing
module Parsing         = Batlib_Baselib_Parsing
module Scanf           = Batlib_Baselib_Scanf
module Str             = Batlib_Baselib_Str

(** {1 Printing}*)
module Format          = Batlib_Baselib_Format
module Printexc        = Batlib_Baselib_Printexc
module Printf          = Batlib_Baselib_Printf
