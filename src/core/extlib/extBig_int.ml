(* 
 * ExtInt32 - Extended Big integers
 * Copyright (C) 2007 Bluestorm <bluestorm dot dylc on-the-server gmail dot com>
 *               2008 David Teller
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

open Sexplib
TYPE_CONV_PATH "" (*For Sexplib, Bin-prot...*)

open Number

module BaseBig_int = struct
  include Big_int
    
  type t = big_int
  let zero = zero_big_int
  let one  = unit_big_int

  let neg  = minus_big_int
  let succ = succ_big_int
  let pred = pred_big_int
  let abs  = abs_big_int
  let add  = add_big_int
  let sub  = sub_big_int
  let mul  = mult_big_int
  let div  = div_big_int

  let modulo = mod_big_int
  let pow  = power_big_int_positive_big_int

  let to_string = string_of_big_int

  let of_string = big_int_of_string

  let to_int    = int_of_big_int

  let of_int    = big_int_of_int

  let compare   = compare_big_int

  let of_float f= of_string (Printf.sprintf "%.0f" f)
  let to_float  = float_of_big_int
end

module Big_int = struct
  include BaseBig_int
  include Number.MakeNumeric(BaseBig_int)

  let ( -- )  x y = Enum.seq x (add one) ( ge_big_int y )
  let ( --- ) x y = 
    if ge_big_int y x then x -- y 
    else Enum.seq x pred (le_big_int y)

  let sexp_of_t = Conv.sexp_of_big_int
  let t_of_sexp = Conv.big_int_of_sexp
  let print out t = IO.nwrite out (to_string t)
end
