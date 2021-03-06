(*
 * ExtNum - Operations on arbitrary-precision numbers
 * Copyright (C) 2008 Gabriel Scherer
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
open Conv
TYPE_CONV_PATH "Batteries.Data.Numeric" (*For Sexplib, Bin-prot...*)

open ExtString
open ExtBig_int
open ExtInt

module BaseNum = struct
  include Num
  type t = num
  
  let zero       = Int 0
  let one        = Int 1
  let neg        = minus_num
  let abs        = abs_num
  let add        = add_num
  let sub        = sub_num
  let mul        = mult_num
  let div        = div_num
  let modulo     = mod_num
  let pow        = power_num
  let compare    = compare_num
  let of_int     = num_of_int
  let to_int     = int_of_num
  let to_float   = float_of_num
  let to_string  = string_of_num
  let of_string  = num_of_string
  let pred       = function
    | Int     i -> Int ( i - 1 )
    | Big_int i -> Big_int (Big_int.pred i)
    | _         -> raise (Invalid_argument "Num.pred")
  let succ       = function
    | Int     i -> Int ( i + 1 )
    | Big_int i -> Big_int (Big_int.succ i)
    | _         -> raise (Invalid_argument "Num.succ")


  let of_float f =
    let s = Printf.sprintf "%f" f in
      try 
	let (prefix, suffix) = String.split s "."    in
	let float_digits     = String.length suffix  in
	let divider = pow (Int 10) (Int (String.length s - float_digits)) in
	let dividee = Big_int (Big_int.of_string  (prefix^suffix))        in
	  div divider dividee
      with Not_found -> of_int (Int.of_float f)

  let round = round_num
  let floor = floor_num
  let ceil  = ceiling_num
  let square= square_num
  let is_integer = is_integer_num
  let approx= integer_num
  let quo   = quo_num
  let sign  = sign_num
end

module Num = struct
  include Number.MakeNumeric(BaseNum)
  include BaseNum
  let sexp_of_t   = sexp_of_num
  let t_of_sexp   = num_of_sexp
  let print out t = InnerIO.nwrite out (to_string t)

end
