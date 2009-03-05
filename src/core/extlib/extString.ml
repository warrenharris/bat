(*
 * ExtString - Additional functions for string manipulations.
 * Copyright (C) 2003 Nicolas Cannasse
 *               2008 David Teller
 *               2008 Edgar Friendly
 *               2009 Warren Harris
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
TYPE_CONV_PATH "" (*For Sexplib, Bin-prot...*)

(*Minor optimization.*)
let int_min (x:int) (y:int) = if x < y then x else y
let int_max (x:int) (y:int) = if x < y then y else x

module String = struct

include String

let sexp_of_t = sexp_of_string
let t_of_sexp = string_of_sexp

let init len f =
  let s = create len in
  for i = 0 to len - 1 do
    unsafe_set s i (f i)
  done;
  s

(******************************************************************************)

(* testing *)
let (@=) actual expect =
  (if actual = expect then "SUCCESS: expect:" else "FAILURE: expect:"),
  expect, "actual:", actual
let (@=!) thunk exn =
  let exnstr = Printexc.to_string exn in
  try let rv = thunk () in ("FAILURE: expect: " ^ exnstr ^ " actual:", Some rv)
  with e -> ("SUCCESS: expect: " ^ exnstr ^ " actual:", None)

(** The following two operators are used to normalize a substring range such
    that if [ofs] is negative it refers to the length of [str] minus that
    offset; if [len] is negative it refers to the length of [str] minus that
    length plus 1 (i.e. specifying [len = -1] means the full length of the
    string [str]).

    After successful normalization, the function [f] is called with the
    normalized offset and length (making a tail-call in this way avoids the
    need to allocate a result tuple).

    @raise Invalid_argument [msg] - if after normalization the offset and
    length fail to fall within the range of the string [str] (default msg
    is ["String.range"]). *)
let ( @> ) msg f str ofs len = (* normalize to a range above the ofs position *)
  let strlen = length str in
  let len = if len < 0 then strlen + len + 1 else len in
  if len < 0 || len > strlen then invalid_arg (msg ^ " len=" ^ string_of_int len);
  let ofs = if ofs < 0 then strlen + ofs else ofs in
  if ofs < 0 || ofs + len > strlen then invalid_arg (msg ^ " ofs=" ^ string_of_int ofs);
  f str ofs len

let ( @< ) msg f str ofs len = (* normalize to a range below the ofs position *)
  let strlen = length str in
  let len = if len < 0 then strlen + len + 1 else len in
  if len < 0 || len > strlen then invalid_arg (msg ^ " len=" ^ string_of_int len);
  let ofs = if ofs < 0 then strlen + ofs else ofs in
  if ofs < 0 || ofs - len < -1 then invalid_arg (msg ^ " ofs=" ^ string_of_int ofs);
  f str ofs len

let sub =
  "String.sub" @> fun str ofs len ->
    let r = create len in
    unsafe_blit str ofs r 0 len;
    r

let left s cnt = sub s 0 cnt
let right s cnt = sub s (-cnt) cnt
let head s pos = sub s 0 pos
let tail s pos = sub s pos (-pos - 1)

let _ = sub "foobar" (-3) 2 @= "ba"
let _ = sub "foo" 0 3 @= "foo"
let _ = sub "foo" 1 2 @= "oo"
let _ = sub "foobar" (-2) 1 @= "a"
let _ = sub "foobar" 0 (-1) @= "foobar"
let _ = sub "foobar" 1 (-3) @= "ooba"

(******************************************************************************)
(** {7 Match Functions} *)

type match_fun = t -> int -> int -> int

let unsafe_match_substring sub subofs sublen str srchpos srchlen =
  assert (subofs >= 0);
  assert (subofs + sublen <= length sub);
  assert (srchpos >= 0);
  assert (srchpos + srchlen <= length str);
  let len = int_min sublen srchlen in
  let rec loop i =
    if i < len && unsafe_get sub (subofs + i) == unsafe_get str (srchpos + i)
    then loop (i + 1)
    else i
  in
  let cnt = loop 0 in
  if cnt == sublen then sublen else 0

let unsafe_count_after p str srchpos srchlen =
  assert (srchpos >= 0);
  let stop = srchpos + srchlen in
  assert (stop <= length str);
  let rec loop i =
    if i < srchlen && p (unsafe_get str (srchpos + i))
    then loop (i + 1)
    else i (* count *)
  in loop 0

let unsafe_count_before p str srchpos srchlen =
  assert (srchpos >= 0);
  assert (srchpos < length str);
  assert (srchpos - srchlen >= -1);
  let rec loop i =
    if i < srchlen && p (unsafe_get str (srchpos - i))
    then loop (i + 1)
    else i (* count *)
  in loop 0

let match_string sub  =
  "String.match_string" @> unsafe_match_substring sub 0 (length sub)

let _ = match_string "bar" "foobarbaz" 2 3 @= 0
let _ = match_string "bar" "foobarbaz" 3 3 @= 3
let _ = match_string "bar" "foobarbaz" 3 2 @= 0
let _ = match_string "ba" "foobarbaz" 3 2 @= 2
let _ = match_string "bar" "foobarbaz" 4 3 @= 0

let match_substring =
  "String.match_substring" @> "String.match_substring" @> unsafe_match_substring

let match_chars chars =
  "String.match_chars" @> unsafe_count_after (contains chars)

let _ = match_chars "x" "xxxab" 0 2 @= 2
let _ = match_chars "x" "xxxab" 0 3 @= 3
let _ = match_chars "x" "xxxab" 1 3 @= 2
let _ = match_chars "x" "xxxab" 1 4 @= 2
let _ = match_chars "x" "xxxxx" 1 4 @= 4

let rmatch_chars chars =
  "String.rmatch_chars" @< unsafe_count_before (contains chars)

let _ = rmatch_chars "bar" "foobarbaz" 8 3 @= 0
let _ = rmatch_chars "bar" "foobarbaz" 7 3 @= 3
let _ = rmatch_chars "bar" "foobarbaz" 6 3 @= 3
let _ = rmatch_chars "bar" "foobarbaz" 6 4 @= 4
let _ = rmatch_chars "bar" "foobarbaz" 6 5 @= 4
let _ = rmatch_chars "baz" "foobarbaz" 7 8 @= 2
let _ = rmatch_chars "baz" "foobarbaz" (-1) (-1) @= 3
let _ = rmatch_chars "baz" "foobarbaz" (-2) (-2) @= 2
let _ = rmatch_chars "baz" "foobarbaz" (-2) 7 @= 2

let match_whitespace =
  "String.match_whitespace" @> unsafe_count_after ExtChar.Char.is_whitespace

let rmatch_whitespace =
  "String.rmatch_whitespace" @< unsafe_count_before ExtChar.Char.is_whitespace

let match_char c str srchpos srchlen =
  let strlen = length str in
  let srchlen = if srchlen < 0 then strlen + srchlen + 1 else srchlen in
  if srchlen < 1 || srchlen > strlen then invalid_arg "String.match_char";
  let srchpos = if srchpos < 0 then strlen + srchpos else srchpos in
  if srchpos < 0 || srchpos > strlen then invalid_arg "String.match_char";
  if unsafe_get str srchpos == c then 1 else 0

let _ = match_char 'r' "foobar" (-1) (-1) @= 1

(******************************************************************************)
(** {6 Generalized Finding} *)

let find_matching m =
  "String.find_matching" @> fun str ofs len ->
  let stop = ofs + len in
  let rec loop i =
    if i >= stop then raise Not_found
    else
      let cnt = m str i (stop - i) in
      if cnt > 0 then i, cnt (* found *)
      else loop (i + 1)
  in loop ofs

let _ = find_matching (match_string "ba") "foobarbaz" 2 7 @= (3, 2)
let _ = find_matching (match_string "ba") "foobarbaz" 3 6 @= (3, 2)
let _ = find_matching (match_string "ba") "foobarbaz" 4 5 @= (6, 2)
let _ = (fun () -> find_matching (match_string "bar") "foobarbaz" 2 2) @=! Not_found
let _ = (fun () -> find_matching (match_string "bar") "foobarbaz" 3 2) @=! Not_found
let _ = find_matching (match_string "bar") "foobarbaz" 3 3 @= (3, 3)
let _ = find_matching (match_string "bar") "foobarbaz" (-9) (-1) @= (3, 3)

let rfind_matching m =
  "String.rfind_matching" @< fun str ofs len ->
  let strlen = length str in
  let stop = ofs - len in
  let rec loop i =
    if i <= stop then raise Not_found
    else
      let cnt = m str i (strlen - i) in
      if cnt > 0 then i, cnt (* found *)
      else loop (i - 1)
  in loop ofs

let _ = rfind_matching (match_string "ba") "foobarbaz" 7 8 @= (6, 2)
let _ = rfind_matching (match_string "ba") "foobarbaz" 6 7 @= (6, 2)
let _ = rfind_matching (match_string "ba") "foobarbaz" 4 5 @= (3, 2)
let _ = (fun () -> rfind_matching (match_string "bar") "foobarbaz" 2 3) @=! Not_found
let _ = rfind_matching (match_string "bar") "foobarbaz" 3 2 @= (3, 3)
let _ = rfind_matching (match_string "bar") "foobarbaz" 3 3 @= (3, 3)
let _ = (fun () -> rfind_matching (match_string "ba") "foobarbaz" 2 3) @=! Not_found
let _ = rfind_matching (match_string "ba") "foobarbaz" 8 (-1) @= (6, 2)
let _ = rfind_matching (match_string "ba") "foobarbaz" (-1) (-1) @= (6, 2)

let find_from str start_pos sub =
  let len = length str in
  let start_pos = if start_pos < 0 then len + start_pos else start_pos in
  let sublen = length sub in
  let search_range = start_pos + sublen in
  if search_range > len then raise Not_found;
  if sublen == 0 then start_pos (* if [sub] is the empty string, by convention,
                                   it may be found wherever we started searching.*)
  else fst (find_matching (match_string sub) str start_pos (len - start_pos))

let _ = find_from "foobarbaz" 2 "ba" @= 3
let _ = find_from "foobarbaz" 3 "ba" @= 3
let _ = find_from "foobarbaz" 4 "ba" @= 6
let _ = find_from "foobarbaz" 5 "ba" @= 6
let _ = find_from "foobarbaz" 6 "ba" @= 6
let _ = (fun () -> find_from "foobarbaz" 7 "ba") @=! Not_found

let rfind_from str start_pos sub =
  let len = length str in
  let start_pos = if start_pos < 0 then len + start_pos else start_pos in
  let sublen = length sub in
  let search_range = start_pos in
  if search_range > len then raise Not_found;
  if sublen == 0 then start_pos (* if [sub] is the empty string, by convention,
                                   it may be found wherever we started searching.*)
  else fst (rfind_matching (match_string sub) str start_pos start_pos)

let _ = (fun () -> rfind_from "foobarbaz" 2 "ba") @=! Not_found
let _ = rfind_from "foobarbaz" 3 "ba" @= 3
let _ = rfind_from "foobarbaz" 4 "ba" @= 3
let _ = rfind_from "foobarbaz" 5 "ba" @= 3
let _ = rfind_from "foobarbaz" 6 "ba" @= 6
let _ = rfind_from "foobarbaz" 7 "ba" @= 6

let find str sub = find_from str 0 sub

let _ = find "foobarbaz" "ba" @= 3
let _ = find "foobarbaz" "baz" @= 6

let rfind str sub = rfind_from str (length str - 1) sub
(*
let _ = rfind_from "foobarbaz" 5 "ba"
let _ = rfind_from "foobarbaz" 6 "ba"
let _ = rfind_from "foobarbaz" 7 "ba"
let _ = rfind_matching (match_chars "bar") "foobarbaz" 6
let _ = rfind_matching (rmatch_chars "bar") "foobarbaz" 6
let _ = find "foobarbaz" "ba"
let _ = rfind "foobarbaz" "ba"
*)
(******************************************************************************)
(** {6 Twiddling} *)

let take_matching m str ofs len =
  let cnt = m str ofs len in
  sub str ofs cnt

let drop_matching m str ofs len =
  let cnt = m str ofs len in
  sub str (ofs + cnt) (-cnt - 1)

let split_matching m str ofs len =
  let pos, cnt = find_matching m str ofs len in
  head str pos, tail str (pos + cnt)

let _ = split_matching (match_string "ba") "foobarbaz" 0 9 @= ("foo", "rbaz")

let rsplit_matching m str ofs len =
  let pos, cnt = rfind_matching m str ofs len in
  head str pos, tail str (pos + cnt)

let splice_at repl pos cnt =
  "String.splice_at" @> fun str ofs len ->
  let pos2 = pos + cnt in
  let repl_len = length repl in
  let r = create (len - cnt + repl_len) in
  unsafe_blit str ofs r 0 pos;
  unsafe_blit repl 0 r pos repl_len;
  unsafe_blit str pos2 r (pos + repl_len) (len - pos2);
  r

let subst_matching m repl str ofs len =
  let pos, cnt = find_matching m str ofs len in
  splice_at repl pos cnt str ofs len

let rsubst_matching m repl str ofs len =
  let pos, cnt = rfind_matching m str ofs len in
  splice_at repl pos cnt str ofs len

let subst str sub by =
  subst_matching (match_string sub) by str 0 (-1)

let rsubst str sub by =
  rsubst_matching (match_string sub) by str (-1) (-1)

let rec fold_left_substring_matching match_delim f acc str ofs len =
  let rec loop acc srchpos srchlen =
    if srchlen > 0 then
      let pos, len =
        try find_matching match_delim str srchpos srchlen
        with Not_found -> srchpos + srchlen, 0
      in
      let acc = f acc str srchpos (pos - srchpos) in
      loop acc (pos + len) (srchlen + srchpos - pos - len)
    else acc
  in loop acc ofs len

let fold_left_matching match_delim f acc str =
  fold_left_substring_matching match_delim
    (fun acc str ofs len -> f acc (sub str ofs len))
    acc str 0 (length str)

let _ = fold_left_matching (match_char ',')
    (fun acc str -> str::acc) [] "foo,bar,baz" @= ["baz"; "bar"; "foo"];;
let _ = fold_left_matching (match_string "ba")
    (fun acc str -> str::acc) [] "foo,bar,baz" @= ["z"; "r,"; "foo,"];;

let map_matching m f = fold_left_matching m (fun acc s -> f s :: acc) []
let iter_matching m f = fold_left_matching m (fun () -> f) ()

let rec fold_right_substring_matching match_delim f str ofs len acc =
  let rec loop acc srchpos srchlen =
    if srchlen > 0 then
      let pos, len =
        try rfind_matching match_delim str srchpos srchlen
        with Not_found -> 0, 0
      in
      let acc = f str (pos + len) (srchpos - pos - len + 1) acc in
      loop acc (pos - 1) (srchlen - srchpos + pos - 1)
    else acc
  in loop acc ofs len

let fold_right_matching match_delim f str acc =
  let strlen = length str in
  fold_right_substring_matching match_delim
    (fun str ofs len acc -> f (sub str ofs len) acc)
    str (strlen - 1) strlen acc

let _ = fold_right_matching (match_char ',')
    (fun str acc -> str::acc) "a,b,c" [] @= ["a"; "b"; "c"];;

let _ = fold_right_matching (match_string "ba")
    (fun str acc -> str::acc) "foobarbaz" [] @= ["foo"; "r"; "z"];;

let rmap_matching m f s = fold_right_matching m (fun s acc -> f s :: acc) s []
let riter_matching m f s = fold_right_matching m (fun s () -> f s) s ()

let nsplit_matching m str ofs len =
  fold_right_substring_matching m
    (fun str ofs len acc -> sub str ofs len :: acc)
    str ofs len []

let nsubst_matching m repl str ofs len =
  let buf = Buffer.create (length str) in
  let first = ref true in
  fold_left_substring_matching m
    (fun () str ofs len ->
      if !first then first := false
      else Buffer.add_string buf repl;
      Buffer.add_substring buf str ofs len)
    () str ofs len;
  Buffer.contents buf

(******************************************************************************)
(** {6 Splitting} *)

(* Unfortunately, the name 'exists' is inconsistent with the other
   collections (if we consider a string to be a collection of chars):
let exists str sub =
	try
		ignore(find str sub);
		true
	with
		Not_found -> false
*)
(*
let strip ?(chars=" \t\r\n") s =
	let p = ref 0 in
	let l = length s in
	while !p < l && contains chars (unsafe_get s !p) do
		incr p;
	done;
	let p = !p in
	let l = ref (l - 1) in
	while !l >= p && contains chars (unsafe_get s !l) do
		decr l;
	done;
	sub s p (!l - p + 1)
*)
let strip ?(chars=" \n\t\r") s =
  let hd_cnt = match_chars chars s 0 (-1) in
  let tl_cnt = rmatch_chars chars s (-1) (-1) in
  sub s hd_cnt (-hd_cnt - tl_cnt - 1)

(*
let left r len = sub r 0 len
let right r len = let rlen = length r in sub r (rlen - len) len
let head = left
let tail r pos = sub r pos (length r - pos)
*)

let split_at pos str =
  let len = length str in
  let pos = if pos < 0 then len + pos else pos in
  if pos < 0 || pos > len then invalid_arg "String.split_at";
  sub str 0 pos, sub str pos (len - pos)

let split str sep =
  let p = find str sep in
  head str p, tail str (p + length sep)

let rsplit str sep =
  let p = rfind str sep in
  head str p, tail str (p + length sep)

(* XXX this seems very broken...
(**
   An implementation of [nsplit] in one pass.

   This implementation traverses the string backwards, hence building the list
   of substrings from the end to the beginning, so as to avoid a call to [List.rev].
*)
let nsplit str sep =
  if str = "" then []
  else let seplen = String.length sep in
       let rec aux acc ofs = match 
	 try Some(rfind_from str ofs sep)
	 with Not_found -> None
       with Some idx -> 
	 (*at this point, [idx] to [idx + seplen] contains the separator, which is useless to us
	   on the other hand, [idx + seplen] to [ofs] contains what's just after the separator,
	   which is s what we want*)
	 let end_of_occurrence = idx + seplen in
	   if end_of_occurrence >= ofs then aux acc idx (*We may be at the end of the string*)
	   else aux ( sub str end_of_occurrence ( ofs - end_of_occurrence ) :: acc ) idx 
	 |  None     -> (sub str 0 ofs)::acc
       in
	 aux [] (length str - 1 )
*)

let nsplit str sep =
  fold_right_matching (match_string sep) (fun str rest -> str :: rest) str []

let join = concat

let splice s1 ofs len s2 = splice_at s2 ofs len s1 0 (-1)

let slice ?(first=0) ?(last=Sys.max_string_length) s =
	let clip _min _max x = max _min (min _max x) in
	let i = clip 0 (length s)
		(if (first<0) then (length s) + first else first)
	and j = clip 0 (length s)
		(if (last<0) then (length s) + last else last)
	in
	if i>=j || i=length s then
		create 0
        else
          	sub s i (j-i)

let lchop s =
	if s = "" then "" else sub s 1 (-2)

let rchop s =
	if s = "" then "" else sub s 0 (-2)

(******************************************************************************)
(** {6 Conversions} *)

let of_int = string_of_int

let of_float = string_of_float

let of_char = make 1

let to_int s = int_of_string s

let to_float s = float_of_string s

let enum s =
  let l = length s in
  let rec make i =
    Enum.make 
      ~next:(fun () ->
	       if !i = l then
		 raise Enum.No_more_elements
	       else
		 unsafe_get s (Ref.post_incr i)
	    )
      ~count:(fun () -> l - !i)
      ~clone:(fun () -> make (Ref.copy i))
  in
    make (ref 0)

let backwards s =
      let rec make i =
	Enum.make 
	  ~next:(fun () ->
		   if !i <= 0 then
		     raise Enum.No_more_elements
		   else
		     unsafe_get s (Ref.pre_decr i)
		)
	  ~count:(fun () -> !i)
	  ~clone:(fun () -> make (Ref.copy i))
      in
	make (ref (length s))

let of_enum e =
  let l = Enum.count e in
  let s = create l in
  let i = ref 0 in
    Enum.iter (fun c -> unsafe_set s (Ref.post_incr i) c) e;
    s

let of_backwards e =
  let l = Enum.count e in
  let s = create l in
  let i = ref (l - 1) in
    Enum.iter (fun c -> unsafe_set s (Ref.post_decr i) c) e;
    s

(******************************************************************************)
(** {6 Collection Operations} *)

let map f s =
	let len = length s in
	let sc = create len in
	for i = 0 to len - 1 do
		unsafe_set sc i (f (unsafe_get s i))
	done;
	sc

let filter_map f s =
  let len = length s          in
  let sc  = Buffer.create len in
    for i = 0 to len - 1 do
      match f (unsafe_get s i) with
	| Some c -> Buffer.add_char sc c
	| None   -> ()
    done;
    Buffer.contents sc

let filter f s =
  let len = length s          in
  let sc  = Buffer.create len in
    for i = 0 to len - 1 do
      let c = unsafe_get s i in
	if f c then Buffer.add_char sc c
    done;
    Buffer.contents sc

(* fold_left and fold_right by Eric C. Cooper *)
let fold_left f init str =
  let n = String.length str in
  let rec loop i result =
    if i = n then result
    else loop (i + 1) (f result str.[i])
  in
  loop 0 init

let fold_right f str init =
  let n = String.length str in
  let rec loop i result =
    if i = 0 then result
    else
      let i' = i - 1 in
      loop i' (f str.[i'] result)
  in
  loop n init

let for_all p str =
  let n = length str in
  let rec loop i =
    if i == n then true
    else if not (p str.[i]) then false
    else loop (i + 1)
  in loop 0

let exists p str =
  let n = length str in
  let rec loop i =
    if i = n then false
    else if p str.[i] then true
    else loop (i + 1)
  in loop 0

(******************************************************************************)
(** {6 Transformations} *)

(* explode and implode from the OCaml Expert FAQ. *)
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let to_list = explode

let implode l =
  let res = String.create (List.length l) in
  let rec imp i = function
  | [] -> res
  | c :: l -> res.[i] <- c; imp (i + 1) l in
  imp 0 l

let of_list = implode

(*
let replace_chars f s =
	let len = String.length s in
	let tlen = ref 0 in
	let rec loop i acc =
		if i = len then
			acc
		else 
			let s = f (unsafe_get s i) in
			tlen := !tlen + length s;
			loop (i+1) (s :: acc)
	in
	let strs = loop 0 [] in
	let sbuf = create !tlen in
	let pos = ref !tlen in
	let rec loop2 = function
		| [] -> ()
		| s :: acc ->
			let len = length s in
			pos := !pos - len;
			blit s 0 sbuf !pos len;
			loop2 acc
	in
	loop2 strs;
	sbuf
*)
let replace_chars f s =
  let len = length s          in
  let sc  = Buffer.create len in
  for i = 0 to len - 1 do
    let c = unsafe_get s i in
    Buffer.add_string sc (f c)
  done;
  Buffer.contents sc

(* concatenation creates unnecessary garbage...
let replace ~str ~sub ~by =
	try
		let i = find str sub in
		(true, (slice ~last:i str) ^ by ^
                   (slice ~first:(i+(String.length sub)) str))
        with
		Not_found -> (false, String.copy str)
*)
let replace ~str ~sub ~by =
  try true, subst str sub by with Not_found -> false, copy str

let repeat s n =
  let buf = Buffer.create ( n * (String.length s) ) in
    for i = 1 to n do Buffer.add_string buf s done;
    Buffer.contents buf

(*
let trim s =
  let len = length s          in
  let rec aux_1 i = (*locate leading whitespaces*)
    if   i = len then None (*The whole string is whitespace*)
    else if ExtChar.Char.is_whitespace (unsafe_get s i) then aux_1 (i + 1)
    else Some i in
  match aux_1 0 with
    | None -> ""
    | Some last_leading_whitespace ->
  let rec aux_2 i =
    if   i < 0 then None(*?*)
    else if ExtChar.Char.is_whitespace (unsafe_get s i) then aux_2 (i - 1)
    else Some i in
  match aux_2 (len - 1) with
    | None -> ""
    | Some first_trailing_whitespace ->
	sub s last_leading_whitespace (first_trailing_whitespace - last_leading_whitespace + 1)
*)
let ltrim str = drop_matching match_whitespace str 0 (-1)

let _ = ltrim " foo" @= "foo"

let rtrim str = drop_matching rmatch_whitespace str (-1) (-1)

let _ = rtrim "foo " @= "foo"

let trim str =
  let hd_cnt = match_whitespace str 0 (-1) in
  let tl_cnt = rmatch_whitespace str (-1) (-1) in
  sub str hd_cnt (-hd_cnt - tl_cnt - 1)

let _ = trim " foo" @= "foo"
let _ = trim "foo " @= "foo"
let _ = trim "\t \r\n\t\tfoo    \n\n" @= "foo"
let _ = trim "\t \r\na\t\tfoo\na\r\r" @= "a\t\tfoo\na"

(******************************************************************************)
(** {6 Comparisons} *)

let is_empty s = length s == 0

let icompare s1 s2 =
  (* case-insensitive compare without allocation - modeled after
     Pervasives.compare for Strings *)
  let len1 = length s1 in
  let len2 = length s2 in
  let len = int_min len1 len2 in
  let rec loop i =
    if i >= len then
      if len1 != len2 then len1 - len2 else 0
    else
      let v = ExtChar.Char.icompare (unsafe_get s1 i) (unsafe_get s2 i) in
      if v == 0 then loop (i + 1)
      else v
  in
  if s1 == s2 then 0 else loop 0

let contains_matching m str ofs len =
  try ignore (find_matching m str ofs len); true
  with Not_found -> false

let contains_string str sub =
  contains_matching (match_string sub) str 0 (length str)

let starts_with str sub =
  let len = length str in
  let sublen = length sub in
  sublen <= len && match_string sub str 0 len == sublen

let ends_with str sub =
  let len = length str in
  let sublen = length sub in
  sublen <= len && match_string sub str (len - sublen) sublen == sublen

let _ = ends_with "foobarbaz" "baz" @= true
let _ = ends_with "foobarbaz" "rba" @= false
let _ = ends_with "foobarbaz" "ban" @= false
let _ = ends_with "foo" "foofoo" @= false

type t_alias = t (* needed for IString  breaks type t = t *)

module IString =
struct
  type t = t_alias
  let compare = icompare
end

let numeric_compare s1 s2 =
(* TODO pluggable transformation functions (for lowercase) *)
(*  let s1 = String.lowercase s1 and s2 = String.lowercase s2 in*)
  let l1 = String.length s1 and l2 = String.length s2 in
  let rec pos_diff i =  (* finds the first position where the strings differ *)
    if i = l1 then -2 else if i = l2 then -1
    else if s1.[i] = s2.[i] then pos_diff (i+1) else i
  and num_end i s = (* scans for the end of a numeric value *)
    try (* TODO: bounds check here *)
      if s.[i] >= '0' && s.[i] <= '9' then num_end (i+1) s else i
    with _ -> i-1 (* let ocaml do our bounds checking for us *)
  in
  if l1 = l2 then String.compare s1 s2
  else let d = pos_diff 0 in
  if d = -2 then -1 else if d = -1 then 1 else
    let e1 = num_end d s1 and e2 = num_end d s2 in
    if e1 = d || e2 = d then Pervasives.compare s1 s2
      (*    else if e1 <> e2 then e1 - e2 else Pervasives.compare s1 s2 *)
    else begin
(*      Printf.eprintf "Compare: %s & %s @ d:%d e1:%d e2:%d->" s1 s2 d e1 e2;*)
      let n1 = Int64.of_string (String.sub s1 d (e1-d))
	(* FIXME?: trailing numbers must be less than Int64.max_int *)
      and n2 = Int64.of_string (String.sub s2 d (e2-d)) in
(*      Printf.eprintf " %Ld & %Ld\n" n1 n2;*)
      Int64.compare n1 n2 (* FIXME: ignores text after equal numbers -- "a1b" = "a01c" *)
    end

module NumString =
struct
  type t = t_alias
  let compare = numeric_compare
end

(******************************************************************************)
(** {7 Printing}*)

let print         = InnerIO.nwrite
let println out s = InnerIO.nwrite out s; InnerIO.write out '\n'
let print_quoted out s = ExtPrintf.Printf.fprintf out "%S" s

let quote = ExtPrintf.Printf.sprintf2 "%S"

(******************************************************************************)

module Cap =
struct
type 'a t = string with sexp

let is_empty      = is_empty
let make          = make
let init          = init
let copy          = copy
let sub           = sub

let enum          = enum
let of_enum       = of_enum
let backwards     = backwards
let of_backwards  = of_backwards
let of_list       = of_list
let to_list       = to_list
let of_int        = of_int
let to_int        = to_int
let of_float      = of_float
let to_float      = to_float
let of_char       = of_char

let map           = map
let fold_left     = fold_left
let fold_right    = fold_right
let filter        = filter
let filter_map    = filter_map
let iter          = iter
let for_all       = for_all
let exists        = exists

let find_matching       = find_matching
let rfind_matching      = rfind_matching
let fold_left_matching  = fold_left_matching
let fold_right_matching = fold_right_matching
let take_matching       = take_matching
let drop_matching       = drop_matching
let split_matching      = split_matching
let rsplit_matching     = rsplit_matching
let nsplit_matching     = nsplit_matching
let subst_matching      = subst_matching
let rsubst_matching     = rsubst_matching
let nsubst_matching     = nsubst_matching
let match_string        = match_string
let match_substring     = match_substring
let match_chars         = match_chars
let rmatch_chars        = rmatch_chars
let match_whitespace    = match_whitespace
let rmatch_whitespace   = rmatch_whitespace
let match_char          = match_char

let index         = index
let rindex        = rindex
let index_from    = index_from
let rindex_from   = rindex_from
let contains      = contains
let contains_from = contains_from
let rcontains_from= rcontains_from
let find          = find
let find_from     = find_from
let rfind         = rfind
let rfind_from    = rfind_from

let lchop         = lchop
let rchop         = rchop
let trim          = trim
let left          = left
let right         = right
let head          = head
let tail          = tail
let strip         = strip
let uppercase     = uppercase
let lowercase     = lowercase
let capitalize    = capitalize
let uncapitalize  = uncapitalize
let fill          = fill
let blit          = blit
let concat        = concat
let escaped       = escaped
let quote         = quote
let replace_chars = replace_chars
let replace       = replace
let repeat        = repeat

let split         = split
let rsplit        = rsplit
let nsplit        = nsplit
let split_at      = split_at
let splice        = splice
let join          = join
let slice         = slice
let explode       = explode
let implode       = implode

let compare       = compare
let icompare      = icompare
let starts_with   = starts_with
let ends_with     = ends_with
let contains_string     = contains_string
let numeric_compare     = numeric_compare

external of_string : string -> _ t                = "%identity"
external to_string : [`Read | `Write] t -> string = "%identity"
external read_only : [> `Read] t -> [`Read] t     = "%identity"
external write_only: [> `Write] t -> [`Write] t   = "%identity"

external length : _ t  -> int = "%string_length"
external get : [> `Read] t -> int -> char = "%string_safe_get"
external set : [> `Write] t -> int -> char -> unit = "%string_safe_set"
external create : int -> _ t = "caml_create_string"

end
end
