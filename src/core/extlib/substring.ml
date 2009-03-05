(*
 * Substring - Functions for manipulating ranges of strings while minimizing
 * allocation. (Parallels the String interface.)
 * Copyright (C) 2009 Warren Harris <warren@metaweb.com>
 *
 * Re-implementation of SML's Substring library in OCaml.
 * Copyright (C) 2008 Edgar Friendly <thelema314@gmail.com>
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
 *
 * See http://www.itu.dk/~sestoft/mosmllib/Substring.html for documentation
 *
 *)

open ExtString
open ExtChar

type t = string * int * int (* string, offset, length *)
type substring = t

(*Minor optimization.*)
let int_min (x:int) (y:int) = if x < y then x else y
let int_max (x:int) (y:int) = if x < y then y else x

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
let ( @> ) msg f ((s:string),o,l) ofs len = (* normalize to a range above the ofs position *)
  let len = if len < 0 then l + len + 1 else len in
  if len < 0 || len > l then invalid_arg (msg ^ " len=" ^ string_of_int len);
  let ofs = if ofs < 0 then l + ofs else ofs in
  if ofs < 0 || ofs + len > l then invalid_arg (msg ^ " ofs=" ^ string_of_int ofs);
  f s (o + ofs) len

let ( @< ) msg f (s,o,l) ofs len = (* normalize to a range below the ofs position *)
  let len = if len < 0 then l + len + 1 else len in
  if len < 0 || len > l then invalid_arg (msg ^ " len=" ^ string_of_int len);
  let ofs = if ofs < 0 then l + ofs else ofs in
  if ofs < 0 || ofs - len < -1 then invalid_arg (msg ^ " ofs=" ^ string_of_int ofs);
  f s (o + ofs) len

(******************************************************************************)
(** {6 Substring Structure} *)

let string (s,o,l) = s
let offset (s,o,l) = o
let length (s,o,l) = l

(******************************************************************************)
(** {6 Constructors} *)

let sub = "Substring.sub" @> (fun s o l -> s, o, l)

let empty () = "", 0, 0

let is_empty (_,_,l) = l = 0

let create len = String.create len, 0, len

let make len c = String.make len c, 0, len

let init len f = String.init len f, 0, len

let of_string s = s, 0, String.length s

let substring str ofs len = sub (of_string str) ofs len

let left s cnt = sub s 0 cnt
let right s cnt = sub s (-cnt) cnt
let head s pos = sub s 0 pos
let tail s pos = sub s pos (-pos - 1)

let _ = sub (of_string "foobar") (-3) 2 @= ("foobar", 3, 2)
let _ = sub (of_string "foo") 0 3 @= ("foo", 0, 3)
let _ = sub (of_string "foo") 1 2 @= ("foo", 1, 2)
let _ = sub (of_string "foobar") (-2) 1 @= ("foobar", 4, 1)
let _ = sub (of_string "foobar") 0 (-1) @= ("foobar", 0, 6)
let _ = sub (of_string "foobar") 1 (-3) @= ("foobar", 1, 4)

(******************************************************************************)
(** {6 Accessors} *)

let get (s,o,l) i =
  ("Substring.get" @> (fun s o l -> String.unsafe_get s (o + i)))
    (s,o,l) i l

let set (s,o,l) i c =
  ("Substring.set" @> (fun s o l -> String.unsafe_set s (o + i) c))
    (s,o,l) i l

(******************************************************************************)
(** {7 Match Functions} *)

type match_fun = t -> int -> int -> int

let match_string (str:string) : match_fun =
  "Substring.match_string" @> String.unsafe_match_substring str 0 (String.length str)

let match_substring ss =
  ("Substring.match_substring" @>
    ("Substring.match_substring" @> String.unsafe_match_substring) ss 0 (length ss))

let match_chars chars =
  "Substring.match_chars" @> String.unsafe_count_after (String.contains chars)

let rmatch_chars chars =
  "Substring.rmatch_chars" @< String.unsafe_count_before (String.contains chars)

let match_whitespace =
  "Substring.match_whitespace" @> String.unsafe_count_after ExtChar.Char.is_whitespace

let rmatch_whitespace =
  "Substring.rmatch_whitespace" @< String.unsafe_count_before ExtChar.Char.is_whitespace

let match_char c (s,o,l) srchpos srchlen =
  let srchlen = if srchlen < 0 then l + srchlen + 1 else srchlen in
  if srchlen < 1 || srchlen > l then invalid_arg "Substring.match_char";
  let srchpos = if srchpos < 0 then l + srchpos else srchpos in
  if srchpos < 0 || srchpos > l then invalid_arg "Substring.match_char";
  if String.unsafe_get s (o + srchpos) == c then 1 else 0

(******************************************************************************)
(** {6 Generalized Finding} *)

let find_matching m =
  "Substring.find_matching" @> fun str ofs len ->
  let ss = of_string str in
  let stop = ofs + len in
  let rec loop i =
    if i >= stop then raise Not_found
    else
      let cnt = m ss i (stop - i) in
      if cnt > 0 then i, cnt (* found *)
      else loop (i + 1)
  in loop ofs

let rfind_matching m =
  "Substring.rfind_matching" @< fun str ofs len ->
  let ss = of_string str in
  let strlen = length ss in
  let stop = ofs - len in
  let rec loop i =
    if i <= stop then raise Not_found
    else
      let cnt = m ss i (strlen - i) in
      if cnt > 0 then i, cnt (* found *)
      else loop (i - 1)
  in loop ofs

let find_from ss start_pos sub =
  let len = length ss in
  let start_pos = if start_pos < 0 then len + start_pos else start_pos in
  let sublen = String.length sub in
  let search_range = start_pos + sublen in
  if search_range > len then raise Not_found;
  if sublen == 0 then start_pos (* if [sub] is the empty string, by convention,
                                   it may be found wherever we started searching.*)
  else fst (find_matching (match_string sub) ss start_pos (len - start_pos))

let rfind_from ss start_pos sub =
  let len = length ss in
  let start_pos = if start_pos < 0 then len + start_pos else start_pos in
  let sublen = String.length sub in
  let search_range = start_pos in
  if search_range > len then raise Not_found;
  if sublen == 0 then start_pos (* if [sub] is the empty string, by convention,
                                   it may be found wherever we started searching.*)
  else fst (rfind_matching (match_string sub) ss start_pos start_pos)

let find str sub = find_from str 0 sub

let rfind str sub = rfind_from str (length str - 1) sub

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

let rsplit_matching m str ofs len =
  let pos, cnt = rfind_matching m str ofs len in
  head str pos, tail str (pos + cnt)

let splice_at (repl, repl_ofs, repl_len) pos cnt =
  "Substring.splice_at" @> fun str ofs len ->
  let pos2 = pos + cnt in
  let r = String.create (len - cnt + repl_len) in
  String.unsafe_blit str ofs r 0 pos;
  String.unsafe_blit repl repl_ofs r pos repl_len;
  String.unsafe_blit str pos2 r (pos + repl_len) (len - pos2);
  of_string r

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

let rmap_matching m f s = fold_right_matching m (fun s acc -> f s :: acc) s []
let riter_matching m f s = fold_right_matching m (fun s () -> f s) s ()

let nsplit_matching m str ofs len =
  fold_right_substring_matching m
    (fun str ofs len acc -> sub str ofs len :: acc)
    str ofs len []

let nsubst_matching m (repl, repl_ofs, repl_len) str ofs len =
  let buf = Buffer.create len in
  let first = ref true in
  fold_left_substring_matching m
    (fun () str ofs len ->
      if !first then first := false
      else Buffer.add_substring buf repl repl_ofs repl_len;
      let str, ofs, len = sub str ofs len in
      Buffer.add_substring buf str ofs len)
    () str ofs len;
  of_string (Buffer.contents buf)

(******************************************************************************)
(** {6 Splitting} *)

let strip ?(chars=" \n\t\r") s =
  let hd_cnt = match_chars chars s 0 (-1) in
  let tl_cnt = rmatch_chars chars s (-1) (-1) in
  sub s hd_cnt (-hd_cnt - tl_cnt - 1)

let split_at pos (str, ofs, len) =
  let pos = if pos < 0 then len + pos else pos in
  if pos < 0 || pos > len then invalid_arg "Substring.split_at";
  (str, ofs, pos), (str, ofs + pos, len - pos)

let split str sep =
  let p = find str sep in
  head str p, tail str (p + String.length sep)

let rsplit str sep =
  let p = rfind str sep in
  head str p, tail str (p + String.length sep)

let nsplit str sep =
  fold_right_matching (match_string sep) (fun str rest -> str :: rest) str []

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
	if is_empty s then s else sub s 1 (-2)

let rchop s =
	if is_empty s then s else sub s 0 (-2)

let split_on_char c (str, ofs, len) = 
  let rec loop acc last_pos pos =
    if pos = -1 then
      (str, 0, last_pos) :: acc
    else
      if str.[pos] = c then
        let pos1 = pos + 1 in
        let sub_str = str,pos1,(last_pos - pos1) in
        loop (sub_str :: acc) pos (pos - 1)
      else loop acc last_pos (pos - 1)
  in
  loop [] len (len - 1)

let split_on_pipe str = split_on_char '|' str;;
let split_on_dot str = split_on_char '.' str;;
let split_on_comma str = split_on_char ',' str;;
let split_on_slash str = split_on_char '/' str;;

(******************************************************************************)
(** {6 Conversions} *)

let to_string (s,o,l) = String.sub s o l

let enum (s,o,l) =
  let rec make i =
    Enum.make
      ~next:(fun () ->
	       if !i = l then
		 raise Enum.No_more_elements
	       else
		 String.unsafe_get s (Ref.post_incr i)
	    )
      ~count:(fun () -> l - !i)
      ~clone:(fun () -> make (Ref.copy i))
  in
    make (ref o)

let of_enum e = of_string (String.of_enum e)

let backwards (s,o,l) =
      let rec make i =
	Enum.make
	  ~next:(fun () ->
		   if !i <= o then
		     raise Enum.No_more_elements
		   else
		     String.unsafe_get s (Ref.pre_decr i)
		)
	  ~count:(fun () -> !i)
	  ~clone:(fun () -> make (Ref.copy i))
      in
	make (ref l)

let of_backwards e = of_string (String.of_backwards e)

let of_int i = of_string (string_of_int i)
let to_int ss =
  let str = to_string ss in (* XXX fix to not allocate *)
  int_of_string str

let of_float f = of_string (string_of_float f)
let to_float ss =
  let str = to_string ss in (* XXX fix to not allocate *)
  float_of_string str

let of_char c = make 1 c

(*
let of_chan chan =
  let tempsize = 16384 in
  let buf = Buffer.create tempsize 
  and tmp = String.create tempsize in
  let n = ref 0 in
  while n := input chan tmp 0 tempsize; !n > 0 do
    Buffer.add_substring buf tmp 0 !n;
  done;
  Buffer.contents buf, 0, Buffer.length buf
*)

let of_input inp =
  let tempsize = 16384 in
  let buf = Buffer.create tempsize
  and tmp = String.create tempsize in
  let n = ref 0 in
  while n := IO.input inp tmp 0 tempsize; !n > 0 do
    Buffer.add_substring buf tmp 0 !n;
  done;
  Buffer.contents buf, 0, Buffer.length buf

(******************************************************************************)
(** {6 Collection Operations} *)

let map f (str, ofs, len) =
  let new_str = String.create len in
  for i = 0 to len-1 do
    new_str.[i] <- f str.[i + ofs];
  done;
  of_string new_str

let fold_left f init (str, ofs, len) =
  let rec loop i result =
    if i = len then result
    else loop (i + 1) (f result str.[i])
  in
  loop ofs init

let fold_right f (str, ofs, len) init =
  let rec loop i result =
    if i = ofs then result
    else loop (i - 1) (f str.[i-1] result)
  in
  loop (ofs+len) init

let iter f (str, ofs, len) =
  for i = ofs to ofs+len-1 do
    f str.[i];
  done

let filter_map f (s,o,l) =
  let sc  = Buffer.create l in
    for i = o to l - 1 do
      match f (String.unsafe_get s i) with
	| Some c -> Buffer.add_char sc c
	| None   -> ()
    done;
    of_string (Buffer.contents sc)

let filter f ss = filter_map (fun c -> if f c then Some c else None) ss

let for_all p str =
  let n = length str in
  let rec loop i =
    if i == n then true
    else if not (p (get str i)) then false
    else loop (i + 1)
  in loop 0

let exists p str =
  let n = length str in
  let rec loop i =
    if i = n then false
    else if p (get str i) then true
    else loop (i + 1)
  in loop 0

(******************************************************************************)
(** {6 Transformations} *)

let explode (s,o,l) =
  let rec exp i acc =
    if i < o then acc else exp (i - 1) (s.[i] :: acc) in
  exp (o + l - 1) []

let to_list = explode

let implode l = of_string (String.of_list l)
let of_list l = implode l

let replace_chars f (s, ofs, len) =
  let sc  = Buffer.create len in
  for i = ofs to len - 1 do
    let c = String.unsafe_get s i in
    let s, o, l = f c in
    Buffer.add_substring sc s o l
  done;
  of_string (Buffer.contents sc)

let copy ss = of_string (String.copy (to_string ss))

let replace ~str ~sub ~by =
  try true, subst_matching (match_substring sub) by str 0 (-1)
  with Not_found -> false, copy str

let repeat (s,o,l) n =
  let buf = Buffer.create ( n * l ) in
  for i = 1 to n do Buffer.add_substring buf s o l done;
  of_string (Buffer.contents buf)

let ltrim str = drop_matching match_whitespace str 0 (-1)
let rtrim str = drop_matching rmatch_whitespace str (-1) (-1)

let trim str =
  let hd_cnt = match_whitespace str 0 (-1) in
  let tl_cnt = rmatch_whitespace str (-1) (-1) in
  sub str hd_cnt (-hd_cnt - tl_cnt - 1)

let uppercase ss =
  let str = to_string ss in (* XXX fix to not allocate *)
  of_string (String.uppercase str)

let lowercase ss =
  let str = to_string ss in (* XXX fix to not allocate *)
  of_string (String.lowercase str)

let capitalize ss =
  let str = to_string ss in (* XXX fix to not allocate *)
  of_string (String.capitalize str)

let uncapitalize ss =
  let str = to_string ss in (* XXX fix to not allocate *)
  of_string (String.uncapitalize str)

let fill (s,o,l) ofs len c =
  if ofs < 0 || len < 0 || ofs > l - len then invalid_arg "fill"
  else String.unsafe_fill s (o + ofs) len c

let blit (s1,o1,l1) ofs1 (s2,o2,l2) ofs2 len =
  if len < 0 || ofs1 < 0 || ofs1 > l1 - len
             || ofs2 < 0 || ofs2 > l2 - len then invalid_arg "blit"
  else String.unsafe_blit s1 (o1 + ofs1) s2 (o2 + ofs2) len

let concat sep ssl =
  let sl = String.length sep in
  let len = List.fold_left (fun acc (_,_,l) ->acc+l+sl) 0 ssl in
  let item = String.create (if len > 0 then len - sl else 0) in
  let write =
    let pos = ref 0 in
    fun (s,o,len) ->
      String.unsafe_blit s o item !pos len;
      pos := !pos + len;
      String.unsafe_blit sep 0 item !pos sl;
      pos := !pos + sl
  in
  List.iter write ssl;
  of_string item

let join = concat

external is_printable: char -> bool = "caml_is_printable"

let escaped ((s,o,l) as ss) =
  let n = ref 0 in
    for i = o to l - 1 do
      n := !n +
        (match String.unsafe_get s i with
           '\"' | '\\' | '\n' | '\t' -> 2
          | c -> if is_printable c then 1 else 4)
    done;
    if !n = l then ss else begin
      let s' = String.create !n in
        n := 0;
        for i = o to l - 1 do
          begin
            match String.unsafe_get s i with
              ('\"' | '\\') as c ->
                String.unsafe_set s' !n '\\'; incr n; String.unsafe_set s' !n c
            | '\n' ->
                String.unsafe_set s' !n '\\'; incr n; String.unsafe_set s' !n 'n'
            | '\t' ->
                String.unsafe_set s' !n '\\'; incr n; String.unsafe_set s' !n 't'
            | c ->
                if is_printable c then
                  String.unsafe_set s' !n c
                else begin
                  let a = Char.code c in
                  String.unsafe_set s' !n '\\';
                  incr n;
                  String.unsafe_set s' !n (Char.chr (48 + a / 100));
                  incr n;
                  String.unsafe_set s' !n (Char.chr (48 + (a / 10) mod 10));
                  incr n;
                  String.unsafe_set s' !n (Char.chr (48 + a mod 10))
                end
          end;
          incr n
        done;
        of_string s'
      end

(******************************************************************************)
(** {6 Comparisons} *)

let compare (str1, ofs1, len1) (str2, ofs2, len2) =
  (* compare without allocating *)
  let rec loop i =
    if i >= len1 then 0
    else
      let c1 = String.unsafe_get str1 (ofs1 + i) in
      let c2 = String.unsafe_get str2 (ofs2 + i) in
      let v = ExtChar.Char.code c1 - ExtChar.Char.code c2 in
      if v == 0 then loop (i + 1)
      else v
  in
  if len1 > len2 then 1
  else if len1 < len2 then -1
  else if str1 == str2 && ofs1 == ofs2 then 0
  else loop 0

let compare_string ss str = compare ss (of_string str)

let icompare (str1, ofs1, len1) (str2, ofs2, len2) =
  (* case-insensitive compare without allocating - modeled after
     Pervasives.compare for Strings *)
  let len = int_min len1 len2 in
  let rec loop i =
    if i >= len then
      if len1 != len2 then len1 - len2 else 0
    else
      let c1 = String.unsafe_get str1 (ofs1 + i) in
      let c2 = String.unsafe_get str2 (ofs2 + i) in
      let v = ExtChar.Char.icompare c1 c2 in
      if v == 0 then loop (i + 1)
      else v
  in
  if str1 == str2 then 0 else loop 0

let icompare_string ss str = icompare ss (of_string str)

let equals a b = compare a b == 0
let equals_string a b = compare_string a b == 0

let starts_with (s1, ofs1, len1) (s2, ofs2, len2) =
  len1 >= len2 && equals (s1, ofs1, len2) (s2, ofs2, len2)

let starts_with_string ss str = starts_with ss (of_string str)

let ends_with (s1, ofs1, len1) (s2, ofs2, len2) =
  len1 >= len2 && equals (s1, len2 - ofs1, len2) (s2, ofs2, len2)

let ends_with_string ss str = ends_with ss (of_string str)

(* modeled after ocaml's built-in caml_hash_univ_param for strings: *)
let hash (str, ofs, len) =
  let beta = 19 in
  let hv = ref 0 in
    for i = ofs to len - 1 do
      hv := !hv * beta + Char.code str.[i]
    done;
    !hv

(******************************************************************************)
(** {6 Finding} *)

let index_from (str, ofs, len) pos ch =
  let stop = ofs + len - 1 in
  let rec loop i =
    if i > stop then raise Not_found
    else if String.unsafe_get str i == ch then i - ofs
    else loop (i + 1)
  in loop (ofs + pos)

let index ss ch = index_from ss 0 ch

let rindex_from (str, ofs, len) pos ch =
  let rec loop i =
    if i < ofs then raise Not_found
    else if String.unsafe_get str i == ch then i - ofs
    else loop (i - 1)
  in loop (ofs + len - 1)

let rindex ((str, ofs, len) as ss) ch = rindex_from ss len ch

let contains (str, ofs, len) ch =
  let stop = ofs + len in
  let rec loop i =
    if i == stop then false
    else if String.unsafe_get str i == ch then true
    else loop (i + 1)
  in loop ofs

let contains_from ss ofs ch = contains (sub ss ofs (length ss - ofs)) ch

let rcontains (str, ofs, len) ch =
  let rec loop i =
    if i == ofs then false
    else if String.unsafe_get str i == ch then true
    else loop (i - 1)
  in loop (ofs + len - 1)

let rcontains_from ss ofs ch = rcontains (sub ss ofs (length ss - ofs)) ch

let contains_substring_from ss ofs sub =
  try ignore (find_matching (match_substring sub) ss ofs (-1 - ofs)); true
  with Not_found -> false

let contains_substring ss sub = contains_substring_from ss 0 sub
let contains_string_from ss ofs sub = contains_substring_from ss ofs (of_string sub)
let contains_string ss sub = contains_string_from ss 0 sub

let rcontains_substring_from ss ofs sub =
  try ignore (rfind_matching (match_substring sub) ss ofs (-1 - ofs)); true
  with Not_found -> false

let rcontains_substring ss sub = rcontains_substring_from ss 0 sub
let rcontains_string_from ss ofs sub = rcontains_substring_from ss ofs (of_string sub)
let rcontains_string ss sub = rcontains_string_from ss 0 sub

(******************************************************************************)
(** {6 SML Stuff} *)

let dropl p (str,ofs,len) =
  let i = ref 0 in
  while !i < len && p str.[ofs+ !i] do incr i; done;
  (str, ofs+ !i, len- !i)

let dropr p (str, ofs, len) =
  let i = ref len in
  while !i >= 0 && p str.[ofs+ !i] do decr i; done;
  (str, ofs, !i)

let takel p (str,ofs,len) =
  let i = ref 0 in
  while !i < len && p str.[ofs+ !i] do incr i; done;
  (str, ofs, !i)

let taker p (str, ofs, len) =
  let i = ref len in
  while !i >= 0 && p str.[ofs+ !i] do decr i; done;
  (str, ofs+ !i, len- !i)

(** not implemented: position *)

let span (str1, ofs1, len1) (str2, ofs2, len2) =
  if str1 != str2 then invalid_arg "Substring.span";
  if ofs1 > ofs2 + len2 then invalid_arg "Substring.span";
  (str1, ofs1, ofs2+len2-ofs1)

let translate f (str,ofs,len) =
  ExtString.String.init len (fun i -> f str.[ofs+i])

(* XXX reconscile tokens with split(_all) split_fold *)
let tokens p (str,ofs,len) =
  let i = ref 0 and j = ref 0 and acc = RefList.empty () in
  while !j < len do
    while !i < len && p str.[ofs+ !i] do incr i; done;
    j := !i+1;
    while !j < len && not (p str.[ofs+ !j]) do incr j; done;
    RefList.push acc (str, !i, !j);
    i := !j+1;
  done;
  RefList.to_list acc

let fields p (str, ofs, len) =
  let i = ref 0 and j = ref 0 and acc = RefList.empty() in
  while !j < len do
    while !j < len && not (p str.[ofs+ !j]) do incr j; done;
    RefList.push acc (str, !i, !j);
    incr j; i := !j; 
  done;
  RefList.to_list acc

(******************************************************************************)
(** {6 Deprecated Stuff} *)

let extract s o = function
    Some len -> substring s o len
  | None -> substring s o (String.length s - o)

let all = of_string

let base s = s

let getc (str,ofs,len) =
  if len = 0 then None else Some (str.[ofs], (str, ofs+1, len-1))

let first (str,ofs,len) = if len = 0 then None else Some str.[ofs]

let size (_,_,len) = len

(** not implemented: collate *)

let splitl p s =
  let m s o l = if p (get s o) then 1 else 0 in
  let pos, _ = find_matching m s 0 (-1) in
  split_at pos s

let splitr p s =
  let m s o l = if p (get s o) then 1 else 0 in
  let pos, _ = rfind_matching m s 0 (-1) in
  split_at pos s

let is_prefix str1 (str2, ofs, len) =
  let l1 = String.length str1 in
  if l1 > len then false
  else 
    let rec loop i = 
      if i < 0 then true 
      else if str1.[i] <> str2.[ofs+i] then false
      else loop (i-1) in
    loop l1

let triml k (str,ofs,len) =
  if k < 0 then invalid_arg "Substring.triml: negative trim not allowed";
  if k > len then (str, ofs+len, 0) else (str, ofs+k, len-k)

let trimr k (str,ofs,len) =
  if k < 0 then invalid_arg "Substring.triml: negative trim not allowed";
  if k > len then (str, ofs, 0) else (str, ofs, len-k)

(******************************************************************************)
