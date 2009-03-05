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

(** Substrings

    @author Eric Norige <thelema314@gmail.com>
    @author Warren Harris, Metaweb Technologies, Inc.
*)

type t
(**
   [Substring.t] is the type of substrings of a basestring, an efficient 
   representation of a piece of a string.

   A substring (s,i,n) is valid if 0 <= i <= i+n <= size s, 
                  or equivalently, 0 <= i and 0 <= n and i+n <= size s.  

   A valid substring (s, i, n) represents the string s[i...i+n-1].  

   Invariant in the implementation: Any value of type [Substring.t] is valid.
*)

type substring = t (* synonym *)

(******************************************************************************)
(** {6 Substring Structure} *)

val string : t -> string
  (** [string s] returns the underlying [string] associated with the
      substring. *)

val offset : t -> int
  (** [offset s] returns the offset of the substring relative to the
      underlying string. *)

val length : t -> int
  (** [length s] returns the length of a substring. *)

(******************************************************************************)
(** {6 Constructors} *)

val substring : string -> int -> int -> t
  (** [substring s o l] returns a substring from base string [s], offset
      [o] and length [l]. Arguments are checked for validity.
      The supplied string [str] is used as the underlying
      string, and is modified if any destructive operations are performed
      on the substring.

      [substring(s, i, n)] creates the substring (s, i, n), consisting
      of the substring of s with length n starting at i.
      @raises Invalid_argument if i<0 or n<0 or i+n > size s. *)

val empty : unit -> t
  (** [empty ()] returns an empty substring. *)

val is_empty : t -> bool
  (** [is_empty s] returns [true] if [s] is the empty string (that
      is, if its length is 0), [false] otherwise. *)

val create : int -> t
  (** [create n] returns a fresh substring of length [n].
      The string initially contains arbitrary characters.
      @raise Invalid_argument if [n < 0] or [n > Sys.max_string_length]. *)

val make : int -> char -> t
  (** [make n c] returns a fresh substring of length [n],
      filled with the character [c].
      @raise Invalid_argument if [n < 0] or [n > ]{!Sys.max_string_length}. *)

val init : int -> (int -> char) -> t
  (** [init n f] returns a substring of length [n] with the chars
      f 0 , f 1 , f 2 ... f (n-1).
      @raise Invalid_argument if [n < 0] or [n > ]{!Sys.max_string_length}. *)

val copy : t -> t
  (** [copy s] returns a copy of the given substring. The portion of the
      underlying string referenced by the substring is also copied. *)

val sub : t -> int -> int -> t
  (** [sub ss start len] returns a substring of length [len],
      containing the characters number [start] to [start + len - 1] of
      substring [ss]. The underlying string is shared between the two.

      @raise Invalid_argument if [start] and [len] do not designate a
      valid substring of [s]; that is, if [start < 0], or [len < 0],
      or [start + len > ]{!String.length}[ s]. *)

(******************************************************************************)
(** {6 Accessors} *)

val get : t -> int -> char
  (** [get s n] returns character number [n] in substring [s].
      The first character is character number 0.
      The last character is character number [Substring.length s - 1].

      @raise Invalid_argument "index out of bounds"
      if [n] is outside the range 0 to [(Substring.length s - 1)]. *)

val set : t -> int -> char -> unit
  (** [set s n c] modifies substring [s] in place (the underlying [string]
      is modified), replacing the character number [n] by [c].
      @raise Invalid_argument "index out of bounds"
      if [n] is outside the range 0 to [(Substring.length s - 1)]. *)

(******************************************************************************)
(** {6 Conversions} *)

val of_string : string -> t
  (** [of_string str] returns a substring object representing the entire
      string [str]. The supplied string [str] is used as the underlying
      string, and is modified if any destructive operations are performed
      on the substring. *)

val to_string : t -> string
  (** [to_string s] allocates and returns a new string that corresponds
      to the substring [s]. *)

val enum : t -> char Enum.t
  (** [enum s] returns an enumeration of the characters of a substring. *)

val of_enum : char Enum.t -> t
  (** [of_enum e] creates a substring from a character enumeration [e]. *)

val backwards : t -> char Enum.t
  (** [backwards s] returns an enumeration of the characters of a substring,
      from last to first. *)

val of_backwards : char Enum.t -> t
  (** [of_backwards e] creates a substring from an enumeration, starting with
      last character, ending with first. *)

val of_list : char list -> t
  (** [of_list cl] converts a list of characters to a substring. *)

val to_list : t -> char list
  (** [to_list s] converts a substring to the list of its characters. *)

val of_int : int -> t
  (** [of_int i] returns the substring representation of an int. *)

val to_int : t -> int
  (** [to_int s] returns the integer represented by the given substring.
      @raise Failure "int_of_string" if the substring does not represent an integer.*)

val of_float : float -> t
  (** [of_float f] returns the substring representation of an float. *)

val to_float : t -> float
  (** [to_float s] returns the float represented by the given substring.
      @raise Failure "float_of_string" if the substring does not represent a float. *)

val of_char : char -> t
  (** [of_char c] returns a substring containing one given character. *)

val of_input : IO.input -> t
  (** [of_input input] constructs a substring from the characters read
      from [input]. The entire input is read until the end-of-file is
      reached. *)

(******************************************************************************)
(** {6 Character Collection Operations} *)

val map : (char -> char) -> t -> t
  (** [map f s] returns a substring where all characters [c] in [s] have
      been replaced by [f c]. **)

val fold_left : ('a -> char -> 'a) -> 'a -> t -> 'a
  (** [fold_left f a ss] folds [f a c] over characters [c] of substring [ss]
      with accumulator [a] from left to right. That is, evaluates
      [f(f( ... f(f(a, s[i]), s[i+1]) ...), s[i+n-1])]
      tail-recursively, where [ss] = [(s, i, n)]. *)

val fold_right : (char -> 'a -> 'a) -> t -> 'a -> 'a
  (** [fold_right f ss a] folds [f c a] over characters [c] of substring [ss]
      with accumulator [a] from right to left. That is, evaluates
      [f(s[i], f(s[i+1], f(... f(s[i+n-1], a) ...)))]
      tail-recursively, where [ss] = [(s, i, n)]. *)

val filter : (char -> bool) -> t -> t
  (** [filter f s] returns a copy of substring [s] in which only
      characters [c] such that [f c = true] remain. *)

val filter_map : (char -> char option) -> t -> t
  (** [filter_map f s] calls [(f a0) (f a1).... (f an)] where [a0..an] are
      the characters of [s]. It returns the substring of characters [ci]
      such as [f ai = Some ci] (when [f] returns [None], the corresponding
      element of [s] is discarded). *)

val iter : (char -> unit) -> t -> unit
  (** [iter f s] applies [f c] to all characters [c] of substring [s]
      from left to right. *)

val for_all : (char -> bool) -> t -> bool
  (** [for_all p ss] returns [true] if all characters [c] of substring
      [ss] satisfy the predicate [p c]. *)

val exists : (char -> bool) -> t -> bool
  (** [exists p ss] returns [true] if any character [c] of substring
      [ss] satisfies the predicate [p c]. *)

(******************************************************************************)
(** {6 Generalized Finding} *)

type match_fun = t -> int -> int -> int

val find_matching : match_fun -> t -> int -> int -> int * int
val rfind_matching : match_fun -> t -> int -> int -> int * int
val fold_left_matching : match_fun -> ('a -> t -> 'a) -> 'a -> t -> 'a
val fold_right_matching : match_fun -> (t -> 'a -> 'a) -> t -> 'a -> 'a

val map_matching : match_fun -> (t -> 'a) -> t -> 'a list
val rmap_matching : match_fun -> (t -> 'a) -> t -> 'a list
val iter_matching : match_fun -> (t -> unit) -> t -> unit
val riter_matching : match_fun -> (t -> unit) -> t -> unit

val take_matching : match_fun -> t -> int -> int -> t
val drop_matching : match_fun -> t -> int -> int -> t

val split_matching : match_fun -> t -> int -> int -> t * t
val rsplit_matching : match_fun -> t -> int -> int -> t * t
val nsplit_matching : match_fun -> t -> int -> int -> t list

val subst_matching : match_fun -> t -> t -> int -> int -> t
val rsubst_matching : match_fun -> t -> t -> int -> int -> t
val nsubst_matching : match_fun -> t -> t -> int -> int -> t

(******************************************************************************)
(** {7 Match Functions} *)

val match_string : string -> match_fun
val match_substring : t -> match_fun
val match_chars : string -> match_fun
val rmatch_chars : string -> match_fun
val match_whitespace : match_fun
val rmatch_whitespace : match_fun
val match_char : char -> match_fun

(******************************************************************************)
(** {6 Finding} *)

val index : t -> char -> int
  (** [index s c] returns the position of the leftmost
      occurrence of character [c] in substring [s].
      @raise Not_found if [c] does not occur in [s]. *)

val rindex : t -> char -> int
  (** [rindex s c] returns the position of the rightmost
      occurrence of character [c] in substring [s].
      @raise Not_found if [c] does not occur in [s]. *)

val index_from : t -> int -> char -> int
  (** Same as {!index}, but start
      searching at the character position given as second argument.
      [index s c] is equivalent to [index_from s 0 c].*)

val rindex_from : t -> int -> char -> int
  (** Same as {!rindex}, but start
      searching at the character position given as second argument.
      [rindex s c] is equivalent to
      [rindex_from s (length s - 1) c]. *)

val contains : t -> char -> bool
  (** [contains s c] tests if character [c] appears in the substring [s]. *)

val contains_from : t -> int -> char -> bool
  (** [contains_from s start c] tests if character [c] appears in
      the substring of [s] starting from [start] to the end of [s].

      @raise Invalid_argument if [start] is not a valid index of [s]. *)

val rcontains_from : t -> int -> char -> bool
  (** [rcontains_from s stop c] tests if character [c]
      appears in the substring of [s] starting from the beginning
      of [s] to index [stop].
      @raise Invalid_argument if [stop] is not a valid index of [s]. *)

val find : t -> string -> int
  (** [find s x] returns the starting index of the first occurrence of
      string [x] within substring [s].

      {b Note} This implementation is optimized for short substrings.

      @raise Not_found if [x] is not a substring of [s]. *)

val find_from : t -> int -> string -> int
  (** [find_from s ofs x] behaves as [find s x] but starts searching
      at offset [ofs]. [find s x] is equivalent to [find_from s 0 x].*)

val rfind : t -> string -> int
  (** [rfind s x] returns the starting index of the last occurrence
      of string [x] within substring [s].

      {b Note} This implementation is optimized for short strings.

      @raise Not_found if [x] is not a substring of [s]. *)

val rfind_from : t -> int -> string -> int
  (** [rfind_from s ofs x] behaves as [rfind s x] but starts searching
      at offset [ofs]. [rfind s x] is equivalent to
      [rfind_from s (length s - 1) x]. *)

(******************************************************************************)
(** {6 Transformations} *)

val lchop : t -> t
  (** Returns the same string but without the first character.
      does nothing if the string is empty. *)

val rchop : t -> t
  (** Returns the same string but without the last character.
      does nothing if the string is empty. *)

val trim : t -> t
  (** Returns the same string but without the leading and trailing
      whitespaces. *)

val left : t -> int -> t
(**[left r len] returns the string containing the [len] first characters of [r]*)

val right : t -> int -> t
(**[left r len] returns the string containing the [len] last characters of [r]*)

val head : t -> int -> t
(**as {!left}*)

val tail : t -> int -> t
(**[tail r pos] returns the string containing all but the [pos] first characters of [r]*)

val strip : ?chars:string -> t -> t
  (** Returns the string without the chars if they are at the beginning or
      at the end of the string. By default chars are " \t\r\n". *)

val uppercase : t -> t
(** Return a copy of the argument, with all lowercase letters
    translated to uppercase, including accented letters of the ISO
    Latin-1 (8859-1) character set. *)

val lowercase : t -> t
(** Return a copy of the argument, with all uppercase letters
    translated to lowercase, including accented letters of the ISO
    Latin-1 (8859-1) character set. *)

val capitalize : t -> t
(** Return a copy of the argument, with the first character set to uppercase. *)

val uncapitalize : t -> t
(** Return a copy of the argument, with the first character set to lowercase. *)

val fill : t -> int -> int -> char -> unit
  (** [fill s start len c] modifies string [s] in place,
   replacing the characters number [start] to [start + len - 1]
   by [c].
   @raise Invalid_argument if [start] and [len] do not
   designate a valid substring of [s]. *)

val blit : t -> int -> t -> int -> int -> unit
  (** [blit src srcoff dst dstoff len] copies [len] characters
   from string [src], starting at character number [srcoff], to
   string [dst], starting at character number [dstoff]. It works
   correctly even if [src] and [dst] are the same string,
   and the source and destination chunks overlap.

    @raise Invalid_argument if [srcoff] and [len] do not
   designate a valid substring of [src], or if [dstoff] and [len]
   do not designate a valid substring of [dst]. *)

val concat : string -> t list -> t
  (** [concat sep sl] concatenates the list of substrings [sl],
      inserting the separator string [sep] between each. *)

val escaped : t -> t
  (** Return a copy of the argument, with special characters
   represented by escape sequences, following the lexical
   conventions of Objective Caml.  If there is no special
   character in the argument, return the original string itself,
   not a copy. *)

val replace_chars : (char -> t) -> t -> t
  (** [replace_chars f s] returns a string where all chars [c] of [s] have been
      replaced by the string returned by [f c]. *)

val replace : str:t -> sub:t -> by:t -> bool * t
  (** [replace ~str ~sub ~by] returns a tuple constisting of a boolean
      and a string where the first occurrence of the string [sub]
      within [str] has been replaced by the string [by]. The boolean
      is true if a subtitution has taken place. *)

val repeat: t -> int -> t
(** [repeat s n] returns [s ^ s ^ ... ^ s] *)

(******************************************************************************)
(** {6 Splitting} *)

val split : t -> string -> t * t
  (** [split s sep] splits the substring [s] between the first
      occurrence of [sep].
      @raise Not_found if the separator is not found. *)

val rsplit : t -> string -> t * t
  (** [rsplit s sep] splits the substring [s] between the last
      occurrence of [sep].
      @raise Not_found if the separator is not found. *)

val nsplit : t -> string -> t list
  (** [nsplit s sep] splits the substring [s] into a list of substrings
      which are separated by [sep].
      [nsplit "" _] returns the empty list. *)

val split_at : int -> t -> t * t
  (** [split_at (sus, k)] returns the pair (sus1, sus2) of substrings,
      where sus1 contains the first k characters of sus, and sus2
      contains the rest.
      @raise Invalid_argument if [k < 0] or [k > length sus]. *)

val join : string -> t list -> t
  (** Same as {!concat} *)

val slice : ?first:int -> ?last:int -> t -> t
  (** [slice ?first ?last s] returns a "slice" of the string
      which corresponds to the characters [s.[first]],
      [s.[first+1]], ..., [s[last-1]]. Note that the character at
      index [last] is {b not} included! If [first] is omitted it
      defaults to the start of the string, i.e. index 0, and if
      [last] is omitted is defaults to point just past the end of
      [s], i.e. [length s].  Thus, [slice s] is equivalent to
      [copy s].

      Negative indexes are interpreted as counting from the end of
      the string. For example, [slice ~last:-2 s] will return the
      string [s], but without the last two characters.

      This function {b never} raises any exceptions. If the
      indexes are out of bounds they are automatically clipped.
  *)

val splice : t -> int -> int -> t -> t
  (** [splice s off len rep] cuts out the section of [s]
      indicated by [off] and [len] and replaces it by [rep] 

      Negative indexes are interpreted as counting from the end
      of the string. If [off+len] is greater than [length s],
      the end of the string is used, regardless of the value of
      [len].
*)

val explode : t -> char list
  (** [explode s] returns the list of characters in the string [s].
      (Equivalent to {!to_list}.) *)

val implode : char list -> t
  (** [implode cs] returns a string resulting from concatenating
      the characters in the list [cs].
      (Equivalent to {!of_list}.)*)

(******************************************************************************)
(** {6 Splitting} *)

val dropl : (char -> bool) -> t -> t
  (** [dropl p sus] drops the longest prefix (left substring) of sus
      all of whose characters satisfy predicate p.  If all characters
      do, it returns the empty substring (s, i+n, 0) where sus = (s,
      i, n).
  *)

val dropr : (char -> bool) -> t -> t
(**
   [dropr p sus] drops the longest suffix (right substring) of sus all
   of whose characters satisfy predicate p.  If all characters do, it
   returns the empty substring (s, i, 0) where sus = (s, i, n).
*)

val takel : (char -> bool) -> t -> t
(**
   [takel p sus] returns the longest prefix (left substring) of sus
   all of whose characters satisfy predicate p.  That is, if the
   left-most character does not satisfy p, returns the empty (s, i, 0)
   where sus = (s, i, n).
*)

val taker : (char -> bool) -> t -> t
(**
   [taker p sus] returns the longest suffix (right substring) of sus
   all of whose characters satisfy predicate p.  That is, if the
   right-most character satisfies p, returns the empty (s, i+n, 0)
   where sus = (s, i, n).

   Let p be a predicate and xxxxfyyyyfzzzz a string where all
   characters in xxxx and zzzz satisfy p, and f a is character
   not satisfying p.  Then

   sus = xxxxfyyyyfzzzz         sus = xxxxzzzz
   ------------------------------------------------------
   dropl p sus =     fyyyyfzzzz
   dropr p sus = xxxxfyyyyf
   takel p sus = xxxx                         xxxxzzzz
   taker p sus =           zzzz               xxxxzzzz

   It also holds that
   concat[takel p sus, dropl p sus] = string sus
   concat[dropr p sus, taker p sus] = string sus
*)

val span : t -> t -> t
(**
   [span (sus1, sus2)] returns a substring spanning from the start of
   sus1 to the end of sus2, provided this is well-defined: sus1 and
   sus2 must have the same underlying string, and the start of sus1
   must not be to the right of the end of sus2; otherwise raises Span.

   More precisely, if base(sus1) = (s,i,n) and base(sus2) = (s',i',n') 
   and s = s' and i <= i'+n', then base(join(sus1, sus2)) = (s, i, i'+n'-i).
   This may be used to compute `span', `union', and `intersection'.
*)

val translate : (char -> char) -> t -> string
(**
   [translate f sus] applies f to every character of sus, from left to
   right, and returns the concatenation of the results.  Raises Size
   if the sum of their sizes is greater than String.maxSize.
   Equivalent to String.concat(List.map f (explode sus)).
*)

val tokens : (char -> bool) -> t -> t list
(**
   [tokens p sus] returns the list of tokens in sus, from left to right,
   where a token is a non-empty maximal substring of sus not containing
   any delimiter, and a delimiter is a character satisfying p.
*)

val fields : (char -> bool) -> t -> t list
(**
   [fields p sus] returns the list of fields in sus, from left to right,
   where a field is a (possibly empty) maximal substring of sus not
   containing any delimiter, and a delimiter is a character satisfying p.

   Two tokens may be separated by more than one delimiter, whereas two
   fields are separated by exactly one delimiter.  If the only delimiter
   is the character #"|", then
   "abc||def" contains two tokens:   "abc" and "def"
   "abc||def" contains three fields: "abc" and "" and "def"
*)

(******************************************************************************)
(** {6 Comparisons} *)

val compare : t -> t -> int
  (** [compare (sus1, sus2)] performs lexicographic comparison, using
      the standard ordering Char.compare on the characters.  Returns
      LESS, EQUAL, or GREATER, according as sus1 is less than, equal
      to, or greater than sus2.  Equivalent to, but more efficient
      than, String.compare(string sus1, string sus2).  *)

val compare_string : t -> string -> int

val icompare : t -> t -> int
  (** [icompare s1 s2] compare two substrings, case-insensitive. *)

val icompare_string : t -> string -> int
  (** [icompare ss str] compare a substring with a string, case-insensitive. *)

val equals : t -> t -> bool
  (** [equals s1 s2] compares two substrings for equality. *)

val equals_string : t -> string -> bool
  (** [equals ss str] compares a substring and string for equality. *)

val starts_with : t -> t -> bool
  (** [starts_with s x] returns [true] if [s] is starting with [x],
      [false] otherwise. *)

val starts_with_string : t -> string -> bool
  (** [starts_with s x] returns [true] if [s] is starting with [x],
      [false] otherwise. *)

val ends_with : t -> t -> bool
  (** [ends_with s x] returns [true] if the substring [s] is ending with
      [x], [false] otherwise. *)

val ends_with_string : t -> string -> bool
  (** [ends_with s x] returns [true] if the substring [s] is ending with
      [x], [false] otherwise. *)

val contains_substring : t -> t -> bool
  (** [contains_string ss sub] returns true if [sub] is a substring of [ss] or
      false otherwise. *)

val contains_string : t -> string -> bool
  (** [contains_string str sub] returns true if [sub] is a substring of [str] or
      false otherwise. *)

val hash : t -> int
  (** [hash ss] returns a hash code that is suitable for use with Hashtbl.t *)

(******************************************************************************)

val split_on_char : char -> t -> t list
  (** [split_on_char c ss] returns substrings of input [ss] as divided
  by [c] *)

val split_on_pipe : t -> t list
val split_on_dot : t -> t list
val split_on_comma : t -> t list
val split_on_slash : t -> t list

(******************************************************************************)
(**/**)

val size : t -> int
  (** [size (s, i, n)] returns the size of the substring, that is, n.
      @deprecated use {!length} instead. *)

val base : t -> string * int * int
  (** [base sus] is the concrete triple (s, i, n), where sus = (s, i, n).
      @deprecated use {!string}, {!offset} and {!length} instead. *)

val all : string -> t
  (** [all s] is the substring (s, 0, size s).
      @deprecated use {!of_string} instead. *)

val extract : string -> int -> int option -> t
  (** [extract(s, i, NONE)] creates the substring (s, i, size s-i)
      consisting of the tail of s starting at i.  Raises
      Invalid_argument if i<0 or i > size s.

      [extract(s, i, SOME n)] creates the substring (s, i, n), consisting
      of the substring of s with length n starting at i.  Raises
      Invalid_argument if i<0 or n<0 or i+n > size s.

      @deprecated use XXX
  *)

val getc : t -> (char * t) option
  (** [getc sus] returns SOME(c, rst) where c is the first character and
      rst the remainder of sus, if sus is non-empty; otherwise returns
      NONE.
      @deprecated *)

val first : t -> char option
  (** [first sus] returns SOME c where c is the first character in
      sus, if sus is non-empty; otherwise returns NONE.
      @deprecated use {!head} instead *)

val triml : int -> t -> t
  (** [triml k sus] returns sus less its leftmost k characters; or the
      empty string at the end of sus if it has less than k characters.
       if k < 0, even in the partial application
      triml(k).
      @deprecated use {!lchop} instead
  *)

val trimr : int -> t -> t
  (** [trimr k sus] returns sus less its rightmost k characters; or the
      empty string at the beginning of sus if it has less than k
      characters.  Raises Invalid_argument if k < 0, even in the partial
      application triml(k).
      @deprecated use {!rchop} instead
  *)

val splitl : (char -> bool) -> t -> t * t
(**
   [splitl p sus] splits sus into a pair (sus1, sus2) of substrings
   where sus1 is the longest prefix (left substring) all of whose
   characters satisfy p, and sus2 is the rest.  That is, sus2 begins
   with the leftmost character not satisfying p.  Disregarding
   sideeffects, we have: 
   splitl p sus = (takel p sus, dropl p sus).
*)

val splitr : (char -> bool) -> t -> t * t
(**
   [splitr p sus] splits sus into a pair (sus1, sus2) of substrings
   where sus2 is the longest suffix (right substring) all of whose
   characters satisfy p, and sus1 is the rest.  That is, sus1 ends
   with the rightmost character not satisfying p.  Disregarding
   sideeffects, we have:
   splitr p sus = (dropr p sus, taker p sus)
*)

(* NOT IMPLEMENTED
   [position s (s',i,n)] splits the substring into a pair (pref, suff)
   of substrings, where suff is the longest suffix of (s', i, n) which
   has s as a prefix.  More precisely, let m = size s.  If there is a
   least index k in i..i+n-m for which s = s'[k..k+m-1], 
   then the result is       pref = (s', i, k-i) and suff = (s', k, n-(k-i)); 
   otherwise the result is  pref = (s', i, n)   and suff = (s', i+n, 0).
*)

(* XXX
val slice : t -> int -> int option -> t
  (** [slice (sus, i', NONE)] returns the substring (s, i+i', n-i'),
      where sus = (s, i, n).  Raises Invalid_argument if i' < 0 or i' > n.  

      [slice (sus, i', SOME n')] returns the substring (s, i+i', n'),
      where sus = (s, i, n).  Raises Invalid_argument if i' < 0 or n' < 0 or
      i'+n' >= n.
      @deprecated signature changed
  *)
*)

(* NOT IMPLEMENTED
  [collate cmp (sus1, sus2)] performs lexicographic comparison, using the
    given ordering cmp on characters.  Equivalent to, but more efficient
      than, String.collate cmp (string sus1, string sus2).
*)

(* XXX
val concat : t list -> string
  (** [concat suss] returns a string consisting of the concatenation of
      the substrings.  Equivalent to String.concat (List.map string
      suss).
      @deprecated signature changed
  *)
*)

val is_prefix : string -> t -> bool
  (** [isPrefix s1 s2] is true if s1 is a prefix of s2. That is, if
      there exists a string t such that string s1 ^ t = string s2.
      @deprecated use {!starts_with} instead
  *)

(******************************************************************************)
