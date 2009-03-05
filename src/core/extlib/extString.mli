(*
 * ExtString - Additional functions for string manipulations.
 * Copyright (C) 2003 Nicolas Cannasse
 * Copyright (C) 1996 Xavier Leroy, INRIA Rocquencourt
 * Copyright (C) 2008 Edgar Friendly
 * Copyright (C) 2009 David Teller, LIFO, Universite d'Orleans
 * Copyright (C) 2009 Warren Harris, Metaweb Technologies
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

(** String operations.

    @author Xavier Leroy (base library)
    @author Nicolas Cannasse
    @author David Teller
    @author Edgar Friendly
    @author Warren Harris

    @documents String*)
module String:
sig

type t = string
(** The type of strings. *)

external length : string -> int = "%string_length"
(** Return the length (number of characters) of the given string. *)

val is_empty : string -> bool
(** [is_empty s] returns [true] if [s] is the empty string, [false]
    otherwise.

    Usually a tad faster than comparing [s] with [""]. *)

(******************************************************************************)
(** {6 Constructors}*)

external create : int -> string = "caml_create_string"
(** [String.create n] returns a fresh string of length [n].
   The string initially contains arbitrary characters.
   Raise [Invalid_argument] if [n < 0] or [n > Sys.max_string_length].
*)

val make : int -> char -> string
(** [String.make n c] returns a fresh string of length [n],
   filled with the character [c].
   Raise [Invalid_argument] if [n < 0] or [n > ]{!Sys.max_string_length}.*)

val init : int -> (int -> char) -> string
  (** [init l f] returns the string of length [l] with the chars
      f 0 , f 1 , f 2 ... f (l-1). *)

val copy : string -> string
(** Return a copy of the given string. *)

val sub : string -> int -> int -> string
(** [String.sub s start len] returns a fresh string of length [len],
   containing the characters number [start] to [start + len - 1]
   of string [s].
   @raise Invalid_argument if [start] and [len] do not
   designate a valid substring of [s]; that is, if [start < 0],
   or [len < 0], or [start + len > ]{!String.length}[ s]. *)

(******************************************************************************)
(** {6 Accessors} *)

external get : string -> int -> char = "%string_safe_get"
(** [String.get s n] returns character number [n] in string [s].
   The first character is character number 0.
   The last character is character number [String.length s - 1].
   You can also write [s.[n]] instead of [String.get s n].

   Raise [Invalid_argument "index out of bounds"]
   if [n] is outside the range 0 to [(String.length s - 1)]. *)


external set : string -> int -> char -> unit = "%string_safe_set"
(** [String.set s n c] modifies string [s] in place,
   replacing the character number [n] by [c].
   You can also write [s.[n] <- c] instead of [String.set s n c].
   Raise [Invalid_argument "index out of bounds"]
   if [n] is outside the range 0 to [(String.length s - 1)]. *)

(******************************************************************************)
(** {6 Conversions}*)

val enum : string -> char Enum.t
  (** Returns an enumeration of the characters of a string. *)

val of_enum : char Enum.t -> string
  (** Creates a string from a character enumeration. *)

val backwards : string -> char Enum.t
  (** Returns an enumeration of the characters of a string, from last to first. *)

val of_backwards : char Enum.t -> string
  (** Build a string from an enumeration, starting with last character, ending with first. *)

val of_list : char list -> string
   (** Converts a list of characters to a string.*) 

val to_list : string -> char list
  (** Converts a string to the list of its characters.*)

val of_int : int -> string
  (** Returns the string representation of an int. *)

val to_int : string -> int
  (** Returns the integer represented by the given string or
      @raise Failure "int_of_string" if the string does not represent an integer.*)

val of_float : float -> string
  (** Returns the string representation of an float. *)

val to_float : string -> float
  (** Returns the float represented by the given string or
      @raise Failure "float_of_string" if the string does not represent a float. *)

val of_char : char -> string
  (** Returns a string containing one given character. *)

(******************************************************************************)
(** {6 Collection Operations} *)

val map : (char -> char) -> string -> string
  (** [map f s] returns a string where all characters [c] in [s] have been
      replaced by [f c]. **)

val fold_left : ('a -> char -> 'a) -> 'a -> string -> 'a
  (** [fold_left f a s] is
      [f (... (f (f a s.[0]) s.[1]) ...) s.[n-1]] *)

val fold_right : (char -> 'a -> 'a) -> string -> 'a -> 'a
  (** [fold_right f s b] is
      [f s.[0] (f s.[1] (... (f s.[n-1] b) ...))] *)

val filter : (char -> bool) -> string -> string
  (** [filter f s] returns a copy of string [s] in which only
      characters [c] such that [f c = true] remain.*)

val filter_map : (char -> char option) -> string -> string
  (** [filter_map f s] calls [(f a0) (f a1).... (f an)] where [a0..an] are
      the characters of [s]. It returns the string of characters [ci] such as
      [f ai = Some ci] (when [f] returns [None], the corresponding element of
      [s] is discarded). *)

val iter : (char -> unit) -> string -> unit
(** [String.iter f s] applies function [f] in turn to all
   the characters of [s].  It is equivalent to
   [f s.[0]; f s.[1]; ...; f s.[String.length s - 1]; ()]. *)

val for_all : (char -> bool) -> string -> bool
  (** [for_all p str] returns [true] if all characters [c] of [str] satisfy
      the predicate [p c]. *)

val exists : (char -> bool) -> string -> bool
  (** [exists p str] returns [true] if any character [c] of [str] satisfies
      the predicate [p c]. *)

(******************************************************************************)
(** {6 Generalized Finding} *)

type match_fun = t -> int -> int -> int
  (** The type of match functions that are used to determine whether a
      string contains a sub-sequence of characters. A match function
      [f str srchpos srchlen] is called for successive positions
      [srchpos] within [str] and should return the count of characters
      that it matches up to the maximum [srchlen]. (Note that
      dependding on the match function, matching may proceed before or
      after the given [srchpos].) Any result > 0 will indicate a
      successful match, and will cause that substring to be found. See
      {!find_matching}. *)

val find_matching : match_fun -> t -> int -> int -> int * int
  (** [find_matching m str ofs len] returns a pair containing the
      offset and length of the first location in [str] between [ofs]
      and [len] (in the left-to-right direction) where the match
      function [m] returns a matched length > 0. Predicates can be
      used to specify either sequences of a given character class, a
      fixed substring, or even a pattern to be matched.

      @raise Not_found if the string does not match the predicate in
      the specified range. *)

val rfind_matching : match_fun -> t -> int -> int -> int * int
  (** [rfind_matching m str ofs len] returns a pair containing the
      offset and length of the first location in [str] between [ofs]
      and [len] (in the right-to-left direction) where the match
      function [m] returns a matched length > 0. Predicates can be
      used to specify either sequences of a given character class, a
      fixed substring, or even a pattern to be matched.

      @raise Not_found if the string does not match the predicate in
      the specified range. *)

val fold_left_matching : match_fun -> ('a -> t -> 'a) -> 'a -> t -> 'a
  (** [fold_left_matching m f acc str] folds function [f acc sub] over
      substrings [sub] of [str] from left-to-right as delimited by the
      match function [m]. *)

val fold_right_matching : match_fun -> (t -> 'a -> 'a) -> t -> 'a -> 'a
  (** [fold_right_matching m f str acc] folds function [f sub acc]
      over substrings [sub] of [str] from right-to-left as delimited
      by the match function [m]. *)

val map_matching : match_fun -> (string -> 'a) -> string -> 'a list
val rmap_matching : match_fun -> (string -> 'a) -> string -> 'a list
val iter_matching : match_fun -> (string -> unit) -> string -> unit
val riter_matching : match_fun -> (string -> unit) -> string -> unit

val take_matching : match_fun -> string -> int -> int -> string
val drop_matching : match_fun -> string -> int -> int -> string

val split_matching : match_fun -> string -> int -> int -> string * string
val rsplit_matching : match_fun -> string -> int -> int -> string * string
val nsplit_matching : match_fun -> string -> int -> int -> string list

val subst_matching : match_fun -> string -> string -> int -> int -> string
val rsubst_matching : match_fun -> string -> string -> int -> int -> string
val nsubst_matching : match_fun -> string -> string -> int -> int -> string

(******************************************************************************)
(** {7 Match Functions} *)

val match_string : string -> match_fun
  (** [match_string str] returns a match function that matches the
      sequence of characters specified by the string [str]. *)

val match_substring : string -> int -> int -> match_fun
  (** [match_substring str ofs len] returns a match function that
      matches the sequence of characters specified by the substring of
      [str] at offset [ofs] with length [len]. *)

val match_chars : string -> match_fun
  (** [match_chars chars] returns a match function that matches any
      characters contained within [chars] at the beginning of a string. *)

val rmatch_chars : string -> match_fun
  (** [rmatch_chars chars] returns a match function that matches any
      characters contained within [chars] at the end of a string. *)

val match_whitespace : match_fun
  (** [match_whitespace] returns a match function that matches any
      whitespace characters ([ \n\t\r]) at the beginning of a string. *)

val rmatch_whitespace : match_fun
  (** [rmatch_whitespace] returns a match function that matches any
      whitespace characters ([ \n\t\r]) at the end of a string. *)

val match_char : char -> match_fun
  (** [match_char c] returns a match function that matches the single
      character [c] at the beginning of a string. *)

(******************************************************************************)
(** {6 Finding} *)

val index : string -> char -> int
(** [String.index s c] returns the position of the leftmost
   occurrence of character [c] in string [s].
   Raise [Not_found] if [c] does not occur in [s]. *)

val rindex : string -> char -> int
(** [String.rindex s c] returns the position of the rightmost
   occurrence of character [c] in string [s].
   Raise [Not_found] if [c] does not occur in [s]. *)

val index_from : string -> int -> char -> int
(** Same as {!String.index}, but start
   searching at the character position given as second argument.
   [String.index s c] is equivalent to [String.index_from s 0 c].*)

val rindex_from : string -> int -> char -> int
(** Same as {!String.rindex}, but start
   searching at the character position given as second argument.
   [String.rindex s c] is equivalent to
   [String.rindex_from s (String.length s - 1) c]. *)

val contains : string -> char -> bool
(** [String.contains s c] tests if character [c]
   appears in the string [s]. *)

val contains_from : string -> int -> char -> bool
  (** [String.contains_from s start c] tests if character [c] appears in
      the substring of [s] starting from [start] to the end of [s].

      @raise Invalid_argument if [start] is not a valid index of [s]. *)

val rcontains_from : string -> int -> char -> bool
(** [String.rcontains_from s stop c] tests if character [c]
   appears in the substring of [s] starting from the beginning
   of [s] to index [stop].
   @raise Invalid_argument if [stop] is not a valid index of [s]. *)


val find : string -> string -> int
  (** [find s x] returns the starting index of the first occurrence of
      string [x] within string [s].

      {b Note} This implementation is optimized for short strings.

      @raise Not_found if [x] is not a substring of [s]. *)

val find_from: string -> int -> string -> int
  (** [find_from s ofs x] behaves as [find s x] but starts searching
      at offset [ofs]. [find s x] is equivalent to [find_from s 0 x].*)

val rfind : string -> string -> int
  (** [rfind s x] returns the starting index of the last occurrence
      of string [x] within string [s].

      {b Note} This implementation is optimized for short strings.

      @raise Not_found if [x] is not a substring of [s]. *)

val rfind_from: string -> int -> string -> int
  (** [rfind_from s ofs x] behaves as [rfind s x] but starts searching
      at offset [ofs]. [rfind s x] is equivalent to [rfind_from s (String.length s - 1) x].*)

(* Unfortunately, the name 'exists' is inconsistent with the other
   collections (if we consider a string to be a collection of chars):
val exists : string -> string -> bool
  (** [exists str sub] returns true if [sub] is a substring of [str] or
      false otherwise. *)
*)

(******************************************************************************)
(** {6 Transformations} *)

val lchop : string -> string
  (** Returns the same string but without the first character.
      does nothing if the string is empty. *)

val rchop : string -> string
  (** Returns the same string but without the last character.
      does nothing if the string is empty. *)

val trim : string -> string
  (** Returns the same string but without the leading and trailing
      whitespaces. *)

val left : string -> int -> string
  (** [left r n] returns the string containing the [n] leftmost characters
      of [r]. *)

val right : string -> int -> string
  (** [right r n] returns the string containing the [n] rightmost characters
      of [r]. *)

val head : string -> int -> string
  (** [head r pos] returns the string containing the characters up to
      but not including position [pos]. (Note: equivalent to [left r pos]
      and [sub r 0 pos].) *)

val tail : string -> int -> string
  (** [tail r pos] returns the string containing the characters starting from
      position [pos] to the end of the string. (Note: equivalent to
      [right r (length r - pos)].) *)

val strip : ?chars:string -> string -> string
  (** Returns the string without the chars if they are at the beginning or
      at the end of the string. By default chars are " \t\r\n". *)

val uppercase : string -> string
(** Return a copy of the argument, with all lowercase letters
    translated to uppercase, including accented letters of the ISO
    Latin-1 (8859-1) character set. *)

val lowercase : string -> string
(** Return a copy of the argument, with all uppercase letters
    translated to lowercase, including accented letters of the ISO
    Latin-1 (8859-1) character set. *)

val capitalize : string -> string
(** Return a copy of the argument, with the first character set to uppercase. *)

val uncapitalize : string -> string
(** Return a copy of the argument, with the first character set to lowercase. *)

val fill : string -> int -> int -> char -> unit
(** [String.fill s start len c] modifies string [s] in place,
   replacing the characters number [start] to [start + len - 1]
   by [c].
   @raise Invalid_argument if [start] and [len] do not
   designate a valid substring of [s]. *)

val blit : string -> int -> string -> int -> int -> unit
(** [String.blit src srcoff dst dstoff len] copies [len] characters
   from string [src], starting at character number [srcoff], to
   string [dst], starting at character number [dstoff]. It works
   correctly even if [src] and [dst] are the same string,
   and the source and destination chunks overlap.
   
    @raise Invalid_argument if [srcoff] and [len] do not
   designate a valid substring of [src], or if [dstoff] and [len]
   do not designate a valid substring of [dst]. *)

val concat : string -> string list -> string
(** [String.concat sep sl] concatenates the list of strings [sl],
   inserting the separator string [sep] between each. *)

val escaped : string -> string
(** Return a copy of the argument, with special characters
   represented by escape sequences, following the lexical
   conventions of Objective Caml.  If there is no special
   character in the argument, return the original string itself,
   not a copy. *)

val quote : string -> string
  (** Add quotes around a string and escape any quote appearing in that string.
      This function is used typically when you need to generate source code
      from a string.

      [quote "foo"] returns ["\"foo\""]
      [quote "\"foo\""] returns ["\\\"foo\\\""]
      etc. *)

val replace_chars : (char -> string) -> string -> string
  (** [replace_chars f s] returns a string where all chars [c] of [s] have been
      replaced by the string returned by [f c]. *)

val replace : str:string -> sub:string -> by:string -> bool * string
  (** [replace ~str ~sub ~by] returns a tuple constisting of a boolean
      and a string where the first occurrence of the string [sub]
      within [str] has been replaced by the string [by]. The boolean
      is true if a subtitution has taken place. *)

val repeat: string -> int -> string
(** [repeat s n] returns [s ^ s ^ ... ^ s] *)

(******************************************************************************)
(** {6 Splitting} *)

val split : string -> string -> string * string
  (** [split s sep] splits the string [s] between the first
      occurrence of [sep].
      @raise Not_found if the separator is not found. *)

val rsplit : string -> string -> string * string
  (** [rsplit s sep] splits the string [s] between the last
      occurrence of [sep].
      @raise Not_found if the separator is not found. *)

val nsplit : string -> string -> string list
  (** [nsplit s sep] splits the string [s] into a list of strings
      which are separated by [sep].
      [nsplit "" _] returns the empty list. *)

val split_at : int -> string -> string * string
  (** [split_at (s, k)] returns the pair (s1, s2) of strings,
      where [s1] contains the first k characters of [s], and [s2]
      contains the rest.
      @raise Invalid_argument if [k < 0] or [k > length s]. *)

val join : string -> string list -> string
  (** Same as {!concat} *)

val slice : ?first:int -> ?last:int -> string -> string
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

val splice: string -> int -> int -> string -> string
  (** [String.splice s off len rep] cuts out the section of [s]
      indicated by [off] and [len] and replaces it by [rep] 

      Negative indexes are interpreted as counting from the end
      of the string. If [off+len] is greater than [length s],
      the end of the string is used, regardless of the value of
      [len].
*)

val explode : string -> char list
  (** [explode s] returns the list of characters in the string [s]. *)

val implode : char list -> string
  (** [implode cs] returns a string resulting from concatenating
      the characters in the list [cs]. *)

(******************************************************************************)
(** {6 Comparisons}*)

val compare: t -> t -> int
  (** The comparison function for strings, with the same specification as
      {!Pervasives.compare}.  Along with the type [t], this function [compare]
      allows the module [String] to be passed as argument to the functors
      {!Set.Make} and {!Map.Make}. *)

val icompare: t -> t -> int
  (** Compare two strings, case-insensitive. *)

val starts_with : string -> string -> bool
  (** [starts_with s x] returns [true] if [s] is starting with [x], [false] otherwise. *)

val ends_with : string -> string -> bool
  (** [ends_with s x] returns [true] if the string [s] is ending with [x], [false] otherwise. *)

val contains_string : string -> string -> bool
  (** [contains_string str sub] returns true if [sub] is a substring of [str] or
      false otherwise. *)

module IString : Interfaces.OrderedType with type t = t
(** uses icompare as ordering function *)

val numeric_compare: t -> t -> int
  (** Compare two strings, sorting "abc32def" before "abc210abc" *)

module NumString : Interfaces.OrderedType with type t = t
(** uses numeric_compare as its ordering function *)

(******************************************************************************)
(** {6 Boilerplate code}*)
(** {7 S-Expressions}*)

val t_of_sexp : Sexplib.Sexp.t -> t
val sexp_of_t : t -> Sexplib.Sexp.t

(** {7 Printing}*)

val print: 'a InnerIO.output -> string -> unit
(**Print a string.*)

val println: 'a InnerIO.output -> string -> unit
(**Print a string, end the line.*)

val print_quoted: 'a InnerIO.output -> string -> unit
(**Print a string, with quotes.

   [print_quoted stdout "foo"] prints ["foo"] (with the quotes)

   [print_quoted stdout "\"bar\""] prints ["\"bar\""] (with the quotes)
*)

(******************************************************************************)
(**/**)

(** Undocumented operations *)

external unsafe_get : string -> int -> char = "%string_unsafe_get"
external unsafe_set : string -> int -> char -> unit = "%string_unsafe_set"
external unsafe_blit :
  string -> int -> string -> int -> int -> unit = "caml_blit_string" "noalloc"
external unsafe_fill :
  string -> int -> int -> char -> unit = "caml_fill_string" "noalloc"
val unsafe_match_substring : string -> int -> int -> string -> int -> int -> int
val unsafe_count_after : (char -> bool) -> string -> int -> int -> int
val unsafe_count_before : (char -> bool) -> string -> int -> int -> int

(**/**)
(******************************************************************************)

(** Capabilities for strings.
    
    This modules provides the same set of features as {!String}, but
    with the added twist that strings can be made read-only or write-only.
    Read-only strings may then be safely shared and distributed.

    There is no loss of performance involved. *)
module Cap:
sig

type 'a t 
(** The type of capability strings.

    If ['a] contains [[`Read]], the contents of the string may be read.
    If ['a] contains [[`Write]], the contents of the string may be written.

    Other (user-defined) capabilities may be added without loss of
    performance or features. For instance, a string could be labelled
    [[`Read | `UTF8]] to state that it contains UTF-8 encoded data and
    may be used only for reading.  Conversely, a string labelled with
    [[]] (i.e. nothing) can neither be read nor written. It can only
    be compared for textual equality using OCaml's built-in [compare]
    or for physical equality using OCaml's built-in [==].
*)

external length : _ t  -> int = "%string_length"
    (** Return the length (number of characters) of the given string. *)

val is_empty : _ t -> bool 
(** Determine if a string is empty. *)

(******************************************************************************)
(** {6 Constructors}*)

external of_string : string -> _ t                = "%identity"
    (**Adopt a regular string.*)

external to_string : [`Read | `Write] t -> string = "%identity"
    (** Return a capability string as a regular string.*)

external read_only : [> `Read] t -> [`Read] t     = "%identity"
    (** Drop capabilities to read only.*)

external write_only: [> `Write] t -> [`Write] t   = "%identity"
    (** Drop capabilities to write only.*)

external create : int -> _ t = "caml_create_string"
(** [String.create n] returns a fresh string of length [n].
   The string initially contains arbitrary characters.
   Raise [Invalid_argument] if [n < 0] or [n > Sys.max_string_length].*)

val make : int -> char -> _ t
(** [String.make n c] returns a fresh string of length [n],
   filled with the character [c].
   Raise [Invalid_argument] if [n < 0] or [n > ]{!Sys.max_string_length}.*)

val init : int -> (int -> char) -> _ t
  (** [init l f] returns the string of length [l] with the chars
      f 0 , f 1 , f 2 ... f (l-1). *)

val copy : [> `Read] t -> _ t
(** Return a copy of the given string. *)

val sub : [> `Read] t -> int -> int -> _ t
(** [String.sub s start len] returns a fresh string of length [len],
   containing the characters number [start] to [start + len - 1]
   of string [s].
   Raise [Invalid_argument] if [start] and [len] do not
   designate a valid substring of [s]; that is, if [start < 0],
   or [len < 0], or [start + len > ]{!String.length}[ s]. *)

(******************************************************************************)
(** {6 Accessors} *)

external get : [> `Read] t -> int -> char = "%string_safe_get"
(** [String.get s n] returns character number [n] in string [s].
   The first character is character number 0.
   The last character is character number [String.length s - 1].
   You can also write [s.[n]] instead of [String.get s n].

   Raise [Invalid_argument "index out of bounds"]
   if [n] is outside the range 0 to [(String.length s - 1)]. *)


external set : [> `Write] t -> int -> char -> unit = "%string_safe_set"
(** [String.set s n c] modifies string [s] in place,
   replacing the character number [n] by [c].
   You can also write [s.[n] <- c] instead of [String.set s n c].
   Raise [Invalid_argument "index out of bounds"]
   if [n] is outside the range 0 to [(String.length s - 1)]. *)

(******************************************************************************)
(** {6 Conversions}*)

val enum : [> `Read] t -> char Enum.t
  (** Returns an enumeration of the characters of a string.*)

val of_enum : char Enum.t -> _ t
  (** Creates a string from a character enumeration. *)

val backwards : [> `Read] t -> char Enum.t
  (** Returns an enumeration of the characters of a string, from last to first. *)
  
val of_backwards : char Enum.t -> _ t
  (** Build a string from an enumeration, starting with last character, ending with first. *)

val of_list : char list -> _ t
  (** Converts a list of characters to a string.*) 

val to_list : [> `Read] t -> char list
  (** Converts a string to the list of its characters.*)

val of_int : int -> _ t
  (** Returns the string representation of an int. *)

val to_int : [> `Read] t -> int
  (** Returns the integer represented by the given string or
      @raise Failure "int_of_string" if the string does not represent an integer.*)

val of_float : float -> _ t
  (** Returns the string representation of an float. *)

val to_float : [> `Read] t -> float
  (** Returns the float represented by the given string or
      @raise Failure "float_of_string" if the string does not represent a float. *)

val of_char : char -> _ t
  (** Returns a string containing one given character. *)

(******************************************************************************)
(** {6 String Traversals} *)

val map : (char -> char) -> [>`Read] t -> _ t
  (** [map f s] returns a string where all characters [c] in [s] have been
      replaced by [f c]. **)
  
val fold_left : ('a -> char -> 'a) -> 'a -> [> `Read] t -> 'a
  (** [fold_left f a s] is
      [f (... (f (f a s.[0]) s.[1]) ...) s.[n-1]] *)

val fold_right : (char -> 'a -> 'a) -> [> `Read] t -> 'a -> 'a
  (** [fold_right f s b] is
      [f s.[0] (f s.[1] (... (f s.[n-1] b) ...))] *)

val filter : (char -> bool) -> [> `Read] t -> _ t
  (** [filter f s] returns a copy of string [s] in which only
      characters [c] such that [f c = true] remain.*)

val filter_map : (char -> char option) -> [> `Read] t -> _ t
  (** [filter_map f s] calls [(f a0) (f a1).... (f an)] where [a0..an] are
      the characters of [s]. It returns the string of characters [ci] such as
      [f ai = Some ci] (when [f] returns [None], the corresponding element of
      [s] is discarded). *)

val iter : (char -> unit) -> [> `Read] t -> unit
(** [String.iter f s] applies function [f] in turn to all
   the characters of [s].  It is equivalent to
   [f s.[0]; f s.[1]; ...; f s.[String.length s - 1]; ()]. *)

val for_all : (char -> bool) -> [> `Read] t -> bool
  (** [for_all p str] returns [true] if all characters [c] of [str] satisfy
      the predicate [p c]. *)

val exists : (char -> bool) -> [> `Read] t -> bool
  (** [exists p str] returns [true] if any character [c] of [str] satisfies
      the predicate [p c]. *)

(******************************************************************************)
(** {6 Generalized Finding} *)

val find_matching : match_fun -> [> `Read] t -> int -> int -> int * int
  (** [find_matching m str ofs len] returns a pair containing the
      offset and length of the first location in [str] between [ofs]
      and [len] (in the left-to-right direction) where the match
      function [m] returns a matched length > 0. Predicates can be
      used to specify either sequences of a given character class, a
      fixed substring, or even a pattern to be matched.

      @raise Not_found if the string does not match the predicate in
      the specified range. *)

val rfind_matching : match_fun -> [> `Read] t -> int -> int -> int * int
  (** [rfind_matching m str ofs len] returns a pair containing the
      offset and length of the first location in [str] between [ofs]
      and [len] (in the right-to-left direction) where the match
      function [m] returns a matched length > 0. Predicates can be
      used to specify either sequences of a given character class, a
      fixed substring, or even a pattern to be matched.

      @raise Not_found if the string does not match the predicate in
      the specified range. *)

val fold_left_matching : match_fun -> ('a -> [> `Read] t -> 'a) ->
  'a -> [> `Read] t -> 'a
  (** [fold_left_matching m f acc str] folds function [f acc sub] over
      substrings [sub] of [str] from left-to-right as delimited by the
      match function [m]. *)

val fold_right_matching : match_fun -> ([> `Read] t -> 'a -> 'a) ->
  [> `Read] t -> 'a -> 'a
  (** [fold_right_matching m f str acc] folds function [f sub acc]
      over substrings [sub] of [str] from right-to-left as delimited
      by the match function [m]. *)

val take_matching : match_fun -> [> `Read] t -> int -> int -> _ t
val drop_matching : match_fun -> [> `Read] t -> int -> int -> _ t

val split_matching : match_fun -> [> `Read] t -> int -> int -> _ t * _ t
val rsplit_matching : match_fun -> [> `Read] t -> int -> int -> _ t * _ t
val nsplit_matching : match_fun -> [> `Read] t -> int -> int -> _ t list

val subst_matching : match_fun -> [> `Read] t -> [> `Read] t -> int -> int -> _ t
val rsubst_matching : match_fun -> [> `Read] t -> [> `Read] t -> int -> int -> _ t
val nsubst_matching : match_fun -> [> `Read] t -> [> `Read] t -> int -> int -> _ t

(******************************************************************************)
(** {7 Match Functions} *)

val match_string : [> `Read] t -> match_fun
  (** [match_string str] returns a match function that matches the
      sequence of characters specified by the string [str]. *)

val match_substring : [> `Read] t -> int -> int -> match_fun
  (** [match_substring str ofs len] returns a match function that
      matches the sequence of characters specified by the substring of
      [str] at offset [ofs] with length [len]. *)

val match_chars : [> `Read] t -> match_fun
  (** [match_chars chars] returns a match function that matches any
      characters contained within [chars] at the beginning of a string. *)

val rmatch_chars : [> `Read] t -> match_fun
  (** [rmatch_chars chars] returns a match function that matches any
      characters contained within [chars] at the end of a string. *)

val match_whitespace : match_fun
  (** [match_whitespace] returns a match function that matches any
      whitespace characters ([ \n\t\r]) at the beginning of a string. *)

val rmatch_whitespace : match_fun
  (** [rmatch_whitespace] returns a match function that matches any
      whitespace characters ([ \n\t\r]) at the end of a string. *)

val match_char : char -> match_fun
  (** [match_char c] returns a match function that matches the single
      character [c] at the beginning of a string. *)

(******************************************************************************)
(** {6 Finding}*)

val index : [>`Read] t -> char -> int
(** [String.index s c] returns the position of the leftmost
   occurrence of character [c] in string [s].
   Raise [Not_found] if [c] does not occur in [s]. *)

val rindex : [> `Read] t -> char -> int
(** [String.rindex s c] returns the position of the rightmost
   occurrence of character [c] in string [s].
   Raise [Not_found] if [c] does not occur in [s]. *)

val index_from : [> `Read] t -> int -> char -> int
(** Same as {!String.index}, but start
   searching at the character position given as second argument.
   [String.index s c] is equivalent to [String.index_from s 0 c].*)

val rindex_from : [> `Read] t -> int -> char -> int
(** Same as {!String.rindex}, but start
   searching at the character position given as second argument.
   [String.rindex s c] is equivalent to
   [String.rindex_from s (String.length s - 1) c]. *)

val contains : [> `Read] t -> char -> bool
(** [String.contains s c] tests if character [c]
   appears in the string [s]. *)

val contains_from : [> `Read] t -> int -> char -> bool
(** [String.contains_from s start c] tests if character [c]
   appears in the substring of [s] starting from [start] to the end
   of [s].
   Raise [Invalid_argument] if [start] is not a valid index of [s]. *)

val rcontains_from : [> `Read] t -> int -> char -> bool
(** [String.rcontains_from s stop c] tests if character [c]
   appears in the substring of [s] starting from the beginning
   of [s] to index [stop].
   Raise [Invalid_argument] if [stop] is not a valid index of [s]. *)


val find : [> `Read] t -> [> `Read] t -> int
  (** [find s x] returns the starting index of the string [x]
      within the string [s] or raises [Not_found] if [x]
      is not a substring of [s]. *)

val find_from: [> `Read] t -> int -> [> `Read] t -> int
  (** [find_from s ofs x] behaves as [find s x] but starts searching
      at offset [ofs]. [find s x] is equivalent to [find_from s 0 x].*)

val rfind : [> `Read] t -> [> `Read] t -> int
  (** [rfind s x] returns the starting index of the last occurrence
      of string [x] within string [s].

      {b Note} This implementation is optimized for short strings.

      @raise Not_found if [x] is not a substring of [s]. *)

val rfind_from: [> `Read] t -> int -> [> `Read] t -> int
  (** [rfind_from s ofs x] behaves as [rfind s x] but starts searching
      at offset [ofs]. [rfind s x] is equivalent to [rfind_from s (String.length s - 1) x].*)

(*
val exists : [> `Read] t -> [> `Read] t -> bool
  (** [exists str sub] returns true if [sub] is a substring of [str] or
      false otherwise. *)
*)

(******************************************************************************)
(** {6 Transformations}*)
  
val lchop : [> `Read] t -> _ t
  (** Returns the same string but without the first character.
      does nothing if the string is empty. *)

val rchop : [> `Read] t -> _ t
  (** Returns the same string but without the last character.
      does nothing if the string is empty. *)

val trim : [> `Read] t -> _ t
  (** Returns the same string but without the leading and trailing
      whitespaces. *)

val left : [> `Read] t -> int -> _ t
(**[left r len] returns the string containing the [len] first characters of [r]*)

val right : [> `Read] t -> int -> _ t
(**[left r len] returns the string containing the [len] last characters of [r]*)

val head : [> `Read] t -> int -> _ t
(**as {!left}*)

val tail : [> `Read] t -> int -> _ t
(**[tail r pos] returns the string containing all but the [pos] first characters of [r]*)

val strip : ?chars:[> `Read] t -> [> `Read] t -> _ t
  (** Returns the string without the chars if they are at the beginning or
      at the end of the string. By default chars are " \t\r\n". *)

val uppercase : [> `Read] t -> _ t
(** Return a copy of the argument, with all lowercase letters
   translated to uppercase, including accented letters of the ISO
   Latin-1 (8859-1) character set. *)

val lowercase : [> `Read] t -> _ t
(** Return a copy of the argument, with all uppercase letters
   translated to lowercase, including accented letters of the ISO
   Latin-1 (8859-1) character set. *)

val capitalize : [> `Read] t -> _ t
(** Return a copy of the argument, with the first character set to uppercase. *)

val uncapitalize : [> `Read] t -> _ t
(** Return a copy of the argument, with the first character set to lowercase. *)

val fill : [> `Write] t -> int -> int -> char -> unit
(** [String.fill s start len c] modifies string [s] in place,
   replacing the characters number [start] to [start + len - 1]
   by [c].
   Raise [Invalid_argument] if [start] and [len] do not
   designate a valid substring of [s]. *)

val blit : [> `Read] t -> int -> [> `Write] t -> int -> int -> unit
(** [String.blit src srcoff dst dstoff len] copies [len] characters
   from string [src], starting at character number [srcoff], to
   string [dst], starting at character number [dstoff]. It works
   correctly even if [src] and [dst] are the same string,
   and the source and destination chunks overlap.
   Raise [Invalid_argument] if [srcoff] and [len] do not
   designate a valid substring of [src], or if [dstoff] and [len]
   do not designate a valid substring of [dst]. *)

val concat : [> `Read] t -> [> `Read] t list -> _ t
(** [String.concat sep sl] concatenates the list of strings [sl],
   inserting the separator string [sep] between each. *)

val escaped : [> `Read] t -> _ t
(** Return a copy of the argument, with special characters
   represented by escape sequences, following the lexical
   conventions of Objective Caml.  If there is no special
   character in the argument, return the original string itself,
   not a copy. *)

val quote : [> `Read] t -> _ t
  (** Add quotes around a string and escape any quote appearing in that string.
      This function is used typically when you need to generate source code
      from a string.

      [quote "foo"] returns ["\"foo\""]
      [quote "\"foo\""] returns ["\\\"foo\\\""]
      etc. *)

val replace_chars : (char -> [> `Read] t) -> [> `Read] t -> _ t
  (** [replace_chars f s] returns a string where all chars [c] of [s] have been
      replaced by the string returned by [f c]. *)

val replace : str:[> `Read] t -> sub:[> `Read] t -> by:[> `Read] t -> bool * _ t
  (** [replace ~str ~sub ~by] returns a tuple constisting of a boolean
      and a string where the first occurrence of the string [sub]
      within [str] has been replaced by the string [by]. The boolean
      is true if a subtitution has taken place. *)

val repeat: [> `Read] t -> int -> _ t
(** [repeat s n] returns [s ^ s ^ ... ^ s] *)

(******************************************************************************)
(** {6 Splitting} *)

val split : [> `Read] t -> [> `Read] t -> _ t * _ t
  (** [split s sep] splits the string [s] between the first
      occurrence of [sep].
      raises [Not_found] if the separator is not found. *)

val rsplit : [> `Read] t -> string -> string * string
  (** [rsplit s sep] splits the string [s] between the last
      occurrence of [sep].
      raises [Not_found] if the separator is not found. *)

val nsplit : [> `Read] t -> [> `Read] t -> _ t list
  (** [nsplit s sep] splits the string [s] into a list of strings
      which are separated by [sep].
      [nsplit "" _] returns the empty list. *)

val splice: [ `Read | `Write] t  -> int -> int -> [> `Read] t -> string
  (** [String.splice s off len rep] cuts out the section of [s]
      indicated by [off] and [len] and replaces it by [rep] *)

val join : [> `Read] t -> [> `Read] t list -> _ t
  (** Same as [concat] *)

val slice : ?first:int -> ?last:int -> [> `Read] t -> _ t
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

val explode : [> `Read] t -> char list
  (** [explode s] returns the list of characters in the string [s]. *)
val implode : char list -> _ t
  (** [implode cs] returns a string resulting from concatenating
      the characters in the list [cs]. *)

(******************************************************************************)
(** {6 Comparisons}*)

val compare: [> `Read] t -> [> `Read] t -> int
  (** The comparison function for strings, with the same specification as
      {!Pervasives.compare}.  Along with the type [t], this function [compare]
      allows the module [String] to be passed as argument to the functors
      {!Set.Make} and {!Map.Make}. *)

val icompare: [> `Read] t -> [> `Read] t -> int
  (** Compare two strings, case-insensitive. *)

val starts_with : [> `Read] t -> [> `Read] t -> bool
  (** [starts_with s x] return true if [s] is starting with [x]. *)

val ends_with : [> `Read] t -> [> `Read] t -> bool
  (** [ends_with s x] returns true if the string [s] is ending with [x]. *)

val contains_string : [> `Read] t -> [> `Read] t -> bool
  (** [contains_string str sub] returns true if [sub] is a substring of [str] or
      false otherwise. *)

val numeric_compare: [> `Read] t -> [> `Read] t -> int
  (** Compare two strings, sorting "abc32def" before "abc210abc" *)

(******************************************************************************)
(**/**)

(** {6 Undocumented operations} *)
external unsafe_get : [> `Read] t -> int -> char = "%string_unsafe_get"
external unsafe_set : [> `Write] -> int -> char -> unit = "%string_unsafe_set"
external unsafe_blit :
  [> `Read] t -> int -> [> `Write] -> int -> int -> unit = "caml_blit_string" "noalloc"
external unsafe_fill :
  [> `Write] -> int -> int -> char -> unit = "caml_fill_string" "noalloc"

(**/**)
(******************************************************************************)
end

end


