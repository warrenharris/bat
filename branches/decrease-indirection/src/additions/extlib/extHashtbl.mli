(* 
 * ExtHashtbl - extra functions over hashtables.
 * Copyright (C) 2003 Nicolas Cannasse
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
 
(** Extra functions over hashtables. *)

(** Operations over hashtables. *)
module Hashtbl :
  (** The wrapper module *)
  sig

type ('a,'b) t =  ('a,'b)  Hashtbl.t
    (** The type of a hashtable. *)

(**{6 Base operations}*)

val create : int -> ('a, 'b) t
(** [Hashtbl.create n] creates a new, empty hash table, with
   initial size [n].  For best results, [n] should be on the
   order of the expected number of elements that will be in
   the table.  The table grows as needed, so [n] is just an
   initial guess. *)

val length : ('a, 'b) t -> int
(** [Hashtbl.length tbl] returns the number of bindings in [tbl].
   Multiple bindings are counted multiply, so [Hashtbl.length]
   gives the number of times [Hashtbl.iter] calls its first argument. *)

val is_empty : ('a, 'b) t -> bool
  (** [Hashtbl.is_empty tbl] returns [true] if there are no bindings
      in [tbl], false otherwise.*)

val add : ('a, 'b) t -> 'a -> 'b -> unit
(** [Hashtbl.add tbl x y] adds a binding of [x] to [y] in table [tbl].
   Previous bindings for [x] are not removed, but simply
   hidden. That is, after performing {!Hashtbl.remove}[ tbl x],
   the previous binding for [x], if any, is restored.
   (Same behavior as with association lists.) *)

val remove : ('a, 'b) t -> 'a -> unit
(** [Hashtbl.remove tbl x] removes the current binding of [x] in [tbl],
   restoring the previous binding if it exists.
   It does nothing if [x] is not bound in [tbl]. *)

val remove_all : ('a,'b) t -> 'a -> unit
  (** Remove all bindings for the given key *)

val replace : ('a, 'b) t -> 'a -> 'b -> unit
(** [Hashtbl.replace tbl x y] replaces the current binding of [x]
   in [tbl] by a binding of [x] to [y].  If [x] is unbound in [tbl],
   a binding of [x] to [y] is added to [tbl].
   This is functionally equivalent to {!Hashtbl.remove}[ tbl x]
   followed by {!Hashtbl.add}[ tbl x y]. *)

val copy : ('a, 'b) t -> ('a, 'b) t
(** Return a copy of the given hashtable. *)

val clear : ('a, 'b) t -> unit
(** Empty a hash table. *)


(**{6 Searching}*)

val find : ('a, 'b) t -> 'a -> 'b
(** [Hashtbl.find tbl x] returns the current binding of [x] in [tbl],
   or raises [Not_found] if no such binding exists. *)

val find_all : ('a, 'b) t -> 'a -> 'b list
(** [Hashtbl.find_all tbl x] returns the list of all data
   associated with [x] in [tbl].
   The current binding is returned first, then the previous
   bindings, in reverse order of introduction in the table. *)

val find_default : ('a,'b) t -> 'a -> 'b -> 'b
  (** Find a binding for the key, and return a default
      value if not found *)

val find_option : ('a,'b) Hashtbl.t -> 'a -> 'b option
  (** Find a binding for the key, or return [None] if no
      value is found *)

val mem : ('a, 'b) t -> 'a -> bool
(** [Hashtbl.mem tbl x] checks if [x] is bound in [tbl]. *)



val exists : ('a,'b) t -> 'a -> bool
  (** [exists h k] returns true is at least one item with key [k] is
      found in the hashtable. *)


(**{6 Traversing}*)
val iter : ('a -> 'b -> unit) -> ('a, 'b) t -> unit
(** [Hashtbl.iter f tbl] applies [f] to all bindings in table [tbl].
   [f] receives the key as first argument, and the associated value
   as second argument. Each binding is presented exactly once to [f].
   The order in which the bindings are passed to [f] is unspecified.
   However, if the table contains several bindings for the same key,
   they are passed to [f] in reverse order of introduction, that is,
   the most recent binding is passed first. *)

val fold : ('a -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
(** [Hashtbl.fold f tbl init] computes
   [(f kN dN ... (f k1 d1 init)...)],
   where [k1 ... kN] are the keys of all bindings in [tbl],
   and [d1 ... dN] are the associated values.
   Each binding is presented exactly once to [f].
   The order in which the bindings are passed to [f] is unspecified.
   However, if the table contains several bindings for the same key,
   they are passed to [f] in reverse order of introduction, that is,
   the most recent binding is passed first. *)

val map : ('b -> 'c) -> ('a,'b) t -> ('a,'c) t
  (** [map f x] creates a new hashtable with the same
      keys as [x], but with the function [f] applied to
      all the values *)

(**{6 Conversions}*)

val keys : ('a,'b) t -> 'a Enum.t
  (** Return an enumeration of all the keys of a hashtable.
      If the key is in the Hashtable multiple times, all occurrences
      will be returned.  *)

val values : ('a,'b) t -> 'b Enum.t
  (** Return an enumeration of all the values of a hashtable. *)

val enum : ('a, 'b) t -> ('a * 'b) Enum.t
  (** Return an enumeration of (key,value) pairs of a hashtable. *)

val of_enum : ('a * 'b) Enum.t -> ('a, 'b) t
  (** Create a hashtable from a (key,value) enumeration. *)



  end
