(*
 * ExtPervasives - Additional functions
 * Copyright (C) 1996 Xavier Leroy
 *               2003 Nicolas Cannasse
 *               2007 Zheng Li
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

(** {6 Additional functions.}

    @author Xavier Leroy (Base module)
    @author Nicolas Cannasse
    @author David Teller
    @author Zheng Li
*)

open IO

(** The initially opened module.
    
    This module provides the basic operations over the built-in types
    (numbers, booleans, strings, exceptions, references, lists, arrays,
    input-output channels, ...)
    
    This module is automatically opened at the beginning of each compilation.
    All components of this module can therefore be referred by their short
    name, without prefixing them by [Standard].

    @author Xavier Leroy (Base module)
    @author Nicolas Cannasse
    @author David Teller
    @author Zheng Li

    @documents Pervasives
*)
module Pervasives :
sig

(** {6 Exceptions} *)

external raise : exn -> 'a = "%raise"
(** Raise the given exception value *)

val invalid_arg : string -> 'a
(** Raise exception [Invalid_argument] with the given string. *)

val failwith : string -> 'a
(** Raise exception [Failure] with the given string. 

    {b Note} This function is provided as a simple technique for
    exiting a function or a program with an error message. It is
    however considered a bad practice to define a library which makes
    use of this function. So don't use it except for quick experiments
    and for teaching.
*)

(** {6 Program termination} *)


val exit : int -> 'a
  (** Terminate the process, returning the given status code
      to the operating system: usually 0 to indicate no errors,
      and a small positive integer to indicate failure. 
      All open output channels are flushed with [flush_all].
      An implicit [exit 0] is performed each time a program
      terminates normally.  An implicit [exit 2] is performed if the program
      terminates early because of an uncaught exception. *)

val at_exit : (unit -> unit) -> unit
  (** Register the given function to be called at program
      termination time. The functions registered with [at_exit]
      will be called when the program executes {!Pervasives.exit},
      or terminates, either normally or because of an uncaught exception.
      The functions are called in ``last in, first out'' order:
      the function most recently added with [at_exit] is called first. *)


(** {6 Comparisons} *)


external ( = ) : 'a -> 'a -> bool = "%equal"
(** [e1 = e2] tests for structural equality of [e1] and [e2].
   Mutable structures (e.g. references and arrays) are equal
   if and only if their current contents are structurally equal,
   even if the two mutable objects are not the same physical object.
   Equality between functional values raises [Invalid_argument].
   Equality between cyclic data structures does not terminate. *)

external ( <> ) : 'a -> 'a -> bool = "%notequal"
(** Negation of {!Pervasives.(=)}. *)

external ( < ) : 'a -> 'a -> bool = "%lessthan"
(** See {!Pervasives.(>=)}. *)

external ( > ) : 'a -> 'a -> bool = "%greaterthan"
(** See {!Pervasives.(>=)}. *)

external ( <= ) : 'a -> 'a -> bool = "%lessequal"
(** See {!Pervasives.(>=)}. *)

external ( >= ) : 'a -> 'a -> bool = "%greaterequal"
(** Structural ordering functions. These functions coincide with
   the usual orderings over integers, characters, strings
   and floating-point numbers, and extend them to a
   total ordering over all types.
   The ordering is compatible with [(=)]. As in the case
   of [(=)], mutable structures are compared by contents.
   Comparison between functional values raises [Invalid_argument].
   Comparison between cyclic structures does not terminate. *)

external compare : 'a -> 'a -> int = "%compare"
(** [compare x y] returns [0] if [x] is equal to [y],
   a negative integer if [x] is less than [y], and a positive integer
   if [x] is greater than [y].  The ordering implemented by [compare]
   is compatible with the comparison predicates [=], [<] and [>]
   defined above,  with one difference on the treatment of the float value
   {!Pervasives.nan}.  Namely, the comparison predicates treat [nan]
   as different from any other float value, including itself;
   while [compare] treats [nan] as equal to itself and less than any
   other float value.  This treatment of [nan] ensures that [compare]
   defines a total ordering relation.

   [compare] applied to functional values may raise [Invalid_argument].
   [compare] applied to cyclic structures may not terminate.

   The [compare] function can be used as the comparison function
   required by the {!Set.Make} and {!Map.Make} functors, as well as
   the {!List.sort} and {!Array.sort} functions. *)

val min : 'a -> 'a -> 'a
(** Return the smaller of the two arguments. *)

val max : 'a -> 'a -> 'a
(** Return the greater of the two arguments. *)

external ( == ) : 'a -> 'a -> bool = "%eq"
(** [e1 == e2] tests for physical equality of [e1] and [e2].
   On integers and characters, physical equality is identical to structural
   equality. On mutable structures, [e1 == e2] is true if and only if
   physical modification of [e1] also affects [e2].
   On non-mutable structures, the behavior of [(==)] is
   implementation-dependent; however, it is guaranteed that
   [e1 == e2] implies [compare e1 e2 = 0]. *)

external ( != ) : 'a -> 'a -> bool = "%noteq"
(** Negation of {!Pervasives.(==)}. *)


(** {6 Boolean operations} *)


external not : bool -> bool = "%boolnot"
(** The boolean negation. *)

external ( && ) : bool -> bool -> bool = "%sequand"
(** The boolean ``and''. Evaluation is sequential, left-to-right:
   in [e1 && e2], [e1] is evaluated first, and if it returns [false],
   [e2] is not evaluated at all. *)

external ( & ) : bool -> bool -> bool = "%sequand"
(** @deprecated {!Pervasives.(&&)} should be used instead. *)

external ( || ) : bool -> bool -> bool = "%sequor"
(** The boolean ``or''. Evaluation is sequential, left-to-right:
   in [e1 || e2], [e1] is evaluated first, and if it returns [true],
   [e2] is not evaluated at all. *)

(*external ( or ) : bool -> bool -> bool = "%sequor"
(** @deprecated {!Pervasives.(||)} should be used instead.*)
  (*Removed because of Camlp4 bug*)
*)

(** {6 Integer arithmetic} 
    
    Integers are 31 bits wide (or 63 bits on 64-bit processors).
    All operations are taken modulo 2{^31} (or 2{^63}).
    
    {b Note} These operations do not fail on overflow. In other words,
    although {!max_int} is the largest possible integer, addition
    [max_int + 1] will succeed. However, the result if this addition
    is [min_int]. If you wish your operations to fail on overflow,
    open module {!Data.Numeric.SafeInt}.

    More operations on integers are defined in {!Int}, {!SafeInt},
    {!Int32}, {!Int64} and {!Native_int}.
*)

external ( ~- ) : int -> int = "%negint"
(** Unary negation. You can also write [-e] instead of [~-e]. *)

external succ : int -> int = "%succint"
(** [succ x] is [x+1]. *)

external pred : int -> int = "%predint"
(** [pred x] is [x-1]. *)

external ( + ) : int -> int -> int = "%addint"
(** Integer addition. *)

external ( - ) : int -> int -> int = "%subint"
(** Integer subtraction. *)

external ( * ) : int -> int -> int = "%mulint"
(** Integer multiplication. *)

external ( / ) : int -> int -> int = "%divint"
(** Integer division.
   Raise [Division_by_zero] if the second argument is 0.
   Integer division rounds the real quotient of its arguments towards zero.
   More precisely, if [x >= 0] and [y > 0], [x / y] is the greatest integer
   less than or equal to the real quotient of [x] by [y].  Moreover,
   [(-x) / y = x / (-y) = -(x / y)].  *)

external ( mod ) : int -> int -> int = "%modint"
(** Integer remainder.  If [y] is not zero, the result
   of [x mod y] satisfies the following properties:
   [x = (x / y) * y + x mod y] and
   [abs(x mod y) <= abs(y)-1].
   If [y = 0], [x mod y] raises [Division_by_zero].
   Notice that [x mod y] is nonpositive if and only if [x < 0].
   Raise [Division_by_zero] if [y] is zero. *)

val abs : int -> int
(** Return the absolute value of the argument.  Note that this may be
  negative if the argument is [min_int]. *)

val max_int : int
(** The greatest representable integer. *)

val min_int : int
(** The smallest representable integer. *)



(** {7 Bitwise operations} *)


external ( land ) : int -> int -> int = "%andint"
(** Bitwise logical and. *)

external ( lor ) : int -> int -> int = "%orint"
(** Bitwise logical or. *)

external ( lxor ) : int -> int -> int = "%xorint"
(** Bitwise logical exclusive or. *)

val lnot : int -> int
(** Bitwise logical negation. *)

external ( lsl ) : int -> int -> int = "%lslint"
(** [n lsl m] shifts [n] to the left by [m] bits.
   The result is unspecified if [m < 0] or [m >= bitsize],
   where [bitsize] is [32] on a 32-bit platform and
   [64] on a 64-bit platform. *)

external ( lsr ) : int -> int -> int = "%lsrint"
(** [n lsr m] shifts [n] to the right by [m] bits.
   This is a logical shift: zeroes are inserted regardless of
   the sign of [n].
   The result is unspecified if [m < 0] or [m >= bitsize]. *)

external ( asr ) : int -> int -> int = "%asrint"
(** [n asr m] shifts [n] to the right by [m] bits.
   This is an arithmetic shift: the sign bit of [n] is replicated.
   The result is unspecified if [m < 0] or [m >= bitsize]. *)


(** {6 Floating-point arithmetic}

    Caml's floating-point numbers follow the
    IEEE 754 standard, using double precision (64 bits) numbers.
    Floating-point operations never raise an exception on overflow,
    underflow, division by zero, etc.  Instead, special IEEE numbers
    are returned as appropriate, such as [infinity] for [1.0 /. 0.0],
    [neg_infinity] for [-1.0 /. 0.0], and [nan] (``not a number'')
    for [0.0 /. 0.0].  These special numbers then propagate through
    floating-point computations as expected: for instance,
    [1.0 /. infinity] is [0.0], and any operation with [nan] as
    argument returns [nan] as result.

    More floating-point operations are defined in {!Float}.
*)

external ( ~-. ) : float -> float = "%negfloat"
(** Unary negation. You can also write [-.e] instead of [~-.e]. *)

external ( +. ) : float -> float -> float = "%addfloat"
(** Floating-point addition *)

external ( -. ) : float -> float -> float = "%subfloat"
(** Floating-point subtraction *)

external ( *. ) : float -> float -> float = "%mulfloat"
(** Floating-point multiplication *)

external ( /. ) : float -> float -> float = "%divfloat"
(** Floating-point division. *)

external ( ** ) : float -> float -> float = "caml_power_float" "pow" "float"
(** Exponentiation *)

external sqrt : float -> float = "caml_sqrt_float" "sqrt" "float"
(** Square root *)

external exp : float -> float = "caml_exp_float" "exp" "float"
(** Exponential. *)

external log : float -> float = "caml_log_float" "log" "float"
(** Natural logarithm. *)

external log10 : float -> float = "caml_log10_float" "log10" "float"
(** Base 10 logarithm. *)

external cos : float -> float = "caml_cos_float" "cos" "float"
(** See {!Pervasives.atan2}. *)

external sin : float -> float = "caml_sin_float" "sin" "float"
(** See {!Pervasives.atan2}. *)

external tan : float -> float = "caml_tan_float" "tan" "float"
(** See {!Pervasives.atan2}. *)

external acos : float -> float = "caml_acos_float" "acos" "float"
(** See {!Pervasives.atan2}. *)

external asin : float -> float = "caml_asin_float" "asin" "float"
(** See {!Pervasives.atan2}. *)

external atan : float -> float = "caml_atan_float" "atan" "float"
(** See {!Pervasives.atan2}. *)

external atan2 : float -> float -> float = "caml_atan2_float" "atan2" "float"
(** The usual trigonometric functions. *)

external cosh : float -> float = "caml_cosh_float" "cosh" "float"
(** See {!Pervasives.tanh}. *)

external sinh : float -> float = "caml_sinh_float" "sinh" "float"
(** See {!Pervasives.tanh}. *)

external tanh : float -> float = "caml_tanh_float" "tanh" "float"
(** The usual hyperbolic trigonometric functions. *)

external ceil : float -> float = "caml_ceil_float" "ceil" "float"
(** See {!Pervasives.floor}. *)

external floor : float -> float = "caml_floor_float" "floor" "float"
(** Round the given float to an integer value.
   [floor f] returns the greatest integer value less than or
   equal to [f].
   [ceil f] returns the least integer value greater than or
   equal to [f]. *)

external abs_float : float -> float = "%absfloat"
(** Return the absolute value of the argument. *)

external mod_float : float -> float -> float = "caml_fmod_float" "fmod" "float"
(** [mod_float a b] returns the remainder of [a] with respect to
   [b].  The returned value is [a -. n *. b], where [n]
   is the quotient [a /. b] rounded towards zero to an integer. *)

external frexp : float -> float * int = "caml_frexp_float"
(** [frexp f] returns the pair of the significant
   and the exponent of [f].  When [f] is zero, the
   significant [x] and the exponent [n] of [f] are equal to
   zero.  When [f] is non-zero, they are defined by
   [f = x *. 2 ** n] and [0.5 <= x < 1.0]. *)

external ldexp : float -> int -> float = "caml_ldexp_float"
(** [ldexp x n] returns [x *. 2 ** n]. *)

external modf : float -> float * float = "caml_modf_float"
(** [modf f] returns the pair of the fractional and integral
   part of [f]. *)

external float : int -> float = "%floatofint"
(** Same as {!Pervasives.float_of_int}. *)

external float_of_int : int -> float = "%floatofint"
(** Convert an integer to floating-point. *)

external truncate : float -> int = "%intoffloat"
(** Same as {!Pervasives.int_of_float}. *)

external int_of_float : float -> int = "%intoffloat"
(** Truncate the given floating-point number to an integer.
   The result is unspecified if the argument is [nan] or falls outside the
   range of representable integers. *)

val infinity : float
(** Positive infinity. *)

val neg_infinity : float
(** Negative infinity. *)

val nan : float
(** A special floating-point value denoting the result of an
   undefined operation such as [0.0 /. 0.0].  Stands for
   ``not a number''.  Any floating-point operation with [nan] as
   argument returns [nan] as result.  As for floating-point comparisons,
   [=], [<], [<=], [>] and [>=] return [false] and [<>] returns [true]
   if one or both of their arguments is [nan]. *)



(** {6 String operations}

   More string operations are provided in module {!String}.
*)

val ( ^ ) : string -> string -> string
(** String concatenation. *)

val uppercase : string -> string
(** Return a copy of the argument, with all lowercase letters
    translated to uppercase, including accented letters of the ISO
    Latin-1 (8859-1) character set. *)

val lowercase : string -> string
(** Return a copy of the argument, with all uppercase letters
    translated to lowercase, including accented letters of the ISO
    Latin-1 (8859-1) character set. *)

(** {6 Character operations}

   More character operations are provided in module {!Char}.
*)

external int_of_char : char -> int = "%identity"
(** Return the ASCII code of the argument. *)

val char_of_int : int -> char
(** Return the character with the given ASCII code.
   Raise [Invalid_argument "char_of_int"] if the argument is
   outside the range 0--255. *)


(** {6 Unit operations} 

    More unit operations are provided in module {!Unit}
*)

external ignore : 'a -> unit = "%ignore"
(** Discard the value of its argument and return [()].
   For instance, [ignore(f x)] discards the result of
   the side-effecting function [f].  It is equivalent to
   [f x; ()], except that the latter may generate a
   compiler warning; writing [ignore(f x)] instead
   avoids the warning. *)


(** {6 String conversion functions} 

    These are the most common string conversion functions.  For
    additional string conversion functions, see in the corresponding
    module (e.g. for conversion between [int32] and [string],
    see module {!Int32}).
*)

val string_of_char : char -> string
(** creates a string from a char. *)

val string_of_bool : bool -> string
  (** Return the string representation of a boolean. *)

val bool_of_string : string -> bool
(** Convert the given string to a boolean.
   Raise [Invalid_argument "bool_of_string"] if the string is not
   ["true"] or ["false"]. *)

val string_of_int : int -> string
(** Return the string representation of an integer, in decimal. *)

external int_of_string : string -> int = "caml_int_of_string"
(** Convert the given string to an integer.
   The string is read in decimal (by default) or in hexadecimal (if it
   begins with [0x] or [0X]), octal (if it begins with [0o] or [0O]),
   or binary (if it begins with [0b] or [0B]).
   Raise [Failure "int_of_string"] if the given string is not
   a valid representation of an integer, or if the integer represented
   exceeds the range of integers representable in type [int]. *)

val string_of_float : float -> string
(** Return the string representation of a floating-point number. *)

external float_of_string : string -> float = "caml_float_of_string"
(** Convert the given string to a float.  Raise [Failure "float_of_string"]
   if the given string is not a valid representation of a float. *)

val dump : 'a -> string
(** Attempt to convert a value to a string.

    Since types are lost at compile time, the representation might not
    match your type. For example, None will be printed 0 since they
    share the same runtime representation. *)


(** {6 List operations}

    More list operations are provided in module {!List}.
*)

val ( @ ) : 'a list -> 'a list -> 'a list
(** List concatenation. *)


(** {6 Input/output} 

    This section only contains the most common input/output operations.
    More operations may be found in modules {!IO} and {!File}.
*)

val stdin : input
(** Standard input, as per Unix/Windows conventions (by default, keyboard).

    Use this input to read what the user is writing on the keyboard.*)

val stdout: unit output
(** Standard output, as per Unix/Windows conventions (by default, console).

    Use this output to display regular messages.*)

val stderr: unit output
(** Standard error output, as per Unix/Windows conventions.
   
    Use this output to display warnings and error messages.*)

val stdnull: unit output
(** An output which discards everything written to it.

    Use this output to ignore messages.*)

val flush_all : unit -> unit
(** Write all pending data to output channels, ignore all errors.

    It is normally not necessary to call this function, as all pending
    data is written when an output channel is closed or when the
    program itself terminates, either normally or because of an
    uncaught exception. However, this function is useful for
    debugging, as it forces pending data to be written immediately.
*)

(** {7 Output functions on standard output} *)


val print_bool : bool -> unit
(** Print a boolean on standard output. *)

val print_char : char -> unit
(** Print a character on standard output. *)

val print_string : string -> unit
(** Print a string on standard output. *)

val print_int : int -> unit
(** Print an integer, in decimal, on standard output. *)

val print_float : float -> unit
(** Print a floating-point number, in decimal, on standard output. *)

val print_endline : string -> unit
(** Print a string, followed by a newline character, on
   standard output and flush standard output. *)

val print_newline : unit -> unit
(** Print a newline character on standard output, and flush
   standard output. This can be used to simulate line
   buffering of standard output. *)

val print_guess : 'a -> unit
  (** Attempt to print the representation of a runtime value on the
      standard output.  See remarks for {!dump}. This function is
      useful mostly for debugging. As a general rule, it should not be
      used in production code.*)

val print_all : input -> unit
  (** Print the contents of an input to the standard output.*)

(** {7 Output functions on standard error} *)

val prerr_bool : bool -> unit
(** Print a boolean to stderr. *)

val prerr_char : char -> unit
(** Print a character on standard error. *)

val prerr_string : string -> unit
(** Print a string on standard error. *)

val prerr_int : int -> unit
(** Print an integer, in decimal, on standard error. *)

val prerr_float : float -> unit
(** Print a floating-point number, in decimal, on standard error. *)

val prerr_endline : string -> unit
(** Print a string, followed by a newline character on standard error
   and flush standard error. *)

val prerr_newline : unit -> unit
(** Print a newline character on standard error, and flush
   standard error. *)

val prerr_guess : 'a -> unit
  (** Attempt to print the representation of a runtime value on the
      error output.  See remarks for {!dump}. This function is
      useful mostly for debugging.*)

val prerr_all : input -> unit
  (** Print the contents of an input to the error output.*)

(** {7 Input functions on standard input} *)

val read_line : unit -> string
(** Flush standard output, then read characters from standard input
   until a newline character is encountered. Return the string of
   all characters read, without the newline character at the end. *)

val read_int : unit -> int
(** Flush standard output, then read one line from standard input
   and convert it to an integer. Raise [Failure "int_of_string"]
   if the line read is not a valid representation of an integer. *)

val read_float : unit -> float
(** Flush standard output, then read one line from standard input
   and convert it to a floating-point number.
   The result is unspecified if the line read is not a valid
   representation of a floating-point number. *)

(** {7 General output functions} *)

val open_out : ?mode:(File.open_out_flag list) -> 
               ?perm:File.permission           ->
  string -> unit IO.output
  (** Open the named file for writing, and return a new output channel
      on that file. You will need to close the file once you have
      finished using it.
      
      You may use optional argument [mode] to decide whether the
      output will overwrite the contents of the file (by default) or
      to add things at the end of the file, whether the file should be
      created if it does not exist yet (the default) or not, whether
      this operation should proceed if the file exists already (the
      default) or not, whether the file should be opened as text
      (the default) or as binary, and whether the file should be
      opened for non-blocking operations.

      You may use optional argument [perm] to specify the permissions
      of the file, as per Unix conventions. By default, files are created
      with default permissions (which depend on your setup).

      Raise [Sys_error] if the file could not be opened. *)

val open_out_bin : string -> unit IO.output
  (** Same as {!open_out}, but the file is opened in binary mode, so
      that no translation takes place during writes. On operating
      systems that do not distinguish between text mode and binary
      mode, this function behaves like {!open_out} without any
      [mode] or [perm]. *)

val open_out_gen : open_flag list -> int -> string -> unit IO.output
  (**
     [open_out_gen mode perm filename] opens the named file for writing,
     as described above. The extra argument [mode]
     specifies the opening mode. The extra argument [perm] specifies
     the file permissions, in case the file must be created.

     @deprecated Use {!open_out instead}*)

val flush : unit IO.output -> unit
  (** Flush the buffer associated with the given output, performing
      all pending writes on that channel. Interactive programs must be
      careful about flushing standard output and standard error at the
      right time. *)



val output_char : unit IO.output -> char -> unit
(** Write the character on the given output channel. *)

val output_string : unit IO.output -> string -> unit
(** Write the string on the given output channel. *)

val output_rope : unit IO.output -> Rope.t -> unit
(** Write the rope on the given output channel. *)

val output : unit IO.output -> string -> int -> int -> unit
(** [output oc buf pos len] writes [len] characters from string [buf],
   starting at offset [pos], to the given output channel [oc].
   Raise [Invalid_argument "output"] if [pos] and [len] do not
   designate a valid substring of [buf]. *)

val output_byte : unit IO.output -> int -> unit
(** Write one 8-bit integer (as the single character with that code)
   on the given output channel. The given integer is taken modulo
   256. *)

val output_binary_int : unit IO.output -> int -> unit
  (** Write one integer in binary format (4 bytes, big-endian)
      on the given output channel.
      The given integer is taken modulo 2{^32}.
      The only reliable way to read it back is through the
      {!Pervasives.input_binary_int} function. The format is compatible across
      all machines for a given version of Objective Caml. *)

val output_value : unit IO.output -> 'a -> unit
  (** Write the representation of a structured value of any type
      to a channel. Circularities and sharing inside the value
      are detected and preserved. The object can be read back,
      by the function {!input_value}. See the description of module
      {!Marshal} for more information. {!output_value} is equivalent
      to {!Marshal.output} with an empty list of flags. *)


val close_out : unit IO.output -> unit
  (** Close the given channel, flushing all buffered write operations.
      Output functions raise a [Sys_error] exception when they are
      applied to a closed output channel, except [close_out] and [flush],
      which do nothing when applied to an already closed channel.
      Note that [close_out] may raise [Sys_error] if the operating
      system signals an error when flushing or closing. *)
  
val close_out_noerr : unit IO.output -> unit
  (** Same as [close_out], but ignore all errors. *)
 
(** {7 General input functions} *)

val open_in : ?mode:(File.open_in_flag list) -> 
  ?perm:File.permission -> 
  string -> IO.input
(** Open the named file for reading. You will need to close the file once you have
    finished using it.
    
    You may use optional argument [mode] to decide whether the opening
    should fail if the file doesn't exist yet (by default) or whether
    the file should be created if it doesn't exist yet, whether the
    opening should fail if the file already exists or not (by
    default), whether the file should be read as binary (by default)
    or as text, and whether reading should be non-blocking.

    You may use optional argument [perm] to specify the permissions of
    the file, should it be created, as per Unix conventions. By
    default, files are created with default permissions (which depend
    on your setup).

    Raise [Sys_error] if the file could not be opened. *)


val open_in_bin : string -> IO.input
(** Same as {!Pervasives.open_in}, but the file is opened in binary mode,
   so that no translation takes place during reads. On operating
   systems that do not distinguish between text mode and binary
   mode, this function behaves like {!Pervasives.open_in}. *)

val open_in_gen : open_flag list -> int -> string -> IO.input
(** [open_in mode perm filename] opens the named file for reading,
    as described above. The extra arguments [mode] and [perm]
    specify the opening mode and file permissions.
    {!Pervasives.open_in} and {!Pervasives.open_in_bin} are special
    cases of this function.
    
    @deprecated Use {!open_in instead}*)


val input_char : IO.input -> char
(** Read one character from the given input channel.
   Raise [End_of_file] if there are no more characters to read. *)

val input_line : IO.input -> string
(** Read characters from the given input channel, until a
   newline character is encountered. Return the string of
   all characters read, without the newline character at the end.
   Raise [End_of_file] if the end of the file is reached
   at the beginning of line. *)

val input : IO.input -> string -> int -> int -> int
(** [input ic buf pos len] reads up to [len] characters from
   the given channel [ic], storing them in string [buf], starting at
   character number [pos].
   It returns the actual number of characters read, between 0 and
   [len] (inclusive).
   A return value of 0 means that the end of file was reached.
   A return value between 0 and [len] exclusive means that
   not all requested [len] characters were read, either because
   no more characters were available at that time, or because
   the implementation found it convenient to do a partial read;
   [input] must be called again to read the remaining characters,
   if desired.  (See also {!Pervasives.really_input} for reading
   exactly [len] characters.)
   Exception [Invalid_argument "input"] is raised if [pos] and [len]
   do not designate a valid substring of [buf]. *)

val really_input : IO.input -> string -> int -> int -> unit
(** [really_input ic buf pos len] reads [len] characters from channel [ic],
    storing them in string [buf], starting at character number [pos].
    Raise [End_of_file] if the end of file is reached before [len]
    characters have been read.
    Raise [Invalid_argument "really_input"] if
    [pos] and [len] do not designate a valid substring of [buf]. *)

val input_byte : IO.input -> int
(** Same as {!Pervasives.input_char}, but return the 8-bit integer representing
    the character.
    Raise [End_of_file] if an end of file was reached. *)

val input_binary_int : IO.input -> int
(** Read an integer encoded in binary format (4 bytes, big-endian)
    from the given input channel. See {!Pervasives.output_binary_int}.
    Raise [End_of_file] if an end of file was reached while reading the
    integer. *)

val input_value : IO.input -> 'a
(** Read the representation of a structured value, as produced
    by {!output_value}, and return the corresponding value.
    This function is identical to {!Marshal.input};
    see the description of module {!Marshal} for more information,
    in particular concerning the lack of type safety. *)
  
val close_in : IO.input -> unit
  (** Close the given channel.  Input functions raise a [Sys_error]
      exception when they are applied to a closed input channel,
      except [close_in], which does nothing when applied to an already
      closed channel.  Note that [close_in] may raise [Sys_error] if
      the operating system signals an error. *)
  
val close_in_noerr : IO.input -> unit
(** Same as [close_in], but ignore all errors. *)
  

 
(** {6 References} 

    More operations on references are defined in module {!Ref}.
*)

external ref : 'a -> 'a ref = "%makemutable"
(** Return a fresh reference containing the given value. *)

external ( ! ) : 'a ref -> 'a = "%field0"
(** [!r] returns the current contents of reference [r].
   Equivalent to [fun r -> r.contents]. *)

external ( := ) : 'a ref -> 'a -> unit = "%setfield0"
(** [r := a] stores the value of [a] in reference [r].
   Equivalent to [fun r v -> r.contents <- v]. *)

external incr : int ref -> unit = "%incr"
(** Increment the integer contained in the given reference.
   Equivalent to [fun r -> r := succ !r]. *)

external decr : int ref -> unit = "%decr"
(** Decrement the integer contained in the given reference.
   Equivalent to [fun r -> r := pred !r]. *)

(** {6 Operations on format strings} *)

(** See modules {!Printf} and {!Scanf} for more operations on
    format strings. *)
type ('a, 'b, 'c, 'd) format4 = ('a, 'b, 'c, 'c, 'c, 'd) format6 

type ('a, 'b, 'c) format = ('a, 'b, 'c, 'c) format4
(** Simplified type for format strings, included for backward compatibility
    with earlier releases of Objective Caml.
    ['a] is the type of the parameters of the format,
    ['c] is the result type for the "printf"-style function,
    and ['b] is the type of the first argument given to
    [%a] and [%t] printing functions. *)

val string_of_format : ('a, 'b, 'c, 'd, 'e, 'f) format6 -> string
(** Converts a format string into a string. *)

external format_of_string :
  ('a, 'b, 'c, 'd, 'e, 'f) format6 ->
  ('a, 'b, 'c, 'd, 'e, 'f) format6 = "%identity"
(** [format_of_string s] returns a format string read from the string
    literal [s]. *)

val ( ^^ ) :
      ('a, 'b, 'c, 'd, 'e, 'f) format6 ->
      ('f, 'b, 'c, 'e, 'g, 'h) format6 ->
      ('a, 'b, 'c, 'd, 'g, 'h) format6
(** [f1 ^^ f2] catenates formats [f1] and [f2].  The result is a format
  that accepts arguments from [f1], then arguments from [f2]. *)


(**
   {6 Fundamental functions and operators}
*)

external identity : 'a -> 'a = "%identity"
(** The identity function. *)

val undefined : ?message:string -> 'a -> 'b
(** The undefined function.

    Evaluating [undefined x] always fails and raises an exception 
    "Undefined". Optional argument [message] permits the 
    customization of the error message.*)


val ( |> ) : 'a -> ('a -> 'b) -> 'b
(** Function application. [x |> f] is equivalent to [f x]. 

    This operator is commonly used to write a function composition by
    order of evaluation (the order used in object-oriented
    programming) rather than by inverse order (the order typically
    used in functional programming).  

    For instance, [g (f x)] means "apply [f] to [x], then apply [g] to
    the result." The corresponding notation in most object-oriented
    programming languages would be somewhere along the lines of [x.f.g
    ()], or "starting from [x], apply [f], then apply [g]." In OCaml,
    operator ( |> ) this latest notation maps to [x |> f |> g], or
    
    This operator may also be useful for composing sequences of
    function calls without too many parenthesis. *)

val ( **>  ) : ('a -> 'b) -> 'a -> 'b
  (** Function application. [f **> x] is equivalent to [f x]. 
      
      This operators may be useful for composing sequences of
      function calls without too many parenthesis.

      {b Note} The name of this operator is not written in stone.
      It is bound to change soon.*)

val ( |- ) : ('a -> 'b) -> ('b -> 'c) -> 'a -> 'c
(** Function composition. [f |- g] is [fun x -> g (f x)]. 
    This is also equivalent to applying [<**] twice.*)

val ( -| ) : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b
(** Function composition. [f -| g] is [fun x -> f (g x)]. Mathematically, this is
    operator o.*)

val flip : ( 'a -> 'b -> 'c ) -> 'b -> 'a -> 'c
  (** Argument flipping. 
      
      [flip f x y] is [f y x]. Don't abuse this function, it may shorten considerably
      your code but it also has the nasty habit of making it harder to read.*)
      

val ( *** ) : ('a -> 'b) -> ('c -> 'd) -> 'a * 'c -> 'b * 'd
(** Function pairing.

    [f *** g] is [fun (x,y) -> (f x, g y)].*)

val ( &&& ) : ('a -> 'b) -> ('a -> 'c) -> 'a -> ('b * 'c)
  (** Applying two functions to the same argument.

      [ f &&& g] is [fun x -> (f x, g x)]. *)

val first : ('a -> 'b) -> ('a * 'c) -> ('b * 'c)
(** Apply a function to the first element of a pair.

    [first f (x, y)] is [(f x, y)]
 *)

val second : ('a -> 'b) -> ('c * 'a) -> ('c * 'b)
(** Apply a function to the second element of a pair.

    [second f (x, y)] is [(x, f y)]
 *)


val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c
(** Convert a function which accepts a pair of arguments into
    a function which accepts two arguments.

    [curry f] is [fun x y -> f (x,y)]*)

val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c
  (** Convert a function which accepts a two arguments into a function
      which accepts a pair of arguments.

      [uncurry f] is [fun (x, y) -> f x y]*)

val const : 'a -> (_ -> 'a)
(** Ignore its second argument.

    [const x] is the function which always returns [x].*)

val unique : unit -> int
(** Returns an unique identifier every time it is called.

    {b Note} This is thread-safe.*)



val finally : (unit -> unit) -> ('a -> 'b) -> 'a -> 'b 
  (** [finally fend f x] calls [f x] and then [fend()] even if [f x] raised
      an exception. *)

val args : unit -> string Enum.t
  (** An enumeration of the arguments passed to this program through the command line.

      [args ()] is given by the elements of [Sys.argv], minus the first element.*)

(**/**)
val invisible_args : int ref
(** The number of arguments which must never be returned by [args]

    Typically, [invisible_args] is [1], to drop the name of the executable. However,
    in some circumstances, it may be useful to pretend that some arguments need not
    be parsed.
*)
(**/**)


val exe  : string
  (** The name of the current executable.

      [exe] is given by the first argument of [Sys.argv]*)


(**
   {6 Enumerations}

   In OCaml Batteries Included, all data structures are enumerable,
   which means that they support a number of standard operations,
   transformations, etc. The general manner of {i enumerating} the
   contents of a data structure is to invoke the [enum] function of
   your data structure.

   For instance, you may use the {!foreach} loop to apply a function
   [f] to all the consecutive elements of a string [s]. For this
   purpose, you may write either [foreach (String.enum s) f] or [open
   String in foreach (enum s) f]. Either possibility states that you
   are enumerating through a character string [s]. Should you prefer
   your enumeration to proceed from the end of the string to the
   beginning, you may replace {! String.enum} with {!
   String.backwards}. Therefore, either [foreach (String.backwards s)
   f] or [open String in foreach (backwards s) f] will apply [f]
   to all the consecutive elements of string [s], from the last to
   the first.

   Similarly, you may use {!List.enum} instead of {!String.enum} to
   visit the elements of a list in the usual order, or
   {!List.backwards} instead of {!String.backwards} to visit them
   in the opposite order, or {!Hashtbl.enum} for hash tables, etc.

   More operations on enumerations are defined in module {!Enum},
   including the necessary constructors to make your own structures
   enumerable.

   The various kinds of loops are detailed further in this documentation.
*)

val foreach: 'a Enum.t -> ('a -> unit) ->  unit
  (** Imperative loop on an enumeration.

      [foreach e f] applies function [f] to each successive element of [e].
      For instance, [foreach (1 -- 10) print_int] invokes function [print_int]
      on [1], [2], ..., [10], printing [12345678910].

      {b Note} This function is one of the many loops available on
      enumerations.  Other commonly used loops are {!iter} (same usage
      scenario as [foreach], but with different notations), {!map}
      (convert an enumeration to another enumeration) or {!fold}
      (flatten an enumeration by applying an operation to each
      element).

  *)

(**
   {7 General-purpose loops}

   {topic loops}

   The following functions are the three main general-purpose loops
   available in OCaml. By opposition to the loops available in
   imperative languages, OCaml loops are regular functions, which
   may be passed, composed, currified, etc. In particular, each
   of these loops may be considered either as a manner of applying
   a function to a data structure or as transforming a function
   into another function which will act on a whole data structure.

   For instance, if [f] is a function operating on one value, you may
   lift this function to operate on all values of an enumeration (and
   consequently on all values of any data structure of OCaml Batteries
   Included) by applying {!iter}, {!map} or {!fold} to this function.
*)


val iter : ('a -> unit) -> 'a Enum.t -> unit
  (** Imperative loop on an enumeration. This loop is typically used
      to lift a function with an effect but no meaningful result and
      get it to work on enumerations.

      If [f] is a function [iter f] is a function which behaves as [f]
      but acts upon enumerations rather than individual elements. As
      indicated in the type of [iter], [f] must produce values of type
      [unit] (i.e. [f] has no meaningful result) the resulting function
      produces no meaningful result either.

      In other words, [iter f] is a function which, when applied upon
      an enumeration [e], calls [f] with each element of [e] in turn.

      For instance, [iter f (1 -- 10)] invokes function [f] on [1],
      [2], ..., [10] and produces value [()].
  *)

val map : ('a -> 'b) -> 'a Enum.t -> 'b Enum.t
  (** Transformation loop on an enumeration, used to build an enumeration
      from another enumeration. This loop is typically used to transform
      an enumeration into another enumeration with the same number of
      elements, in the same order.

      If [f] is a function, [map f e] is a function which behaves as
      [f] but acts upon enumerations rather than individual elements --
      and builds a new enumeration from the results of each application.

      In other words, [map f] is a function which, when applied
      upon an enumeration containing elements [e1], [e2], ...,
      produces enumeration [f e1], [f e2], ...

      For instance, if [odd] is the function which returns [true]
      when applied to an odd number or [false] when applied to
      an even number, [map odd (1 -- 10)] produces enumeration
      [true], [false], [true], ..., [false].

      Similarly, if [square] is the function [fun x -> x * x],
      [map square (1 -- 10)] produces the enumeration of the
      square numbers of all numbers between [1] and [10].
*)


val reduce : ('a -> 'a -> 'a) -> 'a Enum.t -> 'a
  (** Transformation loop on an enumeration, used to build a single value
      from an enumeration.

      If [f] is a function and [e] is an enumeration, [reduce f e] applies
      function [f] to the first two elements of [e], then to the result of this
      expression and to the third element of [e], then to the result of this
      new expression and to the fourth element of [e]...

      In other words, [fold f e] returns [a_1] if [e] contains only
      one element, otherwise [f (... (f (f a1) a2) ...) aN] where
      a1..N are the elements of [e]. 

      @raise Not_found if [e] is empty.

      For instance, if [add] is the function [fun x y -> x + y],
      [reduce add] is the function which computes the sum of the
      elements of an enumeration -- and doesn't work on empty
      enumerations. Therefore, [reduce add (1 -- 10)]
      produces result [55].
  *)

val fold : ('a -> 'b -> 'b) -> 'b -> 'a Enum.t -> 'b
  (** Transformation loop on an enumeration, used to build a single value
      from an enumeration. This is the most powerful general-purpose
      loop and also the most complex.

      If [f] is a function, [fold f v e] applies [f v] to the first
      element of [e], then, calling [acc_1] the result of this
      operation, applies [f acc_1] to the second element of [e], then,
      calling [acc_2] the result of this operation, applies [f acc_2]
      to the third element of [e]...

      In other words, [fold f v e] returns [v] if [e] is empty,
      otherwise [f (... (f (f v a1) a2) ...) aN] where a1..N are
      the elements of [e]. 

      For instance, if [add] is the function [fun x y -> x + y],
      [fold add 0] is the function which computes the sum of the
      elements of an enumeration. Therefore, [fold add 0 (1 -- 10)]
      produces result [55].
  *)

val scanl : ('a -> 'b -> 'b) -> 'b -> 'a Enum.t -> 'b Enum.t
  (** Functional loop on an enumeration, used to build an enumeration
      from both an enumeration and an initial value. This function may
      be seen as a variant of {!fold} which returns not only the final
      result of {!fold} but the enumeration of all the intermediate
      results of {!fold}.

      If [f] is a function, [scanl f v e] is applies [f v] to the first
      element of [e], then, calling [acc_1] the result of this
      operation, applies [f acc_1] to the second element of [e], then,
      calling [acc_2] the result of this operation, applies [f acc_2]
      to the third element of [e]...

      For instance, if [add] is the function [fun x y -> x + y],
      [scanl add 0] is the function which computes the sum of the
      elements of an enumeration. Therefore, [scanl add 0 (1 -- 10)]
      produces result the enumeration with elements [0, 1, 3, 6, 10,
      15, 21, 28, 36, 45, 55].  *)

val ( /@ ) : 'a Enum.t -> ('a -> 'b) -> 'b Enum.t

val ( @/ ) : ('a -> 'b) -> 'a Enum.t -> 'b Enum.t
  (**
     Mapping operators.

     These operators have the same meaning as function {!map} but are
     sometimes more readable than this function, when chaining
     several transformations in a row.
  *)

(**
   {7 Other operations on enumerations}
*)

val exists: ('a -> bool) -> 'a Enum.t -> bool
(** [exists f e] returns [true] if there is some [x] in [e] such
    that [f x]*)

val for_all: ('a -> bool) -> 'a Enum.t -> bool
(** [exists f e] returns [true] if for every [x] in [e], [f x] is true*)



val find : ('a -> bool) -> 'a Enum.t -> 'a
  (** [find f e] returns the first element [x] of [e] such that [f x] returns
      [true], consuming the enumeration up to and including the
      found element, or, raises [Not_found] if no such element exists
      in the enumeration, consuming the whole enumeration in the search.
      
      Since [find] consumes a prefix of the enumeration, it can be used several 
      times on the same enumeration to find the next element. *)

val peek : 'a Enum.t -> 'a option
  (** [peek e] returns [None] if [e] is empty or [Some x] where [x] is
      the next element of [e]. The element is not removed from the
      enumeration. *)

val get : 'a Enum.t -> 'a option
  (** [get e] returns [None] if [e] is empty or [Some x] where [x] is
      the next element of [e], in which case the element is removed
      from the enumeration. *)

val push : 'a Enum.t -> 'a -> unit
  (** [push e x] will add [x] at the beginning of [e]. *)
  
val junk : 'a Enum.t -> unit
  (** [junk e] removes the first element from the enumeration, if any. *)

val filter : ('a -> bool) -> 'a Enum.t -> 'a Enum.t
  (** [filter f e] returns an enumeration over all elements [x] of [e] such
      as [f x] returns [true]. *)

val ( // ) : 'a Enum.t -> ('a -> bool) -> 'a Enum.t
(** Filtering (pronounce this operator name "such that").

    For instance, [(1 -- 37) // odd] is the enumeration of all odd
    numbers between 1 and 37.*)

val concat : 'a Enum.t Enum.t -> 'a Enum.t
  (** [concat e] returns an enumeration over all elements of all enumerations
      of [e]. *)

val ( -- ) : int -> int -> int Enum.t
(** Enumerate numbers.

    [5 -- 10] is the enumeration 5,6,7,8,9,10.
    [10 -- 5] is the empty enumeration*)

val ( --- ) : int -> int -> int Enum.t
(** As [--], but accepts enumerations in reverse order.

    [5 --- 10] is the enumeration 5,6,7,8,9,10.
    [10 --- 5] is the enumeration 10,9,8,7,6,5.*)

val ( --~ ) : char -> char -> char Enum.t
(** As ( -- ), but for characters.*)

val print :  ?first:string -> ?last:string -> ?sep:string -> ('a InnerIO.output -> 'b -> unit) -> 'a InnerIO.output -> 'b Enum.t -> unit
(** Print and consume the contents of an enumeration.*)

(**/**)

(**
   {6 Printing directives}
*)

(** {7 Equivalent of classical directives} *)

val pdir_a : (('acc IO.output -> 'a -> unit) -> 'a -> 'b, 'b, 'acc) Print.directive
val pdir_t : (('acc IO.output -> unit) -> 'a, 'a, 'acc) Print.directive
val pdir_B : (bool -> 'a, 'a, 'acc) Print.directive
val pdir_c : (char -> 'a, 'a, 'acc) Print.directive
val pdir_C : (char -> 'a, 'a, 'acc) Print.directive
val pdir_s : ?left_justify:bool -> ?width:int -> (string -> 'a, 'a, 'acc) Print.directive
val pdir_S : ?left_justify:bool -> ?width:int -> (string -> 'a, 'a, 'acc) Print.directive

(*
TODO: implement that:

type ('a, 'b) unum_directive = ?left_justify:bool -> ?pad_with_zeros:bool -> ?width:int -> ('a, 'b) Print.directive
  (** Directive which prints an unsigned number *)

type ('a, 'b) snum_directive = ?prefix_with_plus:bool -> ?prefix_with_space:bool -> ('a, 'b) unum_directive
  (** Directive which prints a signed number *)
*)

val pdir_d : (int -> 'a, 'a, 'acc) Print.directive
val pdir_i : (int -> 'a, 'a, 'acc) Print.directive
val pdir_u : (int -> 'a, 'a, 'acc) Print.directive
val pdir_x : (int -> 'a, 'a, 'acc) Print.directive
val pdir_X : (int -> 'a, 'a, 'acc) Print.directive
val pdir_o : (int -> 'a, 'a, 'acc) Print.directive
val pdir_ld : (int32 -> 'a, 'a, 'acc) Print.directive
val pdir_li : (int32 -> 'a, 'a, 'acc) Print.directive
val pdir_lu : (int32 -> 'a, 'a, 'acc) Print.directive
val pdir_lx : (int32 -> 'a, 'a, 'acc) Print.directive
val pdir_lX : (int32 -> 'a, 'a, 'acc) Print.directive
val pdir_lo : (int32 -> 'a, 'a, 'acc) Print.directive
val pdir_Ld : (int64 -> 'a, 'a, 'acc) Print.directive
val pdir_Li : (int64 -> 'a, 'a, 'acc) Print.directive
val pdir_Lu : (int64 -> 'a, 'a, 'acc) Print.directive
val pdir_Lx : (int64 -> 'a, 'a, 'acc) Print.directive
val pdir_LX : (int64 -> 'a, 'a, 'acc) Print.directive
val pdir_Lo : (int64 -> 'a, 'a, 'acc) Print.directive
val pdir_nd : (nativeint -> 'a, 'a, 'acc) Print.directive
val pdir_ni : (nativeint -> 'a, 'a, 'acc) Print.directive
val pdir_nu : (nativeint -> 'a, 'a, 'acc) Print.directive
val pdir_nx : (nativeint -> 'a, 'a, 'acc) Print.directive
val pdir_nX : (nativeint -> 'a, 'a, 'acc) Print.directive
val pdir_no : (nativeint -> 'a, 'a, 'acc) Print.directive

val pdir_f : (float -> 'a, 'a, 'acc) Print.directive
val pdir_F : (float -> 'a, 'a, 'acc) Print.directive

(** {7 Batteries-specific directives} *)

val pdir_format : (('a, 'b, 'acc) Print.format -> 'a, 'b, 'acc) Print.directive
  (** [sp_format] take a format, then the arguments of the format and
      print according to it. For example

      {[
        sprintf p"x = %format * %d" p"%d + %d" 1 3 5
        =
        "x = 1 + 3 * 5"
      ]} *)

val pdir_rope : (Rope.t -> 'a, 'a, 'acc) Print.directive
val pdir_utf8 : (ExtUTF8.UTF8.t -> 'a, 'a, 'acc) Print.directive
val pdir_obj : (< print : 'acc IO.output -> unit; .. > -> 'a, 'a, 'acc) Print.directive

(**/**)

(**
   {6 Results}
*)

type ('a, 'b) result = ('a, 'b) Std.result =
  | Ok  of 'a
  | Bad of 'b

(**
   {6 Thread-safety internals}

   Unless you are attempting to adapt Batteries Included to a new model of
   concurrency, you probably won't need this.
*)

val lock: Concurrent.lock ref
(**
   A lock used to synchronize internal operations.

   By default, this is {!Concurrent.nolock}. However, if you're using a version
   of Batteries compiled in threaded mode, this uses {!Mutex}. If you're attempting
   to use Batteries with another concurrency model, set the lock appropriately.
*)

end
