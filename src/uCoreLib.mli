(** Unicode characters.

   This module implements Unicode characters.
*)

(* Copyright (C) 2002, 2003, 2004, 2011 Yamagata Yoriyuki. *)

(* This library is free software; you can redistribute it and/or *)
(* modify it under the terms of the GNU Lesser General Public License *)
(* as published by the Free Software Foundation; either version 2 of *)
(* the License, or (at your option) any later version. *)

(* As a special exception to the GNU Library General Public License, you *)
(* may link, statically or dynamically, a "work that uses this library" *)
(* with a publicly distributed version of this library to produce an *)
(* executable file containing portions of this library, and distribute *)
(* that executable file under terms of your choice, without any of the *)
(* additional requirements listed in clause 6 of the GNU Library General *)
(* Public License. By "a publicly distributed version of this library", *)
(* we mean either the unmodified Library as distributed by the authors, *)
(* or a modified version of this library that is distributed under the *)
(* conditions defined in clause 3 of the GNU Library General Public *)
(* License. This exception does not however invalidate any other reasons *)
(* why the executable file might be covered by the GNU Library General *)
(* Public License . *)

(* This library is distributed in the hope that it will be useful, *)
(* but WITHOUT ANY WARRANTY; without even the implied warranty of *)
(* MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU *)
(* Lesser General Public License for more details. *)

(* You should have received a copy of the GNU Lesser General Public *)
(* License along with this library; if not, write to the Free Software *)
(* Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 *)
(* USA *)

(* You can contact the authour by sending email to *)
(* yori@users.sourceforge.net *)

(* This module could raises Invalid_arg and BatNumber.Overflow *)	

module UChar : sig
  type t
      
(** [char_of_exn u] returns the Latin-1 representation of [u].
    If [u] can not be represented by Latin-1, raises Invalid_arg
    [char_of] returns [None] instead of raising Invalid_arg. *)
  val char_of_exn : t -> char
  val char_of : t -> char option
      
(** [of_char c] returns the Unicode character of the Latin-1 character [c] *)
  val of_char : char -> t
      
(** [code u] returns the Unicode code number of [u]. *)
  external code : t -> int = "%identity"
      
(** [chr n] returns the Unicode character with the code number [n]. 
    If n does not lay in the valid range of Unicode or designates a
    surrogate charactor, returns [None] or raises invalid_arg *)
  val chr : int -> t option
  val chr_exn : int -> t
      
(** Equality by code point comparison *)
  val eq : t -> t -> bool
      
(** [compare u1 u2] returns, 
    a value > 0 if [u1] has a larger Unicode code number than [u2], 
    0 if [u1] and [u2] are the same Unicode character,
    a value < 0 if [u1] has a smaller Unicode code number than [u2]. *)
  val compare : t -> t -> int
      
  (** [escape u] returns the notation for code points defined in
      Unicode Standard.*)
  val escape : t -> string

(** Aliases of [type t] *)
  type uchar = t
	
(** Alias of [code] *)
  val int_of : uchar -> int
      
(** Alias of [chr] *)
  val of_int : int -> uchar option
  val of_int_exn : int -> uchar
end

(** Aliase for UChar.t *)
type uchar = UChar.t

module USet : sig
  type t
  (** the underlying representation is a balanced tree of ranges *)

  type elt = uchar
  (** This kind of set only holds uchars *)

  val empty : t
  (** The empty set *)

  val is_empty : t -> bool
  (** Test whether a set is empty, returns [true] if the set is empty. *)

  val mem : uchar -> t -> bool
  (** test whether a given uchar is a member of the set *)

  val add : uchar -> t -> t
  (** Add the given uchar to the set, returning a new set *)

  val add_range : uchar -> uchar -> t -> t 
  (** [add_range lo hi t] adds the range of uchars [lo, hi] (including both endpoints) to
     the given set, returning a new set
    @raise Invalid_argument if [lo] > [hi] *)

  val singleton : uchar -> t
  (** Return the singleton set containing only the given element *)

  val remove : uchar -> t -> t
  (** Remove an element from the given set, returning a new set *)

  val remove_range : uchar -> uchar -> t -> t
  (** [remove_range lo hi t] removes a range of elements from the given set, returning a new set
    @raise Invalid_argument if [lo] > [hi] *)

  val union : t -> t -> t
  (** Compute the union of two sets.  This is the set whose elements are
      those elements in either input set. *)

  val inter : t -> t -> t
  (** Compute the intersection of two sets.  This is the set whose
      elements are those in *both* of the input sets. *)

  val diff : t -> t -> t
  (** Compute the difference between two sets.  This is the set of
      elements that are in the first but not in the second. Unlike
      [union] and [inter], order matters here.*)

  val compl : t -> t
  (** Create the complement of the given set - i.e. the set of all
      values not in the input set. *)

  val compare : t -> t -> int
  (** Compare two sets.  It is not safe to use the polymorphic (<) and
      related functions to compare these sets, as the tree representation
      used can balance in multiple ways. *)

  val equal : t -> t -> bool
  (** Test whether two sets are equal.  It is not safe to use the
      polymorphic (=) on these sets, as the same set can have multiple
      representations depending on how it was built. *)

  val ord : t -> t -> BatOrd.order
  (** Same as [compare] but returns [BatOrd.Lt | BatOrd.Eq | BatOrd.Gt]
      instead of an int. *)

  val subset : t -> t -> bool
  (** [subset t u] returns [true] if [t] is a subset of [u] *)

  val from : uchar -> t -> t
  (** [from x t] returns the portion of [t] in the range [x, max_int] *)

  val after : uchar -> t -> t
  (** [after x t] returns the portion of [t] in the range [x+1, max_int] *)

  val until : uchar -> t -> t
  (** [until x t] returns the portion of [t] in the range [min_int, x] *)

  val before : uchar -> t -> t
  (** [before x t] returns the portion of [t] in the range [min_int, x-1] *)

  val iter : (uchar -> unit) -> t -> unit
  (** [iter f t] calls [f] once for each element of [t] *)

  val iter_range : (uchar -> uchar -> unit) -> t -> unit
  (** [iter_range f t] calls [f] once for each contiguous range of [t].
      The contiguous ranges of a set are sequences of adjacent integers
      all part of the set. *)

  val fold : (uchar -> 'a -> 'a) -> t -> 'a -> 'a
  (** [fold f t x0] returns the final result of merging each element of
      [t] into [x0] using merge function [f] *)

  val fold_range : (uchar -> uchar -> 'a -> 'a) -> t -> 'a -> 'a
  (** As fold, but operates on contiguous ranges *)

  val for_all : (uchar -> bool) -> t -> bool
  (** Tests whether a predicate applies to all elements of the set *)

  val exists : (uchar -> bool) -> t -> bool
  (** Test whether some element of a set satisfies a predicate *)

  val filter : (uchar -> bool) -> t -> t
  (** Builds the subset of those elements that satisfy the predicate *)

  val partition : (uchar -> bool) -> t -> t * t
  (** partitions the input set into two sets with elements that satisfy
      the predicate and those that don't *)

  val cardinal : t -> int
  (** Returns the number of elements in the set *)

  val elements : t -> uchar list
  (** Returns a list of all elements in the set *)

  val ranges : t -> (uchar * uchar) list
  (** Returns a list of all contiguous ranges in the set *)

  val min_elt : t -> uchar
  (** Returns the minimum element in the set *)

  val max_elt : t -> uchar
  (** Returns the maximum element in the set *)

  val choose : t -> uchar
  (** Returns some element in the set *)

  val enum : t -> (uchar * uchar) BatEnum.t
  (** Enumerates all contiguous ranges in the set *)

  val of_enum : (uchar*uchar) BatEnum.t -> t
  val of_list : (uchar*uchar) list -> t
  (** Build a ISet.t out of a list or enum of ranges *)

end

module Text : sig

  type t

  val empty : t

  val length : t -> int

  val max_length : int

(** [init len f] 
    returns a new text which contains [len] Unicode characters.
    The i-th Unicode character is initialized by [f i].  Raises
    Failure if [len] is minus. *)	
  val init : int -> (int -> uchar) -> t

  (** Returns a text which consists of the given single character. *)
  val of_uchar : uchar -> t

  (** [of_string s] converts UTF-8 encoded string [s] to Text.t 
      If [s] is an invalid UTF-8 string, returns None *)
  val of_string : string -> t option
  (** Same as above but raises Malformed_code instead of returing None *)
  val of_string_exn : string -> t

  (** Returns UTF-8 encoded string. *)
  val string_of : t -> string

  (** [of_string s] converts Ascii encoded string [s] to Text.t 
      If [s] is an invalid Ascii string, returns None *)
  val of_ascii : string -> t option
  (** Same as above but raises Malformed_code instead of returing None *)
  val of_ascii_exn : string -> t

  (** [of_string s] converts Latin-a encoded string [s] to Text.t *)
  val of_latin1 : string -> t

  (** Append two texts *)
  val append : t -> t -> t

  (** Append one Unicode character to the last of the text *)
  val append_uchar : t -> uchar -> t

  (** Byte order of texts *)
  val compare : t -> t -> int

  (** [get s i] gets [i]-th character of [s] *)
  val get : t -> int -> uchar option
  (** Raises Invalid_arg *)
  val get_exn : t -> int -> uchar

  (** Iterator.  Also behaves like a zipper.  Iterator can point the
  location after the last character of the text. *)
  type iterator

  (** The head of the text *)
  val first : t -> iterator
  (** Points the last element of the text *)
  val last : t -> iterator
  (** [nth t i] return the iterator which points the begininng of
  [i+1]-th character of [t]. *)
  val nth : t -> int -> iterator option
  (** Raises Invalid_argument "index out of bounds" if the argument is
  out of bound. *)
  val nth_exn : t -> int -> iterator

  (** Moving around an iterator *)
  val next : iterator -> iterator option
  (** Raises Invalid_argument "index out of bounds" if the iterator
  already locates after the last character of the underlining text. *)
  val next_exn : iterator -> iterator
  (** Points the privious character *)
  val prev : iterator -> iterator option
  (** Raises Invalid_argument "index out of bounds" if the iterator
  already locates in the first character of the underlining text. *)
  val prev_exn : iterator -> iterator

  (** Returns the value of the location which the iterator points. *)
  val value : iterator -> uchar

  (** Returns the underlining text of the give iterator. *)
  val base : iterator -> t
  (** Returns the position of the iterator *)
  val pos : iterator -> int    

  (** Text modifications *)
  (** [insert t0 n t] inserts [t] into the left of [n]-th character in
  [t0] *)
  val insert : t -> int -> t -> t option
  (** Raise invalid_arg "index out of bound" if [pos] is not contained in [t]*)
  val insert_exn : t -> int -> t -> t
  (** [delete t pos len] deletes length [len] text from [pos]-th
  character in [t] *)
  val delete : t -> pos:int -> len:int -> t
  (** [sub t pos len] obtains the substring of [t] which starts from
  [pos]-th character and has [len] characters *)
  val sub : t -> pos:int -> len:int -> t

  (** Fold *)
  val fold : t -> 'a -> ('a -> uchar -> 'a) -> 'a
end

type text = Text.t

module CharEncoding : sig

  module type Encoder = sig
    (** internal state of an encoder *) 
    type state

    (** Initial state *)    
    val init : state

    (** [encode ~repl state text] tries to encode [text].  If [text]
        contains a Unicode character which cannot be encoded, first
        tries to replace it by [repl].  If this still fails, it reports
        error and terminates. If [repl] is not supplied, [encode]
        fails at the first ocation of a Unicode character which
        cannot be encoded.*)
    val encode : ?repl:(uchar -> text) -> state -> text ->
      [`Success of state * string | `Error ]
      
    (** [terminate state] finalizes the encoder. *)
    val terminate : state -> string
  end


  module type Decoder = sig
      
    type state

    (** Initial state *)
    val init : state

    (** [decode state text] decodes [string] under the [state].  It
        returns a new state and a decoded text.  Replacement chacarter
        \0xfffd is used for the string which cannot be decoded. *)
    val decode : state -> string -> state * text

    val terminate : state -> text

  end

  (** Type of encoding *)
  type t = {name : string; 
            encoder : (module Encoder);
            decoder : (module Decoder)}

  (** aliase *)
  type enc = t

  (** Builtin encodings *)
  val ascii : enc
  val latin1 : enc
  val utf8 : enc
  val utf16be : enc
  val utf16le : enc
  val utf16 : enc
  val utf32be : enc
  val utf32le : enc
  val utf32 : enc

  (** [register f] registers [f] as a search method of [enc].  [f]
      takes an encoding name as an argument, then returns [enc]. *)
  val register : (string -> enc option) -> unit

  (** [of_name name] tries to find the encoding whose name is [name].*)
  val of_name : string -> enc option

  (** Universal encoder type *)
  type encoder

  (** [create_encoder ~repl enc] creates a new encoder of encoding [enc]
      using [repl] to escape [uchar].  If [repl] is not supplied,
      escaping is not done and an error is reported. *)
  val create_encoder : ?repl : (uchar -> text) -> enc -> encoder

  (** [fun u -> Text.of_latin1 (UChar.escape u)] *)
  val repl_escape : uchar -> text
(*  (** Escape as defined as XML *)
  val repl_xml : uchar -> text
*)
  (** [encode ~repl encoder text] tries to encode [text] using the [encoder].
      If [text] contains a Unicode character which cannot be encoded,
      encoder first tries to replace it by [repl].  If this still
      fails, it reports error and terminates.  *)
  val encode : encoder -> text -> [`Success of encoder * string | `Error ] 

  (** A trailing part of an encoded string. *)
  val terminate_encoder : encoder -> string

  (** Encode a text. *)
  val encode_text : ?repl : (uchar -> text) -> enc -> text -> 
    [ `Success of string | `Error ]

  (** Universal decoder type *)
  type decoder

  (** [create_decoder enc] creates a new encoder of encoding [enc].*)
  val create_decoder : enc -> decoder

  (** [encode encoder text] tries to encode [text] using the
      [encoder].  If it encounters characters which cannot be
      decoded, it replaces shortest such characters to 0xfffd and
      resume decoding after the such sequence. *)
  val decode: decoder -> string -> decoder * text

  (** A trailing part of a decoded text. *)
  val terminate_decoder : decoder -> text

  (** Decode a string *)
  val decode_string : enc -> string -> text

  (** Code conversion *)
  val recode : ?repl : (uchar -> text) -> enc -> string -> enc ->
    [ `Success of string | `Error ]
end
