(** ucorelib : a feather weight Unicode library for OCaml *)

(* Copyright (C) 2011, 2013 Yamagata Yoriyuki. *)

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
(* yoriyuki.y@gmail.com *)


(** Unicode (ISO-UCS) characters.

   This module implements Unicode characters.
*)

(* Copyright (C) 2002, 2003, 2004 Yamagata Yoriyuki. *)

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
(* yoriyuki.y@gmail.com *)

exception Out_of_range
exception Malformed_code

module UChar = struct 
  type t = int
      
  external code : t -> int = "%identity"
      
  let char_of c = 
    if c >= 0 && c < 0x100 then Char.chr c else raise Out_of_range
      
  let of_char = Char.code
      
(* valid range: U+0000..U+D7FF and U+E000..U+10FFFF *)
  let chr n =
    if (n >= 0 && n <= 0xd7ff) or (n >= 0xe000 && n <= 0x10ffff) 
    then n 
    else raise Out_of_range

  let unsafe_chr n = n

  let eq (u1 : t) (u2 : t) = u1 = u2
      
  let compare u1 u2 = u1 - u2
      
  type uchar = t
	
  let int_of u = code u
  let of_int n = chr n

  let escape u = 
    let n = code u in
    if u <= 0xffff then 
      Printf.sprintf "\\u%04x" n
    else
      Printf.sprintf "\\U%08x" n

end

type uchar = UChar.t

(** UTF-8 encoded Unicode strings. The type is normal string. *)

(* Copyright (C) 2002, 2003 Yamagata Yoriyuki.  *)

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
(* yoriyuki.y@gmail.com *)

module UTF8 = struct 
  type t = string

  let empty = ""

  type index = int

  let look s i =
    let n' =
      let n = Char.code (String.unsafe_get s i) in
      if n < 0x80 then n else
      if n <= 0xdf then
	(n - 0xc0) lsl 6 lor (0x7f land (Char.code (String.unsafe_get s (i + 1))))
      else if n <= 0xef then
	let n' = n - 0xe0 in
	let m = Char.code (String.unsafe_get s (i + 1)) in
	let n' = n' lsl 6 lor (0x7f land m) in
	let m = Char.code (String.unsafe_get s (i + 2)) in
	n' lsl 6 lor (0x7f land m)
      else
	let n' = n - 0xf0 in
	let m = Char.code (String.unsafe_get s (i + 1)) in
	let n' = n' lsl 6 lor (0x7f land m) in
	let m = Char.code (String.unsafe_get s (i + 2)) in
	let n' = n' lsl 6 lor (0x7f land m) in
	let m = Char.code (String.unsafe_get s (i + 3)) in
	n' lsl 6 lor (0x7f land m)     
    in
    UChar.unsafe_chr n'
      
  let next s i = 
    let n = Char.code s.[i] in
    if n < 0x80 then i + 1 else
    if n <= 0xdf then i + 2
    else if n <= 0xef then i + 3
    else i + 4
	
  let rec search_head_backward s i =
    if i < 0 then -1 else
    let n = Char.code s.[i] in
    if n < 0x80 || n >= 0xc2 then i else
    search_head_backward s (i - 1)
      
  let prev s i = search_head_backward s (i - 1)
      
  let move s i n =
    if n >= 0 then
      let rec loop i n = if n <= 0 then i else loop (next s i) (n - 1) in
      loop i n
    else
      let rec loop i n = if n >= 0 then i else loop (prev s i) (n + 1) in
      loop i n
	
  let rec nth_aux s i n =
    if n = 0 then i else
    nth_aux s (next s i) (n - 1)
      
  let nth s n = nth_aux s 0 n
      
  let first _ = 0
      
  let last s = search_head_backward s (String.length s - 1)
      
  let out_of_range s i = i < 0 || i >= String.length s
    
  let compare_index _ i j = i - j
      
  let get s n = look s (nth s n)
      
  let add_uchar buf u =
    let masq = 0b111111 in
    let k = UChar.code u in
    if k <= 0x7f then
      Buffer.add_char buf (Char.unsafe_chr k)
    else if k <= 0x7ff then begin
      Buffer.add_char buf (Char.unsafe_chr (0xc0 lor (k lsr 6)));
      Buffer.add_char buf (Char.unsafe_chr (0x80 lor (k land masq)))
    end else if k <= 0xffff then begin
      Buffer.add_char buf (Char.unsafe_chr (0xe0 lor (k lsr 12)));
      Buffer.add_char buf (Char.unsafe_chr (0x80 lor ((k lsr 6) land masq)));
      Buffer.add_char buf (Char.unsafe_chr (0x80 lor (k land masq)));
    end else  begin
      Buffer.add_char buf (Char.unsafe_chr (0xf0 + (k lsr 18)));
      Buffer.add_char buf (Char.unsafe_chr (0x80 lor ((k lsr 12) land masq)));
      Buffer.add_char buf (Char.unsafe_chr (0x80 lor ((k lsr 6) land masq)));
      Buffer.add_char buf (Char.unsafe_chr (0x80 lor (k land masq)));
    end
	
  let init len f =
    let buf = Buffer.create len in
    for c = 0 to len - 1 do add_uchar buf (f c) done;
    Buffer.contents buf

  let make len u = init len (fun _ -> u)

  let of_char u = make 1 u

  let of_string_unsafe s = s 
  let to_string_unsafe s = s

  let rec distance_aux s i j c =
    if i >= j then c else
    let n = Char.code (String.unsafe_get s i) in
    let k =
      if n < 0x80 then 1 else
      if n < 0xe0 then 2 else
      if n < 0xf0 then 3 else 4
    in
    distance_aux s (i + k) j (c + 1)

  let distance s i j = distance_aux s i j 0

  let length s = distance s 0 (String.length s)
      
  let rec iter_aux proc s i =
    if i >= String.length s then () else
    let u = look s i in
    proc u;
    iter_aux proc s (next s i)
      
  let iter proc s = iter_aux proc s 0

  let rec iteri_aux f s i count =
    if i >= String.length s then () else
    let u = look s i in
    f u count;
    iteri_aux f s (next s i) (count + 1)
      
  let iteri f s = iteri_aux f s 0 0

  let compare s1 s2 = String.compare s1 s2

  let sub s n len =
    let ipos = move s (first s) n in
    let jpos = move s ipos len in
    String.sub s ipos (jpos-ipos)
      
  let validate s =
    let rec trail c i a =
      if c = 0 then a else
      if i >= String.length s then raise Malformed_code else
      let n = Char.code (String.unsafe_get s i) in
      if n < 0x80 || n >= 0xc0 then raise Malformed_code else
      trail (c - 1) (i + 1) (a lsl 6 lor (0x7f land n)) in
    let rec main i =
      if i >= String.length s then () else
      let n = Char.code (String.unsafe_get s i) in
      if n < 0x80 then main (i + 1) else
      if n < 0xc2 then raise Malformed_code else
      if n <= 0xdf then 
	if trail 1 (i + 1) (n - 0xc0) < 0x80 then raise Malformed_code else 
	main (i + 2)
      else if n <= 0xef then 
	let n' = trail 2 (i + 1) (n - 0xe0) in
	if n' < 0x800 then raise Malformed_code else
	if n' >= 0xd800 && n' <= 0xdfff then raise Malformed_code else
	main (i + 3)
      else if n <= 0xf4 then 
	let n = trail 3 (i + 1) (n - 0xf0) in
	if n < 0x10000 or n > 0x10FFFF then raise Malformed_code else
	main (i + 4)
      else raise Malformed_code in
    main 0

  let of_ascii s =
    for i = 0 to String.length s - 1 do
      if Char.code s.[i] >= 0x80 then raise Malformed_code;
    done;
    String.copy s

  let of_latin1 s = init (String.length s) (fun i -> UChar.of_char s.[i])
      
  module Buf = 
    struct
      include Buffer
      type buf = t
      let add_char = add_uchar
    end

  let map f us = 
    let b = Buf.create (length us) in
    iter (fun c -> Buf.add_char b (f c)) us;
    Buf.contents b

  let filter_map f us = 
    let b = Buf.create (length us) in
    iter (fun c -> match f c with None -> () | Some c -> Buf.add_char b c) us;
    Buf.contents b

  let filter p us = 
    let b = Buf.create (length us) in
    iter (fun c -> if p c then Buf.add_char b c) us;
    Buf.contents b

  let fold f a s =
    let rec loop a i =
      if out_of_range s i then a else
      let a' = f a (look s i) in
      loop a' (next s i) in
    loop a 0

  let escaped = String.escaped

    module ByteIndex : sig
      type t = string
      type b_idx(* = private int*)
      type char_idx = int
      val of_int_unsafe : int -> b_idx
      val to_int : b_idx -> int
      val next : t -> b_idx -> b_idx
      val prev : t -> b_idx -> b_idx
      val of_char_idx : t -> char_idx -> b_idx
      val at_end : t -> b_idx -> bool
      val out_of_range : t -> b_idx -> bool
      val first : b_idx
      val last : t -> b_idx
      val move : t -> b_idx -> int -> b_idx
      val look : t -> b_idx -> UChar.t
    end = struct
      type t = string
      type b_idx = int
      type char_idx = int
      external of_int_unsafe : int -> b_idx = "%identity"
      external to_int : b_idx -> int = "%identity"
      let look = look
      let next = next
      let prev = prev
      let first = 0
      let last us = prev us (String.length us)
      let at_end us bi = bi = String.length us
      let out_of_range us bi = bi < 0 || bi >= String.length us
      let move us bi n = (* faster moving positive than negative n *)
        let bi = ref bi in
        let step = if n > 0 then next else prev in
        for j = 1 to abs n do bi := step us !bi done;
        !bi
      let of_char_idx us ci = move us first ci
    end

  (* Could be improved. *)
  let rindex us ch =
    let rec aux ci bi =
      if ByteIndex.out_of_range us bi then raise Not_found;
      if ByteIndex.look us bi = ch then ci
      else aux (ci-1) (ByteIndex.prev us bi)
    in
    aux 0 (ByteIndex.last us)

  let rec contains_aux step bi us ch =
    if ByteIndex.out_of_range us bi then false
    else if ByteIndex.look us bi = ch then true
    else contains_aux step (step us bi) us ch

  let contains us ch = contains_aux ByteIndex.next ByteIndex.first us ch

end

(* Interface to String used for Rope implementation *)
module type BaseStringType = sig
  type t

  val empty : t
  (* [create n] creates [n]-bytes base string *)
  val create : int -> t
  val init : int -> (int -> uchar) -> t
  val of_string : string -> t option
  val of_string_unsafe : string -> t
  val of_ascii : string -> t option

  val length : t -> int
  val compare : t -> t -> int

  type index 
  val first : t -> index
  val end_pos : t -> index
  val out_of_range : t -> index -> bool
  val next : t -> index -> index
  val prev : t -> index -> index
  val move : t -> index -> int -> index
  val equal_index : t -> index -> index -> bool
  val compare_index : t -> index -> index -> int
  val distance : t -> index -> index -> int

  (* Low level functions *)
  (* bytes of substring *)
  val size : t -> index -> index -> int
  (* [blit s1 i1 i2 s2 j] copies the contents of [s1] from [i1] to
  [i2] into the location [j] of [s2]. *)
  val blit : t -> index -> index -> t -> index -> unit
  (* [move_by_bytes s i x] moves index [i] by [x] bytes.*)
  val move_by_bytes : t -> index -> int -> index

  val add_substring : Buffer.t -> t -> index -> index -> unit

  val read : t -> index -> uchar
  val write : t -> index -> uchar -> index
end

module BaseString : BaseStringType = struct
  include UTF8

  let empty = ""

  let is_valid s = try validate s; true with Malformed_code -> false

  let copy = String.copy
  let read = look
  let append = (^)
  let create = String.create
  let end_pos s = String.length s
  let of_string s = if is_valid s then Some (String.copy s) else None 
  let of_string_unsafe s = s
  let of_ascii s = try Some (UTF8.of_ascii s) with Malformed_code -> None
  let equal_index s i j = i = j
  let size s i j = j - i
  let blit s1 i1 i2 s2 j = String.blit s1 i1 s2 j (i2 - i1)
  let move_by_bytes s i x = i + x
  let add_substring b s i j = Buffer.add_substring b s i (j - i)
 
  let write s i u =
    let masq = 0b111111 in
    let k = UChar.code u in
    if k <= 0x7f then
      if i >= String.length s then i else begin
	s.[i] <- Char.unsafe_chr k;
	i + 1
      end
    else if k <= 0x7ff then
      if i >= String.length s - 1 then i else begin
	s.[i] <- Char.unsafe_chr (0xc0 lor (k lsr 6));
	s.[i+1] <- (Char.unsafe_chr (0x80 lor (k land masq)));
	i+2
    end else if k <= 0xffff then
      if i >= String.length s - 2 then i else begin
	s.[i] <- Char.unsafe_chr (0xe0 lor (k lsr 12));
	s.[i+1] <- Char.unsafe_chr (0x80 lor ((k lsr 6) land masq));
	s.[i+2] <- Char.unsafe_chr (0x80 lor (k land masq));
	i + 3
    end else 
      if i >= String.length s - 3 then i else
      begin
	s.[i] <- Char.unsafe_chr (0xf0 + (k lsr 18));
	s.[i+1] <- Char.unsafe_chr (0x80 lor ((k lsr 12) land masq));
	s.[i+2] <- Char.unsafe_chr (0x80 lor ((k lsr 6) land masq));
	s.[i+3] <- Char.unsafe_chr (0x80 lor (k land masq));
	i + 4
      end
end

module Text' = struct
(* 
 * Rope: Rope: an implementation of the data structure described in
 *   
 * Boehm, H., Atkinson, R., and Plass, M. 1995. Ropes: an alternative to
 * strings. Softw. Pract. Exper. 25, 12 (Dec. 1995), 1315-1330.
 * 
 * Motivated by Luca de Alfaro's extensible array implementation Vec.
 * 
 * Copyright (C) 2013 Yoriyuki Yamagata <yoriyuki.y@gmail.com>
 * Copyright (C) 2007 Mauricio Fernandez <mfp@acm.org>
 * Copyright (C) 2008 Edgar Friendly <thelema314@gmail.com>
 * Copyright (C) 2008 David Teller, LIFO, Universite d'Orleans
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

  open BatInt.Safe_int

  module B = BaseString

  let int_max (x:int) (y:int) = if x < y then y else x
  let int_min (x:int) (y:int) = if x < y then x else y

  type base_string = {mutable s : B.t; mutable unused : B.index} 
	
  type t = 
      Empty
    | Concat of node
    | Leaf of leaf
  and node = {left : t; left_length : int; right : t; right_length : int; height : int}
  and leaf = {b : base_string; 
	      i : B.index;  (*..............:::::::::::::::::::::.... *)
	      j : B.index;  (*              ^                    ^    *)
	      len : int}    (*              i                    j    *)

  let empty = Empty

  let length = function
      Empty -> 0
    | Concat node -> node.left_length + node.right_length
    | Leaf leaf -> leaf.len
	  
  let height = function
      Empty -> 0
    | Concat node -> node.height
    | Leaf _ -> 1

	  (* 48 limits max rope size to 220GB on 64 bit,
	   * ~ 700MB on 32bit (length fields overflow after that) *)
  let max_height = 48
      
  let leaf_size = 256 (* bytes *)

  let make_concat l r =
    let hl = height l and hr = height r in
    let cl = length l and cr = length r in
    Concat {left = l; left_length = cl; 
	    right = r; right_length = cr;
	    height = max hl hr}

  let min_len =
    let fib_tbl = Array.make max_height 0 in
    let rec fib n = match fib_tbl.(n) with
      0 ->
        let last = fib (n - 1) and prev = fib (n - 2) in
        let r = last + prev in
        let r = if r > last then r else last in (* check overflow *)
        fib_tbl.(n) <- r; r
    | n -> n
    in
    fib_tbl.(0) <- leaf_size + 1; fib_tbl.(1) <- 3 * leaf_size / 2 + 1;
    Array.init max_height (fun i -> if i = 0 then 1 else fib (i - 1))
      
  let max_length = min_len.(Array.length min_len - 1)
      
  let make_concat l r =
    let hl = height l and hr = height r in
    let cl = length l and cr = length r in
    Concat {left = l; left_length = cl; right = r; right_length = cr; 
	    height = 1 + int_max hl hr}

  let concat_fast l r = match l with
    Empty -> r
  | Leaf _ | Concat _ ->
      match r with
        Empty -> l
      | Leaf _ | Concat _ -> make_concat l r
	    
	    (* based on Hans-J. Boehm's *)
  type forest_element = { mutable c : t; mutable rlen : int }

  let not_empty = function Empty -> false | _ -> true

  let add_forest forest rope len =
    let i = ref 0 in
    let sum = ref empty in
    while len > min_len.(!i+1) do
      if not_empty forest.(!i).c then begin
        sum := concat_fast forest.(!i).c !sum;
        forest.(!i).c <- Empty
      end;
      incr i
    done;
    sum := concat_fast !sum rope;
    let sum_len = ref (length !sum) in
    while !sum_len >= min_len.(!i) do
      if not_empty forest.(!i).c then begin
        sum := concat_fast forest.(!i).c !sum;
        sum_len := !sum_len + forest.(!i).rlen;
        forest.(!i).c <- Empty;
      end;
      incr i
    done;
    decr i;
    forest.(!i).c <- !sum;
    forest.(!i).rlen <- !sum_len
	
  let concat_forest forest =
    Array.fold_left (fun s x -> concat_fast x.c s) Empty forest
      
  let rec balance_insert rope len forest = match rope with
    Empty -> ()
  | Leaf _ -> add_forest forest rope len
  | Concat node when node.height >= max_height || len < min_len.(node.height) ->
      balance_insert node.left node.left_length forest;
      balance_insert node.right node.right_length forest
  | x -> add_forest forest x len (* function or balanced *)
	
  let balance r =
    if height r < max_height then r else
    match r with
      Empty | Leaf _ -> r
    | _ ->
        let forest = Array.init max_height (fun _ -> {c = Empty; rlen = 0}) in
        balance_insert r (length r) forest;
        concat_forest forest

  let bal_if_needed l r =
    let r = make_concat l r in balance r
      
  let is_full_tail leaf = B.equal_index leaf.b.s leaf.j leaf.b.unused

  let leaf_append leaf_l leaf_r =
    let size_l = B.size leaf_l.b.s leaf_l.i leaf_l.j in
    let size_r = B.size leaf_r.b.s leaf_r.i leaf_r.j in
    if size_l + size_r <= leaf_size then begin
      if size_l + size_r <= 
	B.size leaf_l.b.s (B.first leaf_l.b.s) (B.end_pos leaf_l.b.s)
	  && is_full_tail leaf_l 
      then begin
	B.blit leaf_r.b.s leaf_r.i leaf_r.j leaf_l.b.s leaf_l.b.unused;
      end else begin
	let s = B.create leaf_size in
	B.blit leaf_l.b.s leaf_l.i leaf_l.j s (B.first s);
	B.blit leaf_r.b.s leaf_r.i leaf_r.j s (B.move_by_bytes s (B.first s) size_l);
	leaf_l.b.s <- s;
      end;
      leaf_l.b.unused <- B.move_by_bytes leaf_l.b.s leaf_l.b.unused size_r;
      let leaf = {leaf_l with j = leaf_l.b.unused;
		  len = leaf_l.len + leaf_r.len} in
      Leaf leaf
    end else
      make_concat (Leaf leaf_l) (Leaf leaf_r) (* height = 1 *)
	
  let concat_leaf l leaf_r = 
    match l with
    | Empty -> Leaf leaf_r
    | Leaf leaf_l -> leaf_append leaf_l leaf_r
    | Concat node ->
	match node.right with
	  Leaf leaf_l ->
	    Concat {node with right = leaf_append leaf_l leaf_r}
	| _ -> bal_if_needed l (Leaf leaf_r)
	      
  let append l = function
      Empty -> l
    | Leaf leaf_r -> concat_leaf l leaf_r
    | Concat node as r ->
	match node.left with
	  Leaf leaf_r ->
	    (match l with
	      Empty -> r
	    | Concat _ -> bal_if_needed l r
	    | Leaf leaf_l -> leaf_append leaf_l leaf_r)
	| _ -> 
	    match l with 
	      Empty -> r
	    | _ -> bal_if_needed l r

  let new_block_uchar u = 
    let s = B.create leaf_size in
    let i = B.write s (B.first s) u in
    let b = {s = s; unused = i} in
    Leaf {b = b; i = (B.first s); j = i; len = 1}

  let leaf_append_uchar leaf u =
    if is_full_tail leaf then begin
      let k = B.write leaf.b.s leaf.j u in
      if B.equal_index leaf.b.s k leaf.j then
	make_concat (Leaf leaf) (new_block_uchar u)
      else begin
	leaf.b.unused <- k;
	let leaf = {leaf with j = k; len = leaf.len + 1} in
	Leaf leaf
      end
    end else
      make_concat (Leaf leaf) (new_block_uchar u)
	
  let leaf_of_uchar u = 
    let s = B.of_string_unsafe (UTF8.make 1 u) in
    let b = {s = s; unused = B.end_pos s} in
    {b = b; i = B.first b.s; j = B.end_pos b.s; len = 1}

  let of_uchar u = Leaf (leaf_of_uchar u)
      
  let append_uchar l u = 
    match l with
    | Empty -> Leaf (leaf_of_uchar u)
    | Leaf leaf_l -> leaf_append_uchar leaf_l u
    | Concat node ->
	match node.right with
	  Leaf leaf_l ->
	    Concat {node with 
		    right = leaf_append_uchar leaf_l u;
		    right_length = node.right_length + 1}
	| _ -> bal_if_needed l (new_block_uchar u)

  let init len f =
    if len < 0 then failwith "Text.init: The length is minus" else
    let s = B.init len f in
    let b = {s = s; unused = B.end_pos s} in
    Leaf {b = b; i = B.first s; j = B.end_pos s; len = len}
      
  let of_string s =
    match B.of_string s with
      None -> None
    | Some s ->
	let b = {s = s; unused = B.end_pos s} in
	Some (Leaf {b = b; i = B.first b.s; j = B.end_pos b.s; len = B.length s})

  let of_string_exn s = 
    match of_string s with
      None -> raise Malformed_code
    | Some text -> text
	  
  let of_ascii s =
    match B.of_ascii s with
      None -> None
    | Some s ->
	let b = {s = s; unused = B.end_pos s} in
	Some (Leaf {b = b; i = B.first b.s; j = B.end_pos b.s; len = B.length s})

  let of_ascii_exn s = 
    match of_string s with
      None -> raise Malformed_code
    | Some text -> text

  let of_latin1 s = 
    init (String.length s) (fun i -> 
      UChar.unsafe_chr (Char.code s.[i]))

  let rec get t n =
    match t with
      Empty -> None
    | Leaf leaf ->
	if n >= leaf.len then None else
	let i = B.move leaf.b.s leaf.i n in
	Some (B.read leaf.b.s i)
    | Concat node ->
	if n < node.left_length then get node.left n else
	let n = n - node.left_length in
	get node.right n

  let rec get_exn t n =
    match t with
      Empty -> invalid_arg "index out of bounds"
    | Leaf leaf ->
	if n >= leaf.len then invalid_arg "index out of bounds" else
	let i = B.move leaf.b.s leaf.i n in
	B.read leaf.b.s i
    | Concat node ->
	if n < node.left_length then get_exn node.left n else
	let n = n - node.left_length in
	get_exn node.right n

  (* In Left (p, t), p is the path to the parent and t is the right sibling.  *)
  type path = Top | Left of path * t | Right of t * path 

  type iterator = {path : path; leaf : leaf; index : B.index}

  let empty_leaf = 
    let base = {s = B.empty; unused = B.end_pos B.empty} in
    {b = base; i = B.first B.empty; j = B.end_pos B.empty; len = 0} 

  let rec first_leaf_sub path = function
      Empty -> (Top, empty_leaf)
    | Leaf leaf -> (path, leaf)
    | Concat node ->
	first_leaf_sub (Left (path, node.right)) node.left
	    
  let first_leaf = first_leaf_sub Top

  let first t =
    let p, leaf = first_leaf t in
    {path = p; leaf = leaf; index = leaf.i}

  let rec end_leaf_sub path = function
      Empty -> (Top, empty_leaf)
    | Leaf leaf -> (path, leaf)
    | Concat node ->
	end_leaf_sub (Right (node.left, path)) node.right
	    
  let end_leaf = end_leaf_sub Top

  let last t =
    let p, leaf = end_leaf t in
    {path = p; leaf = leaf; index = B.prev leaf.b.s leaf.j}

  let rec nth_aux p t n =
    match t with
      Empty -> None
    | Leaf leaf ->
	if n >= leaf.len then None else
	let i = B.move leaf.b.s leaf.i n in
	Some {path = p; leaf = leaf; index = i}
    | Concat node ->
	if n < node.left_length then 
	  nth_aux (Left (p, node.right)) node.left n 
	else
	  let n = n - node.left_length in
	  nth_aux (Right (node.left, p)) node.right n

  let nth t n = nth_aux Top t n

  let rec nth_exn_aux p t n =
    match t with
      Empty -> invalid_arg "index out of bounds"
    | Leaf leaf ->
	if n >= leaf.len then invalid_arg "index out of bounds" else
	let i = B.move leaf.b.s leaf.i n in
        {path = p; leaf = leaf; index = i}
    | Concat node ->
	if n < node.left_length then 
	  nth_exn_aux (Left (p, node.right)) node.left n 
	else
	  let n = n - node.left_length in
	  nth_exn_aux (Right (node.left, p)) node.right n

  let nth_exn t n = nth_exn_aux Top t n

  let rec next_leaf = function
      Top -> None
    | Left (p, t) ->
	Some (first_leaf_sub p t)
    | Right (t, p) ->
	next_leaf p

  let next it =
    let i = B.next it.leaf.b.s it.index in
    if not (B.equal_index it.leaf.b.s i it.leaf.j) then 
      Some {it with index = i} 
    else match next_leaf it.path with 
      None -> None
    | Some (path, leaf) -> 
	Some {path = path; leaf = leaf; index = leaf.i}

  let next_exn it =
    let i = B.next it.leaf.b.s it.index in
    if not (B.equal_index it.leaf.b.s i it.leaf.j) then 
      {it with index = i} 
    else match next_leaf it.path with 
      None -> invalid_arg "index out of bounds"
    | Some (path, leaf) -> 
        {path = path; leaf = leaf; index = leaf.i}

  let rec prev_leaf = function
      Top -> None
    | Left (p, t) ->
	prev_leaf p
    | Right (t, p) ->
	Some (end_leaf_sub p t)

  let prev it =
    if not (B.equal_index it.leaf.b.s it.index it.leaf.i) then 
      let i = B.prev it.leaf.b.s it.index in
      Some {it with index = i} 
    else match prev_leaf it.path with 
      None -> None
    | Some (path, leaf) -> 
	Some {path = path; leaf = leaf; index = B.prev leaf.b.s leaf.j}

  let prev_exn it =
    if not (B.equal_index it.leaf.b.s it.index it.leaf.i) then 
      let i = B.prev it.leaf.b.s it.index in
      {it with index = i} 
    else match prev_leaf it.path with 
      None -> invalid_arg "index out of	bounds"
    | Some (path, leaf) -> 
	{path = path; leaf = leaf; index = B.prev leaf.b.s leaf.j}

  let rec base_aux path sub =
    match path with
      Top -> sub
    | Left (p, t) ->
	base_aux p (make_concat sub t)
    | Right (t, p) ->
	base_aux p (make_concat t sub)

  let base it = balance (base_aux it.path (Leaf it.leaf))

  let rec pos_path = function
      Top -> 0
    | Left (p, t) -> pos_path p
    | Right (t, p) -> length t

  let pos it = pos_path it.path + B.distance it.leaf.b.s it.leaf.i it.index 

  let move_ahead_leaf it n =
    let rec loop i n =
      if B.compare_index it.leaf.b.s i it.leaf.j > 0 then 
	`Out_of_range ({it with index = it.leaf.j}, n) 
      else if n <= 0 then `Success {it with index = i} else 
      loop (B.next it.leaf.b.s i) (n - 1) in
    loop it.index n
        
  let rec move_ahead path sub it n =
    match sub with
      Empty -> `Out_of_range (it, n)
    | Leaf leaf -> 
	let it = 
	  if it.leaf == leaf then it else
	  {path = path; leaf = leaf; index = leaf.i} in
       	(match move_ahead_leaf it n with
	  `Success it as ret -> ret
	| `Out_of_range (it, n) -> 
	    (match path with
	      Top -> `Out_of_range (it, n)
	    | Left (p, t) ->
		move_ahead (Right (sub, p)) t it n
	    | Right (t, p) ->
		let node = make_concat t sub in
		move_ahead p node it n))
    | Concat node ->
	if node.left_length >= n then 
	  move_ahead (Left (path, node.right)) node.left it n
	else
	  move_ahead (Right (node.left, path)) node.right it (n - node.left_length)

  let move_behind_leaf it n =
    let rec loop i n =
      if B.compare_index it.leaf.b.s i it.leaf.i < 0 then 
	`Out_of_range ({it with index = it.leaf.i}, n) 
      else if n <= 0 then `Success {it with index = i} else 
      loop (B.prev it.leaf.b.s i) (n-1) in
    loop it.index n
        
  let rec move_behind path sub it n =
    match sub with
      Empty -> `Out_of_range (it, n)
    | Leaf leaf -> 
	let it = 
	  if it.leaf == leaf then it else
	  {path = path; leaf = leaf; index = leaf.j} in
       	(match move_behind_leaf it n with
	  `Success it as ret -> ret
	| `Out_of_range (it, n) -> 
	    (match path with
	      Top -> `Out_of_range (it, n)
	    | Right (t, p) ->
		move_behind (Left (p, sub)) t it n
	    | Left (p, t) ->
		let node = make_concat sub t in
		move_behind p node it n))
    | Concat node ->
	if node.right_length >= n then 
	  move_behind (Right (node.left, path)) node.right it n
	else
	  move_ahead (Right (node.right, path)) node.left it (n - node.left_length)

  let move it n =
    if n > 0 then move_ahead it.path (Leaf it.leaf) it n else
    if n < 0 then move_behind it.path (Leaf it.leaf) it (-n) else
    `Success it 

  let move_exn it n =
    match move it n with
      `Success it -> it
    | `Out_of_range _ -> invalid_arg "number out of bounds"

  let move_as_possible it n =
    match move it n with
      `Success it -> it
    | `Out_of_range (it, n) -> it

  let rec delete_left_pos path sub =
    match path with
      Top -> (path, sub)
    | Left (p, t) -> delete_left_pos p t
    | Right (t, p) -> (p, sub)

  let delete_left it =
    let p, _ = delete_left_pos it.path (Leaf it.leaf) in
    let leaf = {it.leaf with i = it.index; 
		len = B.distance it.leaf.b.s it.index it.leaf.j} in
    let it = {it with path = p; leaf = leaf} in
    let b = base it in
    first b

  let rec delete_right_pos path sub =
    match path with
      Top -> (path, sub)
    | Right (t, p) -> delete_right_pos p t
    | Left (p, t) -> (p, sub)

  let delete_right it =
    let p, _ = delete_right_pos it.path (Leaf it.leaf) in
    let leaf = {it.leaf with j = it.index; 
		len = B.distance it.leaf.b.s it.leaf.i it.index} in
    let it = {it with path = p; leaf = leaf} in
    let b = base it in
    last b

  let sub it len =
    let it = delete_left it in
    match move it len with
      `Out_of_range _ -> None
    | `Success it ->
	Some (delete_right it)

  let sub_exn it len =
    match sub it len with
      None -> invalid_arg "iterator out of bound"
    | Some it -> it

  let insert it text =
    let n = pos it in
    let left = base (delete_right it) in 
    let right = base (delete_left it) in
    let base = (append (append left text) right) in
    match nth base n with 
      None -> assert false
    | Some it -> it 

  let value it = B.read it.leaf.b.s it.index

  let fold_leaf leaf a f =
    let rec loop a i =
      if B.compare_index leaf.b.s i leaf.j >= 0 then a else
      let a' = f a (B.read leaf.b.s i) in
      loop a' (B.next leaf.b.s i) in
    loop a leaf.i

  let rec fold t a f = 
    match t with
      Empty -> a
    | Leaf leaf -> fold_leaf leaf a f
    | Concat node ->
	let a' = fold node.left a f in
	fold node.right a' f

  let rec compare_iterator it1 it2 =
    let u1 = value it1 in
    let u2 = value it2 in
    let sgn = UChar.compare u1 u2 in
    if sgn <> 0 then sgn else
    match next it1, next it2 with
      None, None -> 0
    | None, _ -> -1
    | _, None -> 1
    | Some it1, Some i2 -> compare_iterator it1 it2

  let compare t1 t2 = compare_iterator (first t1) (first t2)

  let rec string_of_aux b = function
      Empty -> ()
    | Leaf leaf ->
	B.add_substring b leaf.b.s leaf.i leaf.j
    | Concat node ->
	string_of_aux b node.left;
	string_of_aux b node.right

  let string_of t =
    let b = Buffer.create 0 in
    string_of_aux b t;
    Buffer.contents b
    
end

(* 
 * Rope: Rope: an implementation of the data structure described in
 *   
 * Boehm, H., Atkinson, R., and Plass, M. 1995. Ropes: an alternative to
 * strings. Softw. Pract. Exper. 25, 12 (Dec. 1995), 1315-1330.
 * 
 * Motivated by Luca de Alfaro's extensible array implementation Vec.
 * 
 * Copyright (C) 2007 Mauricio Fernandez <mfp@acm.org>
 * Copyright (C) 2008 Edgar Friendly <thelema314@gmail.com>
 * Copyright (C) 2008 David Teller, LIFO, Universite d'Orleans
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

module Text = struct 
  
  (**Low-level optimization*)
  let int_max (x:int) (y:int) = if x < y then y else x
  let int_min (x:int) (y:int) = if x < y then x else y

  let splice s1 off len s2 =
    let len1 = String.length s1 and len2 = String.length s2           in
    let off  = if off < 0 then len1 + off - 1 else off  in
    let len  = int_min (len1 - off) len                 in
    let out_len = len1 - len + len2                     in
    let s = String.create out_len in
    String.blit s1 0 s 0 off; (* s1 before splice point *)
    String.blit s2 0 s off len2; (* s2 at splice point *)
    String.blit s1 (off+len) s (off+len2) (len1 - (off+len)); (* s1 after off+len *)
    s

  exception Invalid_rope
      
  type t =
      Empty                             (**An empty rope*)
    | Concat of t * int * t * int * int (**[Concat l ls r rs h] is the concatenation of
                                           ropes [l] and [r], where [ls] is the total 
  					   length of [l], [rs] is the length of [r]
  					   and [h] is the height of the node in the
  					   tree, used for rebalancing. *)
    | Leaf of int * UTF8.t              (**[Leaf l t] is string [t] with length [l],
  					   measured in number of Unicode characters.*)
	  
  type forest_element = { mutable c : t; mutable len : int }
	
  let str_append = (^)
  let empty_str = ""
  let string_of_string_list l = String.concat empty_str l


      
      (* 48 limits max rope size to 220GB on 64 bit,
       * ~ 700MB on 32bit (length fields overflow after that) *)
  let max_height = 48
      
      (* actual size will be that plus 1 word header;
       * the code assumes it's an even num.
       * 256 gives up to a 50% overhead in the worst case (all leaf nodes near
       * half-filled *)
  let leaf_size = 256 (* utf-8 characters, not bytes *)
      (* =end *)
      
      (* =begin code *)
      
  exception Out_of_bounds
      
  let empty = Empty
      


      (* by construction, there cannot be Empty or Leaf "" leaves *)
  let is_empty = function Empty -> true | _ -> false
      
  let height = function
      Empty | Leaf _ -> 0
    | Concat(_,_,_,_,h) -> h
	  
  let length = function
      Empty -> 0
    | Leaf (l,_) -> l
    | Concat(_,cl,_,cr,_) -> cl + cr
	  
  let make_concat l r =
    let hl = height l and hr = height r in
    let cl = length l and cr = length r in
    Concat(l, cl, r, cr, if hl >= hr then hl + 1 else hr + 1)
      
  let min_len =
    let fib_tbl = Array.make max_height 0 in
    let rec fib n = match fib_tbl.(n) with
      0 ->
        let last = fib (n - 1) and prev = fib (n - 2) in
        let r = last + prev in
        let r = if r > last then r else last in (* check overflow *)
        fib_tbl.(n) <- r; r
    | n -> n
    in
    fib_tbl.(0) <- leaf_size + 1; fib_tbl.(1) <- 3 * leaf_size / 2 + 1;
    Array.init max_height (fun i -> if i = 0 then 1 else fib (i - 1))
      
  let max_length = min_len.(Array.length min_len - 1)
      
  let concat_fast l r = match l with
    Empty -> r
  | Leaf _ | Concat(_,_,_,_,_) ->
      match r with
        Empty -> l
      | Leaf _ | Concat(_,_,_,_,_) -> make_concat l r
	    
	    (* based on Hans-J. Boehm's *)
  let add_forest forest rope len =
    let i = ref 0 in
    let sum = ref empty in
    while len > min_len.(!i+1) do
      if forest.(!i).c <> Empty then begin
        sum := concat_fast forest.(!i).c !sum;
        forest.(!i).c <- Empty
      end;
      incr i
    done;
    sum := concat_fast !sum rope;
    let sum_len = ref (length !sum) in
    while !sum_len >= min_len.(!i) do
      if forest.(!i).c <> Empty then begin
        sum := concat_fast forest.(!i).c !sum;
        sum_len := !sum_len + forest.(!i).len;
        forest.(!i).c <- Empty;
      end;
      incr i
    done;
    decr i;
    forest.(!i).c <- !sum;
    forest.(!i).len <- !sum_len
	
  let concat_forest forest =
    Array.fold_left (fun s x -> concat_fast x.c s) Empty forest
      
  let rec balance_insert rope len forest = match rope with
    Empty -> ()
  | Leaf _ -> add_forest forest rope len
  | Concat(l,cl,r,cr,h) when h >= max_height || len < min_len.(h) ->
      balance_insert l cl forest;
      balance_insert r cr forest
  | x -> add_forest forest x len (* function or balanced *)
	
  let balance r =
    match r with
      Empty | Leaf _ -> r
    | _ ->
        let forest = Array.init max_height (fun _ -> {c = Empty; len = 0}) in
        balance_insert r (length r) forest;
        concat_forest forest
	  
  let bal_if_needed l r =
    let r = make_concat l r in
    if height r < max_height then r else balance r
      
  let concat_str l = function
      Empty | Concat(_,_,_,_,_) -> invalid_arg "concat_str"
    | Leaf (lenr, rs) as r ->
        match l with
        | Empty -> r
        | Leaf (lenl, ls) ->
            let slen = lenr + lenl in
            if slen <= leaf_size then Leaf ((lenl+lenr),(str_append ls rs))
            else make_concat l r (* height = 1 *)
        | Concat(ll, cll, Leaf (lenlr ,lrs), clr, h) ->
            let slen = clr + lenr in
            if clr + lenr <= leaf_size then
              Concat(ll, cll, Leaf ((lenlr + lenr),(str_append lrs rs)), slen, h)
            else
              bal_if_needed l r
        | _ -> bal_if_needed l r
	      
  let append_char c r = concat_str r (Leaf (1, (UTF8.make 1 c)))

  let append l = function
      Empty -> l
    | Leaf _ as r -> concat_str l r
    | Concat(Leaf (lenrl,rls),rlc,rr,rc,h) as r ->
        (match l with
          Empty -> r
        | Concat(_,_,_,_,_) -> bal_if_needed l r
        | Leaf (lenl, ls) ->
            let slen = rlc + lenl in
            if slen <= leaf_size then
              Concat(Leaf((lenrl+lenl),(str_append ls rls)), slen, rr, rc, h)
            else
              bal_if_needed l r)
    | r -> (match l with Empty -> r | _ -> bal_if_needed l r)
	  
  let ( ^^^ ) = append

  let prepend_char c r = append (Leaf (1,(UTF8.make 1 c))) r
      
  let get r i = 
    let rec aux i = function
	Empty -> raise Out_of_bounds
      | Leaf (lens, s) ->
          if i >= 0 && i < lens then UTF8.get s i
          else raise Out_of_bounds
      | Concat (l, cl, r, cr, _) ->
          if i < cl then aux i l
          else aux (i - cl) r
    in aux i r
      
  let copy_set us cpos c =
    let ipos = UTF8.ByteIndex.of_char_idx us cpos in 
    let jpos = UTF8.ByteIndex.next us ipos in
    let i = UTF8.ByteIndex.to_int ipos
    and j = UTF8.ByteIndex.to_int jpos in
    splice us i (j-i) (UTF8.of_char c)
      
  let set r i v = 
    let rec aux i = function
        Empty -> raise Out_of_bounds
      | Leaf (lens, s) ->
  	  if i >= 0 && i < lens then
  	    let s = copy_set s i v in
            Leaf (lens, s)
  	  else raise Out_of_bounds
      | Concat(l, cl, r, cr, _) ->
  	  if i < cl then append (aux i l) r
  	  else append l (aux (i - cl) r)
    in aux i r


  module Iter =
    struct


      (* Iterators are used for iterating efficiently over multiple ropes
	 at the same time *)

      type iterator = {
	  mutable leaf : UTF8.t;
	  (* Current leaf in which the iterator is *)
	  mutable idx : UTF8.ByteIndex.b_idx;
	  (* Current byte position of the iterator *)
	  mutable rest : t list;
	  (* Ropes not yet visited *)
	}

      type t = iterator option

	    (* Initial iterator state: *)
      let make rope = { leaf = UTF8.empty;
			idx = UTF8.ByteIndex.first;
			rest = if rope = Empty then [] else [rope] }

      let rec next_leaf = function
	| Empty :: l ->
            next_leaf l
	| Leaf(len, str) :: l ->
            Some(str, l)
	| Concat(left, left_len, right, right_len, height) :: l ->
            next_leaf (left :: right :: l)
	| [] ->
            None

	      (* Advance the iterator to the next position, and return current
		 character: *)
      let rec next iter =
	if UTF8.ByteIndex.at_end iter.leaf iter.idx then
          (* We are at the end of the current leaf, find another one: *)
          match next_leaf iter.rest with
          | None ->
              None
          | Some(leaf, rest) ->
	      if leaf = "" then None else begin
		iter.leaf <- leaf;
		iter.idx <- UTF8.ByteIndex.next leaf UTF8.ByteIndex.first;
		iter.rest <- rest;
		Some(UTF8.ByteIndex.look leaf UTF8.ByteIndex.first)
	      end
	else begin
          (* Just advance in the current leaf: *)
          let ch = UTF8.ByteIndex.look iter.leaf iter.idx in
          iter.idx <- UTF8.ByteIndex.next iter.leaf iter.idx;
          Some ch
	end
	    
	    (* Same thing but map leafs: *)
      let rec next_map f iter =
	if UTF8.ByteIndex.at_end iter.leaf iter.idx then
          match next_leaf iter.rest with
          | None ->
              None
          | Some(leaf, rest) ->
              let leaf = f leaf in
              iter.leaf <- leaf;
              iter.idx <- UTF8.ByteIndex.next leaf UTF8.ByteIndex.first;
              iter.rest <- rest;
              Some(UTF8.ByteIndex.look leaf UTF8.ByteIndex.first)
	else begin
          let ch = UTF8.ByteIndex.look iter.leaf iter.idx in
          iter.idx <- UTF8.ByteIndex.next iter.leaf iter.idx;
          Some ch
	end

	    (* Same thing but in reverse order: *)

      let rec prev_leaf = function
	| Empty :: l ->
            prev_leaf l
	| Leaf(len, str) :: l ->
            Some(str, l)
	| Concat(left, left_len, right, right_len, height) :: l ->
            prev_leaf (right :: left :: l)
	| [] ->
            None

      let prev iter =
	if iter.idx = UTF8.ByteIndex.first then
          match prev_leaf iter.rest with
          | None ->
              None
          | Some(leaf, rest) ->
              iter.leaf <- leaf;
              iter.idx <- UTF8.ByteIndex.last leaf;
              iter.rest <- rest;
              Some(UTF8.ByteIndex.look leaf iter.idx)
	else begin
          iter.idx <- UTF8.ByteIndex.prev iter.leaf iter.idx;
          Some(UTF8.ByteIndex.look iter.leaf iter.idx)
	end
    end

      (* Can be improved? *)
  let compare a b =
    let ia = Iter.make a and ib = Iter.make b in
    let rec loop _ =
      match Iter.next ia, Iter.next ib with
      | None, None -> 0
      | None, _ -> -1
      | _, None -> 1
      | Some ca, Some cb ->
          match UChar.compare ca cb with
          | 0 -> loop ()
          | n -> n
    in
    loop ()


let of_ustring ustr =
  (* We need fast access to raw bytes: *)
  let bytes = UTF8.to_string_unsafe ustr in
  let byte_length = String.length bytes in

  (* - [rope] is the accumulator
     - [start_byte_idx] is the byte position of the current slice
     - [current_byte_idx] is the current byte position
     - [slice_size] is the number of unicode characters contained
     between [start_byte_idx] and [current_byte_idx] *)
  let rec loop rope start_byte_idx current_byte_idx slice_size =
    if current_byte_idx = byte_length then begin

      if slice_size = 0 then
        rope
      else
        add_slice rope start_byte_idx current_byte_idx slice_size

    end else begin
      let next_byte_idx = UTF8.next ustr current_byte_idx in

      if slice_size = leaf_size then
        (* We have enough unicode characters for this slice, extract
           it and add a leaf to the rope: *)
        loop (add_slice rope start_byte_idx current_byte_idx slice_size)
          next_byte_idx next_byte_idx 0
      else
        loop rope start_byte_idx next_byte_idx (slice_size + 1)
    end
  and add_slice rope start_byte_idx end_byte_idx slice_size =
    append rope (Leaf(slice_size,
                      (* This is correct, we are just extracting a
                         sequence of well-formed UTF-8 encoded unicode
                         characters: *)
                      UTF8.of_string_unsafe
                        (String.sub bytes start_byte_idx (end_byte_idx - start_byte_idx))))
  in
  loop Empty 0 0 0

  let of_string s =
    (* Validate + unsafe to avoid an extra copy (it is OK because
       of_ustring do not reuse its argument in the resulting rope): *)
    UTF8.validate s;
    of_ustring (UTF8.of_string_unsafe s)

  let append_us r us = append r (of_ustring us)

  let rec make len c =
    let rec concatloop len i r =
      if i <= len then
        (*TODO: test for sharing among substrings *)
        concatloop len (i * 2) (append r r)
      else r
    in
      if len = 0 then Empty
      else if len <= leaf_size then Leaf (len, (UTF8.make len c))
      else
        let rope = concatloop len 2 (of_ustring (UTF8.make 1 c)) in
          append rope (make (len - length rope) c)
   
  let of_uchar c = make 1 c
  let of_char c = of_uchar (UChar.of_char c)

  let sub r start len = 
    let rec aux start len = function
      Empty -> if start <> 0 || len <> 0 then raise Out_of_bounds else Empty
    | Leaf (lens, s) ->
        if len < 0 || start < 0 || start + len > lens then
          raise Out_of_bounds
        else if len > 0 then (* Leaf "" cannot happen *)
          (try Leaf (len, (UTF8.sub s start len)) with _ -> raise Out_of_bounds)
        else Empty
    | Concat(l,cl,r,cr,_) ->
        if start < 0 || len < 0 || start + len > cl + cr then raise Out_of_bounds;
        let left =
          if start = 0 then
            if len >= cl then
              l
            else aux 0 len l
          else if start > cl then Empty
          else if start + len >= cl then
            aux start (cl - start) l
          else aux start len l in
        let right =
          if start <= cl then
            let upto = start + len in
              if upto = cl + cr then r
              else if upto < cl then Empty
              else aux 0 (upto - cl) r
          else aux (start - cl) len r
        in
          append left right
    in aux start len r
   
  let insert start rope r =
    append (append (sub r 0 start) rope) (sub r start (length r - start))
   
  let remove start len r =
    append (sub r 0 start) (sub r (start + len) (length r - start - len))
   
  let to_ustring r =
    let rec strings l = function
        Empty -> l
      | Leaf (_,s) -> s :: l
      | Concat(left,_,right,_,_) -> strings (strings l right) left
    in
      string_of_string_list (strings [] r)
   
  let rec bulk_iter f = function
      Empty -> ()
    | Leaf (_,s) -> f s
    | Concat(l,_,r,_,_) -> bulk_iter f l; bulk_iter f r

  let rec bulk_iteri ?(base=0) f = function
      Empty -> ()
    | Leaf (_,s) -> f base s
    | Concat(l,cl,r,_,_) -> 
        bulk_iteri ~base f l; 
        bulk_iteri ~base:(base+cl) f r

  let rec iter f = function
      Empty -> ()
    | Leaf (_,s) -> UTF8.iter f s
    | Concat(l,_,r,_,_) -> iter f l; iter f r

   
  let rec iteri ?(base=0) f = function
      Empty -> ()
    | Leaf (_,s) ->
	UTF8.iteri (fun j c -> f (base + j) c) s
    | Concat(l,cl,r,_,_) -> iteri ~base f l; iteri ~base:(base + cl) f r
   

  let rec bulk_iteri_backwards ~top f = function
      Empty -> ()
    | Leaf (lens,s) -> f (top-lens) s (* gives f the base position, not the top *)
    | Concat(l,_,r,cr,_) -> 
        bulk_iteri_backwards ~top f r;
        bulk_iteri_backwards ~top:(top-cr) f l

  let rec range_iter f start len = function
      Empty -> if start <> 0 || len <> 0 then raise Out_of_bounds
    | Leaf (lens, s) ->
        let n = start + len in
        if start >= 0 && len >= 0 && n <= lens then
    for i = start to n - 1 do
            f (UTF8.look s (UTF8.nth s i)) (*TODO: use enum to iterate efficiently*)
          done
        else raise Out_of_bounds
    | Concat(l,cl,r,cr,_) ->
        if start < 0 || len < 0 || start + len > cl + cr then raise Out_of_bounds;
        if start < cl then begin
          let upto = start + len in
            if upto <= cl then
              range_iter f start len l
            else begin
              range_iter f start (cl - start) l;
              range_iter f 0 (upto - cl) r
            end
        end else begin
          range_iter f (start - cl) len r
        end
   
  let rec range_iteri f ?(base = 0) start len = function
      Empty -> if start <> 0 || len <> 0 then raise Out_of_bounds
    | Leaf (lens, s) ->
        let n = start + len in
        if start >= 0 && len >= 0 && n <= lens then
  	for i = start to n - 1 do
            f (base+i) (UTF8.look s (UTF8.nth s i))
  	    (*TODO: use enum to iterate efficiently*)
          done
        else raise Out_of_bounds
    | Concat(l,cl,r,cr,_) ->
        if start < 0 || len < 0 || start + len > cl + cr then raise Out_of_bounds;
        if start < cl then begin
          let upto = start + len in
            if upto <= cl then
              range_iteri f ~base start len l
            else begin
              range_iteri f ~base start (cl - start) l;
              range_iteri f ~base:(base + cl - start) 0 (upto - cl) r
            end
        end else begin
          range_iteri f ~base (start - cl) len r
        end

  let rec fold f a = function
      Empty -> a
    | Leaf (_,s) ->
        UTF8.fold (fun a c -> f a c) a s
    | Concat(l,_,r,_,_) -> fold f (fold f a l) r
   
  let rec bulk_fold f a = function
    | Empty                  -> a
    | Leaf   (_, s)          -> f a s
    | Concat (l, _, r, _, _) -> bulk_fold f (bulk_fold f a l) r
    
  let to_string t =
    (* We use unsafe version to avoid the copy of the non-reachable
       temporary string: *)
    UTF8.to_string_unsafe (to_ustring t)

  let init len f = Leaf (len, UTF8.init len f)
    
  let of_string_unsafe s = of_ustring (UTF8.of_string_unsafe s)
  let of_int i = of_string_unsafe (string_of_int i)
  let of_float f = of_string_unsafe (string_of_float f)

  let to_int r = int_of_string (UTF8.to_string_unsafe (to_ustring r))
  let to_float r = float_of_string (UTF8.to_string_unsafe (to_ustring r))

  let bulk_map f r = bulk_fold (fun acc s -> append_us acc (f s)) Empty r
  let map f r = bulk_map (fun s -> UTF8.map f s) r

  let bulk_filter_map f r = bulk_fold (fun acc s -> match f s with None -> acc | Some r -> append_us acc r) Empty r
  let filter_map f r = bulk_map (UTF8.filter_map f) r

  let filter f r = bulk_map (UTF8.filter f) r

  let left r len  = sub r 0 len
  let right r len = let rlen = length r in sub r (rlen - len) len
  let head = left
  let tail r pos = sub r pos (length r - pos)

  let index r u =
    let i = Iter.make r in
    let rec loop n =
      match Iter.next i with
        | None  -> raise Not_found
        | Some u' ->
            if UChar.eq u u' then n else
	    loop (n + 1)
    in
    loop 0


  module Return : sig type 'a t
    (** A label which may be used to return values of type ['a]*)
        
    val label : ('a t -> 'a) -> 'a
    (** [label f] creates a new label [x] and invokes
        [f x]. If, during the execution of [f], [return x v]
        is invoked, the execution of [f x] stops
        immediately and [label f] returns [v].
        Otherwise, if [f x] terminates normally and
        returns [y], [label f] returns [y].

        Calling [return x v] from outside scope [f]
        is a run-time error and causes termination
        of the program.*)
    val with_label  : ('a t -> 'a) -> 'a
      (**as [label]*)

    val return : 'a t -> 'a -> _
    (** Return to a label. [return l v] returns
        to the point where label [l] was obtained
        and produces value [l].

        Calling [return l v] from outside the scope
        of [l] (i.e. the call to function [label]
        which produced [l]) is a run-time error
        and causes termination of the program.*)
end = struct 
    type 'a t = 'a option ref
	  
    exception Return
	
    let return label value =
      label := Some value;
      raise Return (*(Obj.repr label)*)
	
    let label f =
      let r = ref None in
      try   f r
      with  Return when !r <> None -> (*[!r = None] may happen if the user has let the exception escape its scope *)
	match !r with                (*in that case, we wish the exception to fall-through for debugging purposes*)
	| None   -> assert false (*Should be impossible*)
	| Some x -> 
	    r := None;             (*Reset the trap for sanity checks should another exception escape scope    *)
	    x                      (*(not that this should be possible in that case -- let's just be careful)  *)
    let with_label = label
  end

  let index_from r base item = 
    Return.with_label (fun label ->
  	        let index_aux i c = 
  	          if c = item then Return.return label i
  	        in
  	        range_iteri index_aux ~base base (length r - base) r;
  	        raise Not_found)


  let rindex r char = 
    Return.with_label (fun label ->
  	        let index_aux i us =
  	          try 
  	            let p = UTF8.rindex us char in
  	            Return.return label (p+i)
  	          with Not_found -> ()
  	        in
  	        bulk_iteri_backwards ~top:(length r) index_aux r;
  	        raise Not_found)

  let rindex_from r start char = 
    let rsub = left r start in
    (rindex rsub char)

  let contains r char = 
    Return.with_label (fun label ->
  	        let contains_aux us =
  	          if UTF8.contains us char then Return.return label true
  	        in
  	        bulk_iter contains_aux r;
  	        false)

  let contains_from r start char = 
    Return.with_label (fun label ->
  	        let contains_aux c = if c = char then Return.return label true in
  	        range_iter contains_aux start (length r - start) r;
  	        false)

  let rcontains_from = contains_from

  let equals r1 r2 = compare r1 r2 = 0

  let starts_with r prefix =
    let ir = Iter.make r and iprefix = Iter.make prefix in
    let rec loop _ =
      match Iter.next iprefix with
        | None -> true
        | Some ch1 ->
            match Iter.next ir with
              | None -> false
              | Some ch2 -> UChar.compare ch1 ch2 = 0 && loop ()
    in
    loop ()

  let ends_with r suffix =
    let ir = Iter.make r and isuffix = Iter.make suffix in
    let rec loop _ =
      match Iter.prev isuffix with
        | None -> true
        | Some ch1 ->
            match Iter.prev ir with
              | None -> false
              | Some ch2 -> UChar.compare ch1 ch2 = 0 && loop ()
    in
    loop ()

  (** find [r2] within [r1] or raises Not_found *)
  let find_from r1 ofs r2 =
    let matchlen = length r2 in
    let r2_string = to_ustring r2 in
    let check_at pos = r2_string = (to_ustring (sub r1 pos matchlen)) in 
    (* TODO: inefficient *)
    Return.with_label (fun label -> 
  	   for i = ofs to length r1 - matchlen do
  	     if check_at i then Return.return label i
  	   done;
  	   raise Not_found)

  let find r1 r2 = find_from r1 0 r2

  let rfind_from r1 suf r2 =
    let matchlen = length r2 in
    let r2_string = to_ustring r2 in
    let check_at pos = r2_string = (to_ustring (sub r1 pos matchlen)) in 
    (* TODO: inefficient *)
    Return.with_label (fun label -> 
  	   for i = suf - (length r1 + 1 ) downto 0 do
  	     if check_at i then Return.return label i
  	   done;
  	   raise Not_found)

  let rfind r1 r2 = rfind_from r1 (length r2 - 1) r2


  let exists r_str r_sub = try ignore(find r_str r_sub); true with Not_found -> false

  let strip_default_chars = List.map UChar.of_char [' ';'\t';'\r';'\n']
  let strip ?(chars=strip_default_chars) rope =
    let rec strip_left n iter =
      match Iter.next iter with
        | None ->
            Empty
        | Some ch when List.mem ch chars ->
            strip_left (n + 1) iter
        | _ ->
            sub rope n (strip_right (length rope - n) (Iter.make rope))
    and strip_right n iter =
      match Iter.prev iter with
        | None ->
            assert false
        | Some ch when List.mem ch chars ->
            strip_right (n - 1) iter
        | _ ->
            n
    in
    strip_left 0 (Iter.make rope)

  let lchop = function
    | Empty -> Empty
    | str -> sub str 1 (length str - 1)
  let rchop = function
    | Empty -> Empty
    | str -> sub str 0 (length str - 1)



let of_list l =
  let e = ref l in
  let get_leaf () =
    Return.label
      (fun label ->
	 let b = Buffer.create 256 in
	 for i = 1 to 256 do
	   match !e with
	       []   -> Return.return label (false, UTF8.of_string_unsafe (Buffer.contents b))
	     | c :: rest  -> Buffer.add_string b (UTF8.to_string_unsafe (UTF8.of_char c)); e := rest
	 done;
	 (true, UTF8.of_string_unsafe (Buffer.contents b) ))
  in
  let rec loop r = (* concat 256 characters at a time *)
    match get_leaf () with
	(true,  us) -> loop     (append r (of_ustring us))
      | (false, us) -> append r (of_ustring us)
  in
  loop Empty

  let splice r start len new_sub = 
    let start = if start >= 0 then start else (length r) + start in
    append (left r start) 
      (append new_sub (tail r (start+len)))

  let fill r start len char = 
    splice r start len (make len char)

  let blit rsrc offsrc rdst offdst len = 
    splice rdst offdst len (sub rsrc offsrc len)


  let list_reduce f = function [] -> invalid_arg "Empty List"
    | h::t -> List.fold_left f h t

  let concat sep r_list = 
    if r_list = [] then empty else
    list_reduce (fun r1 r2 -> append r1 (append sep r2)) r_list

  (**T concat
     concat (of_string "xyz") [] = empty
  **)

  let escaped r = bulk_map UTF8.escaped r

  let replace_chars f r = fold (fun acc s -> append_us acc (f s)) Empty r

  let split r sep = 
    let i = find r sep in
    head r i, tail r (i+length sep)

  let rsplit (r:t) sep = 
    let i = rfind r sep in
    head r i, tail r (i+length sep)

  (**
     An implementation of [nsplit] in one pass.

     This implementation traverses the string backwards, hence building the list
     of substrings from the end to the beginning, so as to avoid a call to [List.rev].
  *)
  let nsplit str sep =
    if is_empty str then []
    else let seplen = length sep in
         let rec aux acc ofs = match 
  	 try Some(rfind_from str ofs sep)
  	 with Invalid_rope -> None
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


  let join = concat

  let slice ?(first=0) ?(last=max_int) s =
    let clip _min _max x = int_max _min (int_min _max x) in
    let i = clip 0 (length s)
      (if (first<0) then (length s) + first else first)
    and j = clip 0 (length s)
      (if (last<0) then (length s) + last else last)
    in
      if i>=j || i=length s then
        Empty
      else
        sub s i (j-i)


  let replace ~str ~sub ~by = 
    try
      let i = find str sub in
        (true, append (slice ~last:i str)  (append by 
           (slice ~first:(i+(length sub)) str)))
    with
        Invalid_rope -> (false, str)


  let explode r = fold (fun a u -> u :: a) [] r

  let implode l = of_list l

  type t_alias = t (* fixes [type t = t] bug below *)


  let of_latin1 s = of_ustring (UTF8.of_latin1 s)

  (* =end *)
end

(** Aliase for Text.t *)
type text = Text.t
type cursor = int

module CharEncoding  = struct

  let subst_char = UChar.chr 0xfffd

  let fold_string f a s =
    let ret = ref a in
    let f' c =
      ret := f !ret c in 
    String.iter f' s; !ret
      
  module type Encoder = sig
    type state
    val init : state
    val encode : ?repl:(uchar -> text) -> state -> text ->
      [`Success of state * string | `Error ]
    val terminate : state -> string
  end

  module type Decoder = sig      
    type state
    val init : state
    val decode : state -> string -> state * text
    val terminate : state -> text
  end

  type t = {name : string; 
            encoder : (module Encoder);
            decoder : (module Decoder)}

  type enc = t

  module type UnivEncoder = sig
    module Encode : Encoder
    type state = Encode.state
    val current_state : state
    val encode : state -> text -> [`Success of state * string | `Error ] 
    val terminate : state -> string
  end

  type encoder = (module UnivEncoder) 

  let create_encoder ?repl enc : (module UnivEncoder) = 
    (module struct 
      module Encode = (val enc.encoder)
      type state =  Encode.state
      let current_state = Encode.init
      let encode = Encode.encode ?repl
      let terminate = Encode.terminate
    end)

  let repl_escape u = Text.of_latin1 (UChar.escape u)

  let encode (encoder : (module UnivEncoder)) text =
    let module E = (val encoder) in
    match E.encode E.current_state text with
      `Success (state, string) ->
        let encoder : (module UnivEncoder) = (module struct
          module Encoder = E
          include E
          let current_state = state
        end) in
        `Success (encoder, string)
    | `Error -> `Error

  let terminate_encoder (encoder : (module UnivEncoder)) =
    let module E = (val encoder) in
    E.terminate E.current_state

  let encode_text ?repl enc text =
    let module E = (val enc.encoder) in
    match E.encode ?repl E.init text with
      `Error -> `Error
    | `Success (state, string) ->
        `Success (string ^ (E.terminate state))

  module type UnivDecoder = sig
    module Decode : Decoder
    val current_state : Decode.state
    val decode : Decode.state -> string -> Decode.state * text
    val terminate : Decode.state -> text
  end

  type decoder = (module UnivDecoder) 

  let create_decoder enc : (module UnivDecoder) = 
    (module struct 
      module Decode = (val enc.decoder)
      include Decode
      let current_state = Decode.init
    end)

  let decode (decoder : (module UnivDecoder))  s =
    let module D = (val decoder) in
    let state, text = D.decode D.current_state s in
    let module D' = struct
      module Decode = D.Decode
      include Decode
      let current_state = state
    end in
    let state' : (module UnivDecoder) = (module D') in
    (state', text)

  let terminate_decoder (decoder : (module UnivDecoder)) =
    let module D = (val decoder) in
    D.terminate D.current_state

  let decode_string enc s =
    let module D = (val enc.decoder) in
    let state, text = D.decode D.init s in 
    Text.append text (D.terminate state)

  let recode ?repl enc1 s1 enc2 = 
    let text = decode_string enc1 s1 in
    encode_text ?repl enc2 text

 (* Individual encodings *)
  module AsciiEnc = struct 
    type state = unit
    let init = ()

    let rec encode ?repl () text =
      let b = Buffer.create 0 in
      let conv u =
        let n = UChar.int_of u in
        if n < 0x80 then
          Buffer.add_char b (Char.chr n)
        else begin
          match repl with
              Some repl -> 
                (match encode () (repl u) with
                    `Success ((), subst) ->
                      Buffer.add_string b subst
                  | `Error -> raise Out_of_range)
          | None -> raise Out_of_range
        end in
      try 
        `Success ((), (Text.iter conv text; Buffer.contents b))
      with 
          Out_of_range -> `Error
      
    let terminate () = ""
  end

  module AsciiDec = struct

    type state = unit

    let init = ()

    let decode () s =
      let conv i =
        if Char.code s.[i] < 0x80 then UChar.of_char s.[i] else
        UChar.of_int 0xfffd in
      (), Text.init (String.length s) conv 

    let terminate () = Text.empty 
  end

  let ascii = {name = "US-ASCII"; 
               encoder = (module AsciiEnc);
               decoder = (module AsciiDec)}

  module Latin1Enc = struct 
    type state = unit
    let init = ()

    let rec encode ?repl () text =
      let b = Buffer.create 0 in
      let conv u =
        let n = UChar.int_of u in
        if n < 0x100 then
          Buffer.add_char b (Char.chr n)
        else begin
          match repl with
              Some repl -> 
                (match encode () (repl u) with
                    `Success ((), subst) ->
                      Buffer.add_string b subst
                  | `Error -> raise Out_of_range)
          | None -> raise Out_of_range
        end in
      try 
        `Success ((), (Text.iter conv text; Buffer.contents b))
      with 
          Out_of_range -> `Error
      
    let terminate () = ""
  end

  module Latin1Dec = struct

    type state = unit

    let init = ()

    let decode () s =
      let conv i = UChar.of_char s.[i] in
      (), Text.init (String.length s) conv 

    let terminate () = Text.empty 
  end

  let latin1 = {name = "Latin-1"; 
               encoder = (module Latin1Enc);
               decoder = (module Latin1Dec)}


  module UTF8Enc = struct
    type state = unit

    let init = ()

    (* TODO: We need to check noncharacters.*) 
    let encode ?repl () t =
      let s = Text.to_string t in
      `Success ((), s)

    let terminate () = ""
  end

  module UTF8Dec = struct 
    type state = [ `Start 
                 | `Second of int 
                 | `Trail of int * int]

    let init = `Start

    (* TODO: We need filter noncharacters. *)
    let rec decode_start s i text = 
      if i >= String.length s then (`Start, text) else
      let n = Char.code s.[i] in
      if n < 0x80 then 
        decode_start s (i+1) (Text.append_char (UChar.chr n) text)
      else if n >= 0xc2 && n <= 0xf4 then
        decode_second n s (i+1) text
      else
        decode_start s (i+1) (Text.append_char subst_char text) 
    
    and decode_second a s i text =
      if i >= String.length s then (`Second a,  text) else
      let n = Char.code s.[i] in
      (* 2-bytes code *)
      if a >= 0xc2 && a <= 0xdf then
        if (n >= 0x80 && n <= 0xbf) then
          let n = (a - 0xc0) lsl 6 lor (0x7f land n) in
          decode_start s (i+1) (Text.append_char (UChar.chr n) text)
        else
          decode_start s i (Text.append_char subst_char text)
      (* 3-bytes code *)
      else if (a >= 0xe1 && a <= 0xec) || a = 0xee || a = 0xef then
        if (n >= 0x80 && n <= 0xbf) then
          let a = (a - 0xe0) lsl 6 lor (0x7f land n) in
          decode_trail 1 a s (i+1) text
        else
          decode_start s i   (Text.append_char subst_char text)
      else if a = 0xe0 then
        if n >= 0xa0 && n <= 0xbf then
          let a = 0x7f land n in
          decode_trail 1 a s (i+1) text
        else
          decode_start s i (Text.append_char subst_char text)
      else if a = 0xed then 
        if n >= 0x80 && n <= 0x9f then
          let a = (a - 0xe0) lsl 6 lor (0x7f land n) in
          decode_trail 1 a s (i+1) text
        else
          decode_start s i (Text.append_char subst_char text)
      (* 4-bytes code *) 
      else if a = 0xf0 then
        if n >= 0x90 && n <= 0xbf then
          let a = (a - 0xf0) lsl 6 lor (0x7f land n) in
          decode_trail 2 a s (i+1) text
        else
          decode_start s i (Text.append_char subst_char text)
      else if a >= 0xf1 && a <= 0xf3 then
        if n >= 0x80 && n <= 0xbf then
          let a = (a - 0xf0) lsl 6 lor (0x7f land n) in
          decode_trail 2 a s (i+1) text
        else
          decode_start s i (Text.append_char subst_char text)
      else if a = 0xf4 then
        if n >= 0x80 && n <= 0x8f then
          let a = (a - 0xf0) lsl 6 lor (0x7f land n) in
          decode_trail 2 a s (i+1) text
        else
          decode_start s i (Text.append_char subst_char text)
      else      
          decode_start s i (Text.append_char subst_char text)

    and decode_trail count a s i text =
          if i >= String.length s then (`Trail (count, a),  text) else  (* FIX ME *)
          let n = Char.code s.[i] in
          if n >= 0x80 && n <= 0xbf then
          let a = a lsl 6 lor (0x7f land n) in
            if count = 1 then
              decode_start s (i+1) (Text.append_char (UChar.chr a) text)
            else if count = 2 then
              decode_trail 1 a s (i+1) text
            else
              assert false
          else
            decode_start s i (Text.append_char subst_char text)

    let decode st s =
      match st with
        `Start -> decode_start s 0 Text.empty
      | `Second a -> decode_second a s 0 Text.empty
      | `Trail (c, a) -> decode_trail c a s 0 Text.empty

    let terminate = function
    | `Start -> Text.empty
    | _ -> Text.of_uchar subst_char

  end

  let utf8 = {name = "UTF-8"; 
               encoder = (module UTF8Enc);
               decoder = (module UTF8Dec)}

  module UTF16BEEnc = struct 
    type state = unit
    let init = ()

    let rec encode ?repl () text =
      let b = Buffer.create 0 in
      let conv u =
        let n = UChar.int_of u in
        if n < 0x10000 then begin
          Buffer.add_char b (Char.chr (n lsr 8));
	  Buffer.add_char b (Char.chr (n land 0xff));
        end else begin
	  let n1 = 0xD800 + (n - 0x10000) lsr 10 in
	  let n2 = 0xDC00 + (n - 0x10000) land 0x03FF in
          Buffer.add_char b (Char.chr (n1 lsr 8));
	  Buffer.add_char b (Char.chr (n1 land 0xff));
          Buffer.add_char b (Char.chr (n2 lsr 8));
	  Buffer.add_char b (Char.chr (n2 land 0xff));
	end in
      `Success ((), (Text.iter conv text; Buffer.contents b))
      
    let terminate () = ""
  end

  module UTF16BEDec = struct

    type state = 
	[ `Success
      | `Byte of char
      | `Surrogate of int
      | `Surrogate_with_Byte of int * char]
	  
    let init = `Success

    let decode state s =
      let conv (state, text) c =
	match state with
	  `Success -> `Byte c, text
	| `Byte c0 ->
	    let n = Char.code c0 in
	    let n = (n lsl 8) lor (Char.code c) in
	    if n >= 0xD800 && n <= 0xDBFF then
	      `Surrogate n, text
	    else if n >= 0xDC00 && n <= 0xDFFF then
	      `Byte c, Text.append_char subst_char text
	    else
	      `Success, Text.append_char (UChar.of_int n) text 
	| `Surrogate n -> 
	    if Char.code c < 0xDC or Char.code c > 0xdf then
	      `Byte c, Text.append_char subst_char text 
	    else
	      `Surrogate_with_Byte (n, c), text
	| `Surrogate_with_Byte (n0, c0) ->
	    let n = Char.code c0 in
	    let n = (n lsl 8) lor (Char.code c) in
	    let n1 = 0x10000 + (n0 - 0xD800) lsl 10 lor (n - 0xDC00) in
	    `Success, Text.append_char (UChar.of_int n1) text in
      fold_string conv (state, Text.empty) s

    let terminate = function
	`Success -> Text.empty
      | _ -> Text.of_uchar subst_char
  end

  let utf16be = {name = "UTF-16BE"; 
               encoder = (module UTF16BEEnc);
               decoder = (module UTF16BEDec)}

  module UTF16LEEnc = struct 
    type state = unit
    let init = ()

    let rec encode ?repl () text =
      let b = Buffer.create 0 in
      let conv u =
        let n = UChar.int_of u in
        if n < 0x10000 then begin
	  Buffer.add_char b (Char.chr (n land 0xff));
          Buffer.add_char b (Char.chr (n lsr 8));
        end else begin
	  let n1 = 0xD800 + (n - 0x10000) lsr 10 in
	  let n2 = 0xDC00 + (n - 0x10000) land 0x03FF in
	  Buffer.add_char b (Char.chr (n1 land 0xff));
          Buffer.add_char b (Char.chr (n1 lsr 8));
	  Buffer.add_char b (Char.chr (n2 land 0xff));
          Buffer.add_char b (Char.chr (n2 lsr 8));
	end in
      `Success ((), (Text.iter conv text; Buffer.contents b))
      
    let terminate () = ""
  end

  module UTF16LEDec = struct

    type state = 
	[ `Success
      | `Byte of char
      | `Surrogate of int
      | `Surrogate_with_Byte of int * char]
	  
    let init = `Success

    let fold_string f a s =
      let ret = ref a in
      let f' c =
	ret := f !ret c in 
      String.iter f' s; !ret

    let decode state s =
      let conv (state, text) c =
	match state with
	  `Success -> `Byte c, text
	| `Byte c0 ->
	    let n = Char.code c in
	    let n = (n lsl 8) lor (Char.code c0) in
	    if n >= 0xD800 && n <= 0xDBFF then
	      `Surrogate n, text
	    else if n >= 0xDC00 && n <= 0xDFFF then
	      `Byte c, Text.append_char subst_char text
	    else
	      `Success, Text.append_char (UChar.of_int n) text 
	| `Surrogate n -> 
	      `Surrogate_with_Byte (n, c), text
	| `Surrogate_with_Byte (n0, c0) ->
	    let n = Char.code c in
	    let n = (n lsl 8) lor (Char.code c0) in
	    if n >= 0xDC00 && n <= 0xDFFF then
	      let n1 = 0x10000 + (n0 - 0xD800) lsl 10 lor (n - 0xDC00) in
	      `Success, Text.append_char (UChar.of_int n1) text
	    else
	      `Byte c, Text.append_char subst_char text
      in
      fold_string conv (state, Text.empty) s

    let terminate = function
	`Success -> Text.empty
      | _ -> Text.of_uchar subst_char
  end

  let utf16le = {name = "UTF-16LE"; 
               encoder = (module UTF16LEEnc);
               decoder = (module UTF16LEDec)}

  module UTF16Enc = struct
    type state = [`Init | `After of UTF16BEEnc.state]

    let init = `Init

    let encode ?repl state text =
      let s0, state = 
	match state with
	  `Init -> "\xFE\xFF", UTF16BEEnc.init
	| `After state -> "", state in
      match UTF16BEEnc.encode ?repl state text with
	`Success (state, s1) -> `Success (`After state, s0 ^ s1)
      | `Error -> `Error

    let terminate = function
	`Init -> ""
      | `After state -> UTF16BEEnc.terminate state
  end

  module UTF16Dec = struct
    type state = 
	[`Init | `Byte of char | `BE of UTF16BEDec.state | `LE of UTF16LEDec.state]

    let init = `Init

    let decode state s =
      match state with
	`Init ->      
	  if String.length s = 0 then `Init, Text.empty else
	  if String.length s = 1 then `Byte s.[0], Text.empty else
	  if s.[0] = '\xfe' && s.[1] = '\xff' then
	    let s' = String.sub s 2 (String.length s - 2) in
	    let state, text = UTF16BEDec.decode UTF16BEDec.init s' in
	    `BE state, text
	  else if s.[0] = '\xff' && s.[1] = '\xfe' then
	    let s' = String.sub s 2 (String.length s - 2) in
	    let state, text = UTF16LEDec.decode UTF16LEDec.init s' in
	    `LE state, text
	  else
	    let state, text = UTF16BEDec.decode UTF16BEDec.init s in
	    `BE state, text
      | `Byte c0 ->
	  if String.length s = 0 then `Byte c0, Text.empty else
	  if c0 = '\xfe' && s.[0] = '\xff' then
	    let s' = String.sub s 1 (String.length s - 1) in
	    let state, text = UTF16BEDec.decode UTF16BEDec.init s' in
	    `BE state, text
	  else if c0 = '\xff' && s.[0] = '\xfe' then
	    let s' = String.sub s 1 (String.length s - 1) in
	    let state, text = UTF16LEDec.decode UTF16LEDec.init s' in
	    `LE state, text
	  else
	    let s' = (String.make 1 c0) ^ s in
	    let state, text = UTF16BEDec.decode UTF16BEDec.init s' in
	    `BE state, text
      | `BE state ->
	  let state, text = UTF16BEDec.decode state s in
	  `BE state, text
      | `LE state ->
	  let state, text = UTF16LEDec.decode state s in
	  `LE state, text

    let terminate = function
	`Init -> Text.empty
      | `Byte c -> Text.of_uchar subst_char
      | `BE state -> UTF16BEDec.terminate state
      | `LE state -> UTF16LEDec.terminate state
  end

  let utf16 = {name = "UTF-16"; 
               encoder = (module UTF16Enc);
               decoder = (module UTF16Dec)}

  module UTF32BEEnc = struct 
    type state = unit
    let init = ()

    let rec encode ?repl () text =
      let b = Buffer.create 0 in
      let conv u =
        let n = UChar.int_of u in
	Buffer.add_char b (Char.chr (n lsr 24));
	Buffer.add_char b (Char.chr ((n lsr 16) land 0xff));
	Buffer.add_char b (Char.chr ((n lsr 8) land 0xff));
	Buffer.add_char b (Char.chr (n land 0xff)) in
      `Success ((), (Text.iter conv text; Buffer.contents b))
      
    let terminate () = ""
  end

  module UTF32BEDec = struct

    type state = int * int
	  
    let init = (0, 0)

    let decode state s =
      let conv ((n, i), text) c =
	if i = 0 then
	  if Char.code c > 0 then
	    (0, 0), Text.append_char subst_char text
	  else
	    (0, 1), text
	else if i = 1 then
	  if Char.code c > 0x10 then
	    (0, 0), Text.append_char subst_char (Text.append_char subst_char text)
	  else
	    (Char.code c, 2), text
	else if i = 2 then
	  (n lsl 8 lor (Char.code c), 3), text
	else if i = 3 then
	  let n = n lsl 8 lor (Char.code c) in
	  (0, 0), Text.append_char (UChar.of_int n) text
	else assert false in
      fold_string conv (state, Text.empty) s

    let terminate = function
	(0, 0) -> Text.empty
      | _ -> Text.of_uchar subst_char
  end

  let utf32be = {name = "UTF-32BE"; 
               encoder = (module UTF32BEEnc);
               decoder = (module UTF32BEDec)}

  module UTF32LEEnc = struct 
    type state = unit
    let init = ()

    let rec encode ?repl () text =
      let b = Buffer.create 0 in
      let conv u =
        let n = UChar.int_of u in
	Buffer.add_char b (Char.chr (n land 0xff));
	Buffer.add_char b (Char.chr ((n lsr 8) land 0xff));
	Buffer.add_char b (Char.chr ((n lsr 16) land 0xff));
	Buffer.add_char b (Char.chr (n lsr 24)) in
      `Success ((), (Text.iter conv text; Buffer.contents b))
      
    let terminate () = ""
  end

  module UTF32LEDec = struct

    type state = int * int
	  
    let init = (0, 0)

    let decode state s =
      let conv ((n, i), text) c =
	if i = 0 then
	  (Char.code c, 1), text
	else if i = 1 then
	  (n lor (Char.code c lsl 8), 2), text
	else if i = 2 then
	  if Char.code c > 0x10 then
	    (Char.code c, 1), Text.append_char subst_char text
	  else
	    (n lor (Char.code c lsl 16), 3), text
	else if i = 3 then
	  if Char.code c > 0 then
	    (Char.code c, 1), Text.append_char subst_char text
	  else
	    (0, 0), Text.append_char (UChar.of_int n) text
	else assert false in
      fold_string conv (state, Text.empty) s

    let terminate = function
	(0, 0) -> Text.empty
      | _ -> Text.of_uchar subst_char

  end

  let utf32le = {name = "UTF-32LE"; 
               encoder = (module UTF32LEEnc);
               decoder = (module UTF32LEDec)}

  module UTF32Enc = struct
    type state = [`Init | `After of UTF32BEEnc.state]

    let init = `Init

    let encode ?repl state text =
      let s0, state = 
	match state with
	  `Init -> "\x00\x00\xFE\xFF", UTF16BEEnc.init
	| `After state -> "", state in
      match UTF32BEEnc.encode ?repl state text with
	`Success (state, s1) -> `Success (`After state, s0 ^ s1)
      | `Error -> `Error

    let terminate = function
	`Init -> ""
      | `After state -> UTF16BEEnc.terminate state
  end

  module UTF32Dec = struct
    type state = 
	[`Bytes of string | `BE of UTF32BEDec.state | `LE of UTF32LEDec.state]

    let init = `Bytes ""

    let decode state s =
      match state with
	`Bytes s0 ->
	  let s = s0 ^ s in
	  if String.length s < 4 then
	    `Bytes s, Text.empty
	  else if String.sub s 0 4 = "\x00\x00\xfe\xff" then
	    let s = String.sub s 4 (String.length s - 4) in
	    let state, text = UTF32BEDec.decode UTF32BEDec.init s in
	    `BE state, text
	  else if String.sub s 0 4 = "\xff\xfe\x00\x00" then
	    let s = String.sub s 4 (String.length s - 4) in
	    let state, text = UTF32LEDec.decode UTF32LEDec.init s in
	    `LE state, text
	  else
	    let state, text = UTF32BEDec.decode UTF32BEDec.init s in
	    `BE state, text
      | `BE state ->
	  let state, text = UTF32BEDec.decode state s in
	  `BE state, text
      | `LE state ->
	  let state, text = UTF32LEDec.decode state s in
	  `LE state, text

    let terminate = function
      `Bytes s -> if s = "" then Text.empty else Text.of_uchar subst_char
      | `BE state -> UTF32BEDec.terminate state
      | `LE state -> UTF32LEDec.terminate state
  end

  let utf32 = {name = "UTF-32"; 
               encoder = (module UTF32Enc);
               decoder = (module UTF32Dec)}
  (* Encoding management *)

  let enc_search_funcs : (string -> enc option) list ref = ref []

  let register f = 
    enc_search_funcs := f :: !enc_search_funcs

  let builtin name =
    match name with
      "US-ASCII" | "USASCII" | "ASCII" | "ISO646US" | "CP367" | "ANSI_X3.4-1968" 
    | "IANA/csASCII" | "IANA/cp367" | "IANA/IBM367" | "IANA/us"
    | "IANA/US-ASCII" | "IANA/ISO646-US" | "IANA/ASCII" | "IANA/ISO_646.irv:1991"
    | "IANA/ANSI_X3.4-1986" | "IANA/iso-ir-6" | "IANA/ANSI_X3.4-1968" ->
        Some ascii
    | "ISO-8859-1" | "IANA/csISOLatin1" | "IANA/CP819" | "IANA/IBM819"
    | "IANA/l1" | "IANA/latin1" | "IANA/ISO-8859-1" | "IANA/ISO_8859-1"
    | "IANA/iso-ir-100" | "IANA/ISO_8859-1:1987" ->
        Some latin1
    | "UTF-8" | "IANA/UTF-8" -> Some utf8
    | "UTF-16BE" | "IANA/UTF-16BE" -> Some utf16be
    | "UTF-16LE" | "IANA/UTF-16LE" -> Some utf16le
    | "UTF-16" | "IANA/UTF-16" -> Some utf16
    | "UTF-32BE" | "IANA/UTF-32BE" -> Some utf32be
    | "UTF-32LE" | "IANA/UTF-32LE" -> Some utf32le
    | "UTF-32" | "IANA/UTF-32" -> Some utf32
    | _ -> None

  let () =  register builtin

  let of_name name =
    let call ret f =
      match ret with
        None -> f name
      | Some enc as x -> x in
    List.fold_left call None !enc_search_funcs
end
