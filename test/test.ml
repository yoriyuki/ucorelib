open OUnit;;

(* Helpers *)

let try_chr n = try Some (UChar.chr n) with UChar.Out_of_range -> None

let sgn n = if n < 0 then -1 else if n = 0 then 0 else 1

(* Tests for UChar *)

let test_char1 () =
  for i = 0 to 255 do
    let c = Char.chr i in
    let c' = UChar.char_of (UChar.of_char c) in
    assert_equal c c'
  done

let test_char2 () =
  for i = 0 to 10000 do
    let n = Random.bits () in
    match try_chr n with
      None -> ()
    | Some u ->
	if n < 255 then
	  assert_equal (UChar.char_of u) (Char.chr n)
	else
	  assert_raises UChar.Out_of_range (fun () -> UChar.char_of u)
  done

let test_uchar_eq () =
  assert_equal true (UChar.eq (UChar.of_char 'a') (UChar.of_char 'a'));
  assert_equal true (UChar.eq (UChar.chr 0xffff) (UChar.chr 0xffff));
  assert_equal false(UChar.eq (UChar.chr 0xffff) (UChar.chr 0xfffe))

let test_int_uchar () =
  for i = 0 to 10000 do
    let n = Random.bits () in
    if n < 0xd800 then
      assert_equal n (UChar.code (UChar.chr n))
    else if n < 0xe000 then
      assert_raises UChar.Out_of_range (fun () -> (UChar.chr n))
    else if n <= 0x10ffff then
	assert_equal n (UChar.code (UChar.chr n))
    else
      assert_raises UChar.Out_of_range (fun () -> (UChar.chr n))
  done	

let test_uchar_compare () =
  for i = 0 to 10000 do
    let n1 = Random.bits () in
    let n2 = Random.bits () in
    match try_chr n1, try_chr n2 with
    Some u1, Some u2 -> 
      assert_equal (sgn (compare n1 n2)) (sgn (UChar.compare u1 u2))
    | _ -> ()
  done

(* UTF-8 *)

let rec random_uchar () = 
  match try_chr (Random.bits ()) with
    Some u -> u
  | None -> random_uchar ()

let test_utf8 () =
  for i = 0 to 10 do
    let a = Array.init (Random.int 1000) (fun _ -> random_uchar ()) in
    let s = UTF8.init (Array.length a) (Array.get a) in

    (* test for length *)
    let len = UTF8.length s in
    assert_equal len (Array.length a);

    (* indexing *)
    for i = 0 to Array.length a - 1 do
      assert_equal (UTF8.get s i) a.(i)
    done;
    
    (* iteration *)
    let r = ref 0 in
    UTF8.iter (fun u ->
      assert_equal u a.(!r);
      incr r) s;
    assert_equal !r len;
    
    (* index *)
    let cur = ref (UTF8.nth s 0) in
    let r = ref 0 in
    while not (UTF8.out_of_range s !cur) do
      assert_equal (UTF8.look s !cur) a.(!r);
      cur := UTF8.next s !cur;
      incr r
    done;
    assert_equal !r len;
    
    (* Moving index around *)
    for i = 0 to 100 do
      let pos = Random.int len in
      let cur = UTF8.nth s pos in
      assert_equal (UTF8.look s cur) a.(pos);
      
      if pos = 0 then () else
      let cur' = UTF8.prev s cur in
      assert_equal (UTF8.look s cur') a.(pos - 1);
    done;
      
      (* Buffer *)
    let b = UTF8.Buf.create 0 in
    
    let p = Random.int len in
    let s1 = UTF8.init p (Array.get a) in
    let s2 = UTF8.init (len - p) (fun x -> Array.get a (p + x)) in
    
    UTF8.Buf.add_string b s1;
    UTF8.Buf.add_string b s2;
    let s' = UTF8.Buf.contents b in
    assert_bool "step1" (UTF8.compare s s' = 0);
    
    UTF8.Buf.clear b;
    UTF8.iter (UTF8.Buf.add_char b) s;
    let s' = UTF8.Buf.contents b in
    assert_bool "step2" (UTF8.compare s s' = 0);
    
    UTF8.Buf.clear b;
    let b' = UTF8.Buf.create 16 in
    let pos = Random.int len in
    for i = 0 to len - 1 do
      if i < pos then
	UTF8.Buf.add_char b a.(i)
      else
	UTF8.Buf.add_char b' a.(i)
    done;
    UTF8.Buf.add_buffer b b';
    let s' = UTF8.Buf.contents b in
    assert_bool "step3" (UTF8.compare s s' = 0);
    
    UTF8.Buf.reset b;
    UTF8.Buf.add_string b s;
    let s' = UTF8.Buf.contents b in
    assert_bool "step4" (UTF8.compare s s' = 0)
  done

let suite = 
  "ulib test" >:::
  ["test UChar" >:::
   ["chr<->uchar" >:::
    ["uchar<-char" >:: test_char1;
     "char<-uchar" >:: test_char2];
    "uchar<->code" >:: test_int_uchar;
    "test_uchar_eq" >:: test_uchar_eq;
    "test_uchar_compare" >:: test_uchar_compare];
   "test UTF8" >:: test_utf8]

let _ = 
  run_test_tt_main suite
