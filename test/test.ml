open OUnit
open UCoreLib

(* Helpers *)

let try_chr n = try Some (UChar.chr n) with Out_of_range -> None

let sgn n = if n < 0 then -1 else if n = 0 then 0 else 1

let rec range i j = 
  if i > j then [] else
  i :: range (i + 1) j

let sprint_text text =
  let b = Buffer.create 0 in
  let f () u = Buffer.add_string b (Printf.sprintf "\\%08x" (UChar.code u)) in
  Text.fold f () text; Buffer.contents b

let sprint_string s =
  let b = Buffer.create 0 in
  let f c = Buffer.add_string b (Printf.sprintf "\\%02x" (Char.code c)) in
  String.iter f s; Buffer.contents b


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
	  assert_raises Out_of_range (fun () -> UChar.char_of u)
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
      assert_raises Out_of_range (fun () -> (UChar.chr n))
    else if n <= 0x10ffff then
	assert_equal n (UChar.code (UChar.chr n))
    else
      assert_raises Out_of_range (fun () -> (UChar.chr n))
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

let () = Random.self_init ()

let rec random_uchar () = 
  match try_chr (Random.bits ()) with
    Some u -> u
  | None -> random_uchar ()

let test_utf8_random () =
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


(* stress test *) 
let random_string () =
  let s = String.create (Random.int 1000) in
  for i = 0 to String.length s - 1 do
    s.[i] <- Char.chr (Random.int 256)
  done;
  s

let test_utf8_random_string () =
  for i = 0 to 100 do
    let s = random_string () in
    match (try UTF8.validate s; Some s with Malformed_code -> None) with
      None -> ()
    | Some s ->
	let cur = ref (UTF8.nth s 0) in
	let r = ref 0 in
	while not (UTF8.out_of_range s !cur) do
	  ignore(UTF8.look s !cur);
	  cur := UTF8.next s !cur;
	  incr r
	done;
	assert_equal !cur (String.length s);
	assert_equal !r (UTF8.length s)
  done
	

(* Test based on "UTF-8 decoder capability and stress test" by
    Markus Kuhn <http://www.cl.cam.ac.uk/~mgk25/> - 2003-02-19 *)

let test_utf8_valid_utf8_string (name, s, clist) =
  let test () =
    assert_equal (UTF8.validate s) ();
    let last = 
      List.fold_left (fun i n ->
	let u = UTF8.look s i in
	assert_equal ~msg:(Printf.sprintf "character %x != %x" n (UChar.code u)) u  (UChar.chr n);
	UTF8.next s i) 0 clist
    in
    assert_equal ~msg:"length" last (String.length s) in
  ("valid string: " ^ name) >:: test

  
let utf8_valid_pairs =
  [
   (* Greek word*)
   ("kosme", "κόσμε", [0x03BA; 0x1f79; 0x03C3; 0x03BC; 0x03B5]);

   (* Boundary cases *)
   ("NULL", " ", [0x00]);
   ("0x80", "", [0x0080]);
   ("0x800", "ࠀ", [0x800]);
   ("0x10000", "𐀀", [0x00010000]);
   ("0x7F", "", [0x0000007F]);
   ("0x7FF", "߿", [0x07FF]);
   ("0xFFFF", "￿", [0xFFFF]);
   ("0xD7FF", "퟿", [0xD7FF]);
   ("0xE000", "",[0xE000]);
   ("0xFFFD", "�", [0xFFFD]);
   ("0x10FFFF", "􏿿", [0x10FFFF]);
 ]

let test_utf8_invalid_utf8_string s =
  ("invalid string:" ^ (String.escaped s)) >:: (fun () -> assert_raises Malformed_code (fun () -> UTF8.validate s))

let utf8_brokens =
  [
   (* Continuation byte *)
   "�"; "�"; "��"; "���"; "����"; "�����";
   "������"; "�������";
   "����������������\
    ����������������\
    ����������������\
    ����������������";

   (* Lonley start characters *)
   "� � � � � � � � � � � � � � � � \
    � � � � � � � � � � � � � � � � ";
   "� � � � � � � � � � � � � � � � ";
   "� � � � � � � � ";
   "� � � � ";
   "� � ";

   (* Missing last byte *)
   "�";
   "��";
   "���";
   "����";
   "�����";
   "�";
   "�";
   "���";
   "����";
   "�����";
   "�����������������������������";

   (* Impossible bytes *)
   "�";
   "�";
   "����";

   (* Overlong sequences *)
   "��";
   "���";
   "����";
   "�����";
   "������";

   "��";
   "���";
   "����";
   "�����";
   "������";

   "��";
   "���";
   "����";
   "�����";
   "������";
   
   (* illegal code point *)
   (* out of range *)
   "����";
   "������";

   (* surrogates *)
   "���";
   "���";
   "���";
   "���";
   "���";
   "���";
   "���";

   "������";
   "������";
   "������";
   "������";
   "������";
   "������";
   "������";  
   "������";

(*   "￾";
   "￿" *)
 ]
  


(* Text *)

let test_text_random () =
  for i = 0 to 10 do
    let a = Array.init (Random.int 1000) (fun _ -> random_uchar ()) in
    let s = Text.init (Array.length a) (Array.get a) in

    (* test for length *)
    let len = Text.length s in
    assert_equal len (Array.length a);

    (* indexing *)
    for i = 0 to Array.length a - 1 do
      assert_equal (Text.get s i) a.(i)
    done;
    
    (* iteration *)
    let r = ref 0 in
    Text.iter (fun u ->
      assert_equal u a.(!r);
      incr r) s;
    assert_equal !r len;
  done


(* stress test *) 
let test_text_random_string () =
  for i = 0 to 100 do
    assert_equal () 
      (let s = random_string () in
      match (try Some (Text.of_string s) with Malformed_code -> None) with
	None -> ()
      | Some text ->
	  for i = 0 to Text.length text - 1 do
	    ignore(Text.get text i)
	  done)
  done
	

(* Test based on "UTF-8 decoder capability and stress test" by
    Markus Kuhn <http://www.cl.cam.ac.uk/~mgk25/> - 2003-02-19 *)

let test_text_valid_utf8_string (name, s, clist) =
  let test () =
    let text = Text.of_string s in
    for i = 0 to List.length clist - 1 do
      let u = Text.get text i in
      let n = List.nth clist i in
      assert_equal ~msg:(Printf.sprintf "character %x != %x" n (UChar.code u)) u  (UChar.chr n);      
    done;
  in
  ("valid string: " ^ name) >:: test


let test_text_invalid_utf8_string s =
  ("invalid string:" ^ (String.escaped s)) >:: (fun () -> assert_raises Malformed_code (fun () -> Text.of_string s))

(* CharEncoding *)

let test_enc_uchar enc u =
  let t = Text.of_uchar u in
  match CharEncoding.encode_text enc t with
    `Error -> assert_failure (Printf.sprintf "Encoding Error:%08x" 
				(UChar.int_of u));
  |`Success s ->
      let t' = CharEncoding.decode_string enc s in
      assert_equal ~msg:(Printf.sprintf "char:%8x" (UChar.int_of u)) 
	(Text.compare t t') 0
      
let random_ascii () =
  let s = String.create (Random.int 1000) in
  for i = 0 to String.length s - 1 do
    s.[i] <- Char.chr (Random.int 0x80)
  done;
  s
 
let test_ascii_valid () =
  for i = 0 to 100 do
    let s = random_ascii () in
    assert_equal 
      (`Success s)
      (CharEncoding.recode CharEncoding.ascii s CharEncoding.ascii) 
  done

let test_ascii_invalid () =
  for i = 0 to 100 do
    let s = random_string () in
    let t = CharEncoding.decode_string CharEncoding.ascii s in

    (* test for length *)
    let len = Text.length t in
    assert_equal len (String.length s);

    (* indexing *)
    for i = 0 to String.length s - 1 do
      if Char.code s.[i]  < 0x80 then
        assert_equal (UChar.code (Text.get t i)) (Char.code s.[i])
      else
        assert_equal (UChar.code (Text.get t i)) 0xfffd
    done;
  done

let test_ascii_repl () =
  let t = Text.of_uchar (UChar.chr 0x1000) in
  assert_equal (`Success "\\u1000")
    (CharEncoding.encode_text ~repl:CharEncoding.repl_escape CharEncoding.ascii t)  


let random_latin1 () =
  let s = String.create (Random.int 1000) in
  for i = 0 to String.length s - 1 do
    s.[i] <- Char.chr (Random.int 0x100)
  done;
  s
 
let test_latin1_valid () =
  for i = 0 to 100 do
    let s = random_latin1 () in
    assert_equal 
      (`Success s)
      (CharEncoding.recode CharEncoding.latin1 s CharEncoding.latin1) 
  done

let test_latin1_repl () =
  let t = Text.of_uchar (UChar.chr 0x1000) in
  assert_equal (`Success "\\u1000")
    (CharEncoding.encode_text ~repl:CharEncoding.repl_escape CharEncoding.latin1 t)  

let random_text n =
  let a = Array.init (Random.int n) (fun _ -> random_uchar ()) in
  let s = Text.init (Array.length a) (Array.get a) in
  s

let random_text_list n = 
  let a = Array.init (Random.int 10) (fun _ -> random_text n) in
  Array.to_list a
  
let test_utf_random_text enc n =
  let text = random_text n in
  let s = match CharEncoding.encode_text enc text with
    `Success s -> s
  | `Error -> assert_failure (Printf.sprintf "Encoding Error:%s" 
				(String.escaped (Text.to_string text))) in
  let text' = CharEncoding.decode_string enc s in
  assert_equal (Text.compare text text') 0

let test_utf8_random n = test_utf_random_text CharEncoding.utf8 n

let chop n s =
  let n = if n > String.length s then String.length s else n in
  let s0 = String.sub s 0 n in
  let s1 = String.sub s n (String.length s - n) in
  (s0, s1)

let test_utf_random_text_list enc n =
  let text = random_text n in
  let s = match CharEncoding.encode_text enc text with
    `Success s -> s
  | `Error -> assert_failure "Encoding Error" in
  let decoder = CharEncoding.create_decoder enc in
  let rec loop decoder s t =
    if s = "" then Text.append t (CharEncoding.terminate_decoder decoder) else
    let s0, s1 = chop 10 s in
    let decoder, t' = CharEncoding.decode decoder s0 in
    loop decoder s1 (Text.append t t') in
  let text' = loop decoder s Text.empty in
  assert_equal 
    ~msg:(Printf.sprintf "\n%s !=\n%s\n %s" (sprint_text text) (sprint_text text') (sprint_string s))
    (Text.compare text text') 0
 
let test_utf8_random_list n = test_utf_random_text_list CharEncoding.utf8 n

let test_decode_valid_utf8 (name, s, clist) =
  let test () =
    let text = CharEncoding.decode_string CharEncoding.utf8 s in
    for i = 0 to List.length clist - 1 do
      let u = Text.get text i in
      let n = List.nth clist i in
      assert_equal ~msg:(Printf.sprintf "character %x != %x" n (UChar.code u)) u  (UChar.chr n);      
    done;
  in
  ("valid string: " ^ name) >:: test


let test_decode_invalid_utf8 s =
  ("invalid string:" ^ (String.escaped s)) >:: 
    (fun () ->
      let text = CharEncoding.decode_string CharEncoding.utf8 s in
      assert_bool s (Text.contains text (UChar.chr 0xfffd)))

(* UTF-16 *)
let test_utf16be_uchar () =
  test_enc_uchar CharEncoding.utf16be (UChar.of_int 0);
  test_enc_uchar CharEncoding.utf16be (UChar.of_int 0xA000);
  test_enc_uchar CharEncoding.utf16be (UChar.of_int 0xFFFD);
  test_enc_uchar CharEncoding.utf16be (UChar.of_int 0x10000);
  test_enc_uchar CharEncoding.utf16be (UChar.of_int 0x10FFFF)

let test_utf16be_invalid_string () =
  let s = "\xd8\x00\xdc\x00\xdc\x10\x00" in
  let t = CharEncoding.decode_string CharEncoding.utf16be s in
  assert_equal ~msg:(sprint_text t) (UChar.code (Text.get t 0)) (0x10000);
  assert_equal ~msg:(sprint_text t) (UChar.code (Text.get t 1)) (0xfffd);
  assert_equal ~msg:(sprint_text t) (UChar.code (Text.get t 2)) (0x1000)

let test_utf16be_random n = test_utf_random_text CharEncoding.utf16be n
let test_utf16be_random_list n = test_utf_random_text_list CharEncoding.utf16be n

let test_utf16le_uchar () =
  test_enc_uchar CharEncoding.utf16le (UChar.of_int 0);
  test_enc_uchar CharEncoding.utf16le (UChar.of_int 0xA000);
  test_enc_uchar CharEncoding.utf16le (UChar.of_int 0xFFFD);
  test_enc_uchar CharEncoding.utf16le (UChar.of_int 0x10000);
  test_enc_uchar CharEncoding.utf16le (UChar.of_int 0x10FFFF)

let test_utf16le_invalid_string () =
  let s = "\x00\xd8\x00\xdc\x00\xdc\x10\x00" in
  let t = CharEncoding.decode_string CharEncoding.utf16le s in
  assert_equal ~msg:(sprint_text t) (UChar.code (Text.get t 0)) (0x10000);
  assert_equal ~msg:(sprint_text t) (UChar.code (Text.get t 1)) (0xfffd);
  assert_equal ~msg:(sprint_text t) (UChar.code (Text.get t 2)) (0x10dc);
  assert_equal ~msg:(sprint_text t) (UChar.code (Text.get t 3)) (0xfffd)

let test_utf16le_random n = test_utf_random_text CharEncoding.utf16le n
let test_utf16le_random_list n = test_utf_random_text_list CharEncoding.utf16le n


let test_utf16_uchar () =
  test_enc_uchar CharEncoding.utf16 (UChar.of_int 0);
  test_enc_uchar CharEncoding.utf16 (UChar.of_int 0xA000);
  test_enc_uchar CharEncoding.utf16 (UChar.of_int 0xFFFD);
  test_enc_uchar CharEncoding.utf16 (UChar.of_int 0x10000);
  test_enc_uchar CharEncoding.utf16 (UChar.of_int 0x10FFFF)

let test_utf16_invalid_string_be () =
  let s = "\xfe\xff\xd8\x00\xdc\x00\xdc\x10\x00" in
  let t = CharEncoding.decode_string CharEncoding.utf16 s in
  assert_equal ~msg:(sprint_text t) (UChar.code (Text.get t 0)) (0x10000);
  assert_equal ~msg:(sprint_text t) (UChar.code (Text.get t 1)) (0xfffd);
  assert_equal ~msg:(sprint_text t) (UChar.code (Text.get t 2)) (0x1000)

let test_utf16_invalid_string_le () =
  let s =  "\xff\xfe\x00\xd8\x00\xdc\x10\xdc\x00" in
  let t = CharEncoding.decode_string CharEncoding.utf16 s in
  assert_equal ~msg:(sprint_text t) (UChar.code (Text.get t 0)) (0x10000);
  assert_equal ~msg:(sprint_text t) (UChar.code (Text.get t 1)) (0xfffd);
  assert_equal ~msg:(sprint_text t) (UChar.code (Text.get t 2)) (0x00dc)

let test_utf16_invalid_string_default () =
  let s = "\xd8\x00\xdc\x00\xdc\x10\x00" in
  let t = CharEncoding.decode_string CharEncoding.utf16 s in
  assert_equal ~msg:(sprint_text t) (UChar.code (Text.get t 0)) (0x10000);
  assert_equal ~msg:(sprint_text t) (UChar.code (Text.get t 1)) (0xfffd);
  assert_equal ~msg:(sprint_text t) (UChar.code (Text.get t 2)) (0x1000)


let test_utf16_random n = test_utf_random_text CharEncoding.utf16 n
let test_utf16_random_list n = test_utf_random_text_list CharEncoding.utf16 n

(* UTF-32 *)
let test_utf32be_uchar () =
  test_enc_uchar CharEncoding.utf32be (UChar.of_int 0);
  test_enc_uchar CharEncoding.utf32be (UChar.of_int 0xA000);
  test_enc_uchar CharEncoding.utf32be (UChar.of_int 0xFFFD);
  test_enc_uchar CharEncoding.utf32be (UChar.of_int 0x10000);
  test_enc_uchar CharEncoding.utf32be (UChar.of_int 0x10FFFF)

let test_utf32be_invalid_string () =
  let s = "\x00\x01\x00\x00\x01\x00\x01\x00\x00" in
  let t = CharEncoding.decode_string CharEncoding.utf32be s in
  assert_equal ~msg:(sprint_text t) (UChar.code (Text.get t 0)) (0x10000);
  assert_equal ~msg:(sprint_text t) (UChar.code (Text.get t 1)) (0xfffd);
  assert_equal ~msg:(sprint_text t) (UChar.code (Text.get t 2)) (0x10000)

let test_utf32be_random n = test_utf_random_text CharEncoding.utf32be n
let test_utf32be_random_list n = test_utf_random_text_list CharEncoding.utf32be n

let test_utf32le_uchar () =
  test_enc_uchar CharEncoding.utf32le (UChar.of_int 0);
  test_enc_uchar CharEncoding.utf32le (UChar.of_int 0xA000);
  test_enc_uchar CharEncoding.utf32le (UChar.of_int 0xFFFD);
  test_enc_uchar CharEncoding.utf32le (UChar.of_int 0x10000);
  test_enc_uchar CharEncoding.utf32le (UChar.of_int 0x10FFFF)

let test_utf32le_invalid_string () =
  let s = "\x00\x00\x01\x00\x00\x01\x00\x01\x00" in
  let t = CharEncoding.decode_string CharEncoding.utf32le s in
  assert_equal ~msg:(sprint_text t) (UChar.code (Text.get t 0)) (0x10000);
  assert_equal ~msg:(sprint_text t) (UChar.code (Text.get t 1)) (0xfffd)

let test_utf32le_random n = test_utf_random_text CharEncoding.utf32le n
let test_utf32le_random_list n = test_utf_random_text_list CharEncoding.utf32le n


let test_utf32_uchar () =
  test_enc_uchar CharEncoding.utf32 (UChar.of_int 0);
  test_enc_uchar CharEncoding.utf32 (UChar.of_int 0xA000);
  test_enc_uchar CharEncoding.utf32 (UChar.of_int 0xFFFD);
  test_enc_uchar CharEncoding.utf32 (UChar.of_int 0x10000);
  test_enc_uchar CharEncoding.utf32 (UChar.of_int 0x10FFFF)

let test_utf32_invalid_string_be () =
  let s = "\x00\x00\xfe\xff\x00\x01\x00\x00\x01\x00\x00\x10\x00" in
  let t = CharEncoding.decode_string CharEncoding.utf32 s in
  assert_equal ~msg:(sprint_text t) (UChar.code (Text.get t 0)) (0x10000);
  assert_equal ~msg:(sprint_text t) (UChar.code (Text.get t 1)) (0xfffd);
  assert_equal ~msg:(sprint_text t) (UChar.code (Text.get t 2)) (0x1000)

let test_utf32_invalid_string_le () =
  let s = "\xff\xfe\x00\x00\x00\x00\x01\x00\x01\x00\x01\x00\x00" in
  let t = CharEncoding.decode_string CharEncoding.utf32 s in
  assert_equal ~msg:(sprint_text t) (UChar.code (Text.get t 0)) (0x10000);
  assert_equal ~msg:(sprint_text t) (UChar.code (Text.get t 1)) (0x010001);
  assert_equal ~msg:(sprint_text t) (UChar.code (Text.get t 2)) (0xfffd)

let test_utf32_invalid_string_default () =
  let s = "\x00\x01\x00\x00\x01\x00\x00\x10\x00" in
  let t = CharEncoding.decode_string CharEncoding.utf32 s in
  assert_equal ~msg:(sprint_text t) (UChar.code (Text.get t 0)) (0x10000);
  assert_equal ~msg:(sprint_text t) (UChar.code (Text.get t 1)) (0xfffd);
  assert_equal ~msg:(sprint_text t) (UChar.code (Text.get t 2)) (0x1000)


let test_utf32_random n = test_utf_random_text CharEncoding.utf32 n
let test_utf32_random_list n = test_utf_random_text_list CharEncoding.utf32 n


let suite = 
  "ucorelib test" >:::
    [ "test UChar" >:::
        ["chr<->uchar" >:::
            ["uchar<-char" >:: test_char1;
             "char<-uchar" >:: test_char2];
         "uchar<->code" >:: test_int_uchar;
         "test_uchar_eq" >:: test_uchar_eq;
         "test_uchar_compare" >:: test_uchar_compare];
     "test UTF8" >::: 
       ["random test :10" >:: (fun () -> test_utf8_random 10);
	"random test :100" >:: (fun () -> test_utf8_random 100);
        "random string test" >:: test_utf8_random_string;
        "valid strings" >::: (List.map test_utf8_valid_utf8_string utf8_valid_pairs);
        "invalid strings" >::: (List.map test_utf8_invalid_utf8_string utf8_brokens)];
     "test Text" >::: 
       ["random test" >:: test_text_random;
        "random string test" >:: test_text_random_string;
        "valid strings" >::: (List.map test_text_valid_utf8_string utf8_valid_pairs);
        "invalid strings" >::: (List.map test_text_invalid_utf8_string utf8_brokens)];
     "test Character Encodings" >:::
       [ "test ascii valid" >:: test_ascii_valid;
        "test ascii invalid" >:: test_ascii_invalid;
        "test ascii repl" >:: test_ascii_repl;
        "test latin1 valid" >:: test_latin1_valid;
        "test latin1 repl" >:: test_latin1_repl;
        "test utf8 random text: 10" >:: (fun () -> test_utf8_random 10);
        "test utf8 random text: 100" >:: (fun () -> test_utf8_random 100);
        "test utf8 random text list: 10" >:: (fun () -> test_utf8_random_list 10);
        "test utf8 random text list: 100" >:: (fun () -> test_utf8_random_list 100);
        "valid strings" >::: (List.map test_decode_valid_utf8 utf8_valid_pairs);
        "invalid strings" >::: (List.map test_decode_invalid_utf8 utf8_brokens);
        "test utf16be random uchar" >:: test_utf16be_uchar;
	"test utf16be invalid string" >:: test_utf16be_invalid_string;
        "test utf16be random text: 10" >:: (fun () -> test_utf16be_random 10);
        "test utf16be random text: 100" >:: (fun () -> test_utf16be_random 100);
        "test utf16be random text list: 10" >:: (fun () -> test_utf16be_random_list 10);
        "test utf16be random text list: 100" >:: (fun () -> test_utf16be_random_list 100);
        "test utf16le random uchar" >:: test_utf16le_uchar;
	"test utf16le invalid string" >:: test_utf16le_invalid_string;
        "test utf16le random text: 10" >:: (fun () -> test_utf16le_random 10);
        "test utf16le random text: 100" >:: (fun () -> test_utf16le_random 100);
        "test utf16le random text list: 10" >:: (fun () -> test_utf16le_random_list 10);
        "test utf16le random text list: 100" >:: (fun () -> test_utf16le_random_list 100);
        "test utf16 random uchar" >:: test_utf16_uchar;
	"test utf16 invalid be string" >:: test_utf16_invalid_string_be;
	"test utf16 invalid le string" >:: test_utf16_invalid_string_le;
	"test utf16 invalid default string" >:: test_utf16_invalid_string_default;
        "test utf16 random text: 10" >:: (fun () -> test_utf16_random 10);
        "test utf16 random text: 100" >:: (fun () -> test_utf16_random 100);
        "test utf16 random text list: 10" >:: (fun () -> test_utf16_random_list 10);
        "test utf16 random text list: 100" >:: (fun () -> test_utf16_random_list 100);
        "test utf32be random uchar" >:: test_utf32be_uchar;
	"test utf32be invalid string" >:: test_utf32be_invalid_string;
        "test utf32be random text: 10" >:: (fun () -> test_utf32be_random 10);
        "test utf32be random text: 100" >:: (fun () -> test_utf32be_random 100);
        "test utf32be random text list: 10" >:: (fun () -> test_utf32be_random_list 10);
         "test utf32be random text list: 100" >:: (fun () -> test_utf32be_random_list 100);
        "test utf32le random uchar" >:: test_utf32le_uchar;
	"test utf32le invalid string" >:: test_utf32le_invalid_string;
        "test utf32le random text: 10" >:: (fun () -> test_utf32le_random 10);
        "test utf32le random text: 100" >:: (fun () -> test_utf32le_random 100);
        "test utf32le random text list: 10" >:: (fun () -> test_utf32le_random_list 10);
        "test utf32le random text list: 100" >:: (fun () -> test_utf32le_random_list 100);
        "test utf32 random uchar" >:: test_utf32_uchar;
	"test utf32 invalid be string" >:: test_utf32_invalid_string_be;
	"test utf32 invalid le string" >:: test_utf32_invalid_string_le; 
	"test utf32 invalid default string" >:: test_utf32_invalid_string_default;
        "test utf32 random text: 10" >:: (fun () -> test_utf32_random 10);
        "test utf32 random text: 100" >:: (fun () -> test_utf32_random 100);
        "test utf32 random text list: 10" >:: (fun () -> test_utf32_random_list 10);
        "test utf32 random text list: 100" >:: (fun () -> test_utf32_random_list 100);
      ]]

let _ = 
  run_test_tt_main suite

