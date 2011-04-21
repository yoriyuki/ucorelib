open OUnit;;

let test_char () =
  for i = 0 to 255 do
    let c = Char.chr i in
    let c' = UChar.char_of (UChar.of_char c) in
    assert_equal c c'
  done

let test_int_conversion code chr () =
  assert_equal 0 (code (chr 0));
  assert_equal 0xd7ff (code (chr 0xd7ff));
  assert_raises (Invalid_argument "UChar.code") (fun () -> chr 0xd800);
  assert_raises (Invalid_argument "UChar.code") (fun () -> chr 0xdfff);
  assert_equal 0xe000 (code (chr 0xe000));
  assert_equal 0xffff (code (chr 0xffff));
  assert_equal 0x10000 (code (chr 0x10000));
  assert_equal 0x10ffff (code (chr 0x10ffff));
  assert_raises (Invalid_argument "UChar.code") (fun () -> chr 0x110000);
  assert_raises (Invalid_argument "UChar.code") (fun () -> chr ~-1)

let test_code_chr = test_int_conversion UChar.code UChar.chr

let test_int_of_int = test_int_conversion UChar.int_of UChar.of_int

let test_eq () =
  assert_equal true (UChar.eq (UChar.of_char 'a') (UChar.of_char 'a'));
  assert_equal true (UChar.eq (UChar.chr 0xffff) (UChar.chr 0xffff));
  assert_equal false(UChar.eq (UChar.chr 0xffff) (UChar.chr 0xfffe))

let test_compare () =
  assert_equal 0 (UChar.compare (UChar.of_char 'a') (UChar.of_char 'a'));
  assert_equal 0 (UChar.compare (UChar.chr 0xffff) (UChar.chr 0xffff));
  assert_equal 1 (UChar.compare (UChar.chr 0xffff) (UChar.chr 0xfffe));
  assert_equal ~-1 (UChar.compare (UChar.chr 0xffff) (UChar.chr 0x10000))

let suite = 
  "test UChar" >:::
  ["test int conversion" >:::
   ["chr->code" >:: test_code_chr;
    "of_int->int_of" >:: test_int_of_int;];
   "test_eq" >:: test_eq;
   "test_compare" >:: test_compare]

let _ = 
  run_test_tt_main suite
