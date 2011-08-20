open OUnit
open Ulib

(* Helpers *)

let try_chr n = try Some (UChar.chr n) with Out_of_range -> None

let sgn n = if n < 0 then -1 else if n = 0 then 0 else 1

let rec range i j = 
  if i > j then [] else
  i :: range (i + 1) j

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

(* IO modules *)

let sample_text = "一章： 道，可道，非常道；名，可名，非常名。无名，天地始；有名，万物母。

常无，欲 观其妙；常有，欲观其徼。此两者同出而异名，同谓之玄，玄之又玄，众妙之门。


二章： 天下皆知美之为美，斯恶已；皆知善之为善，斯不善已。故有无相生，

难易相成， 长短相形，高下相倾，音声相和，前后相随。是以圣人处无为之事，行不言

之教。 万物作而不辞，生而不有，为而不恃，成功不居。夫唯不居，是以不去。

 

三章： 不上贤，使民不争；不贵难得之货，使民不盗；不见可欲，使心不乱。圣人治：

虚 其心，实其腹，弱其志，强其骨。常使民无知无欲，使知者不敢为，则无不治。


四章： 道冲，而用之久不盈。深乎！万物宗。挫其锐，解其忿，和其光，同其尘。

湛常存。 吾不知谁子？象帝之先。


五章： 天地不仁，以万物为刍狗；圣人不仁，以百姓为刍狗。天地之间，其犹橐龠 。

虚而 不屈，动而俞出。多言数穷，不如守中。


六章： 谷神不死，是谓玄牝。玄牝门，天地根。绵绵若存，用之不勤。

七章： 天长地久。天地所以能长久者，以其不自生，故能长久。是以圣人后其身而身先，

外其身而身存。以其无私，故能成其私。


八章： 上善若水。水善利万物，又不争。处众人之所恶，故几于道。

居善地，心善渊，与 善人，言善信，政善治，事善能，动善时。夫唯不争，故无尤。


九章： 持而盈之，不若其以。揣而锐之，不可长保。金玉满堂，莫之能守。富贵而骄，

自 遗其咎。功成、名遂、身退，天之道。


十章： 载营魄抱一，能无离？专气致柔，能婴儿？涤除玄览，能无疵？

爱人治国，能无为？ 天门开阖，能为雌？明白四达，能无知？生之畜之，

生而不有，为而不恃，长而不 宰，是谓玄德。


十一章： 三十辐共一毂，当其无有，车之用。埏埴以为器，当其无有，器之用。

凿户牖以为 室，当其无有，室之用。有之以为利.

十二章： 五色令人目盲；五音令人耳聋；五味令人口爽；驰骋田猎，令人心发狂；难得之货， 令人行妨。是以圣人为腹不为目。故去彼取此。


十三章： 宠辱若惊，贵大患若身。何谓宠辱？辱为下。得之若惊，失之若惊，是谓宠辱若惊。

何谓贵大患若身？吾所以有大患，为我有身。及我无身，吾有何患！故贵身于天下，

若可托天下；爱以身为天下者，若可寄天下。


十四章： 视之不见，名曰夷；听之不闻，名曰希；抟之不得，名曰微。此三者不可致诘，

故 混而为一。其上不 徼 ，在下不昧。绳绳不可名，复归于无物。是谓无状之状，无物 之象，

是谓忽恍。迎不见其首，随不见其后。执古之道，以语今之有。以知古始， 是谓道已。


十五章： 古之善为士者，微妙玄通，深不可识。夫唯不可识，故强为之容：豫若冬涉川，

犹 若畏四邻，俨若客，涣若冰将释，敦若朴，混若浊，旷若谷。熟能浊以静之？

徐清。 安以动之？徐生。保此道者，不欲盈。夫唯不盈，能弊复成。


十六章： 致虚极，守静笃。万物并作，吾以观其复。夫物云云，各归其根。

归根曰静，静曰 复命，复命曰常，知常曰明。不知常，忘作，凶。

知常容，容能公，公能王，王能 天，天能道，道能久，没身不殆。


十七章： 太上，下知有之；其次，亲之豫之；其次，畏之侮之。信不足，

有不信！由其贵言。成功事遂，百姓谓我自然。


十八章： 大道废，有人义。智惠出，有大伪。六亲不和，有孝慈。国家昏乱，有忠臣。


十九章： 绝圣弃智，民利百倍；绝民弃义，民复孝慈；绝巧弃利，盗贼无有。

此三者，为文 不足，故令有所属：见素抱朴，少私寡欲。


二十章： 绝学无忧。唯之与阿，相去几何？善之与恶，相去何若？人之所畏，不可不畏。

忙 兮其未央！众人熙熙，若享太牢，若春登台。我魄未兆，若婴儿未孩。乘乘无所归！

众人皆有余，我独若遗。我愚人之心，纯纯。俗人昭昭，我独若昏。俗人察察，

我 独闷闷。淡若海，漂无所止。众人皆有已，我独顽似鄙。我独异于人，而贵食母。


二十一章： 孔得之容，唯道是从。道之为物，唯恍唯忽。忽恍中有象，恍忽中有物。

真冥中有 精，其精甚真，其中有信。自古及今，其名不去，以阅众甫。吾何以知众甫之然？ 以此。


二十二章： 曲则全，枉则正；洼 则盈，弊则新；少则得，多则或。是以圣人抱一为天下式。

不 自见，故明；不自是，故彰；不自伐，故有功；不自矜，故长。夫惟不争，

故天下 莫能与之争。 古之所谓“曲则全”者， 岂虚语？故成全而归之。


二十三章： 希言自然。飘风不终朝，骤雨不终日。熟为此？天地。天地上不能久，而况于人？

故从事而道者，道德之；同于德者，德德之；同于失者，道失之。信不足，有不信。


二十四章： 企者不久，跨者不行，自见不明，自是不彰，自伐无功，自矜不长。其在道，

曰余 食赘行，物或有恶之，故有道不处。


二十五章： 有物混成，先天地生。寂漠！独立不改，周行不殆，可以为天下母。吾不知其名，

强字之曰道，强为之名曰大。大曰逝，逝曰远，远曰返。道大，天大，

地大，王大。 域中有四大，而王处一。人法地，地法天，天法道，道法自然。


二十六章： 重为轻根，静为躁君。是以君子终日行，不离辎重，虽有荣观，燕处超然。

如何万 乘之主，以身轻天下？轻则失臣，躁则失君。


二十七章： 善行，无辙迹；善言，无瘕谪；善计，不用筹策；善闭，无关键不可开；

善结，无 绳约不可解。是以圣人常善救人，而无弃人；常善救物，而无弃物。是谓袭明。

善 人，不善人之师；不善人，善人之资。不贵其师，不爱其资，虽知大迷，此谓要妙。


二十八章： 知其雄，守其雌，为天下蹊。为天下蹊，常德不离，复归于婴儿。

知其白，守其黑， 为天下式。常得不忒，复归于无极。知其荣，守其辱，为天下谷。

为天下谷，常得 乃足，复归于朴。朴散为器，圣人用为官长。是以大制无割。


二十九章： 将欲取天下而为之，吾见其不得已。天下神器，不可为。为者败之，执者失之。

夫 物或行或随，或嘘或吹，或强或赢，或接或隳。是以圣人去甚，去奢，去泰。


三十章： 以道作人主者，不以兵强天下，其事好还：师之所处，荆棘生。故善者果而已，

不 以取强。果而勿骄，果而勿矜，果而勿伐，果而不得以，是果而勿强。

物牡则老， 谓之非道，非道早已。


三十一章： 夫佳兵者，不祥之器，物或恶之，故有道不处。君子居则贵左，用兵则贵右。

兵者 不祥之器，非君子之器，不得已而用之，恬淡为上，故不美，若美之，是乐杀人。

夫乐杀者，不可得意于天下。故吉事尚左，凶事尚右。是以偏将军居左，

上将军居 右。杀人众多，以悲哀泣之；战胜，以哀礼处之。


三十二章： 道常无名。朴虽小，天下不敢臣。王侯若能守，万物将自宾。

天地相合，以降甘露， 人莫之令而自均。始制有名。名亦既有，天将知止。

知止不殆。譬道在天下，犹川 谷与江海。


三十三章 ： 知人者智，自知者明。胜人有力，自胜者强。知足者富，强行有志。

不失其所者久， 死而不亡者寿。


三十四章： 大道汜，其可左右。万物恃之以生而不辞，成功不名有。

爱养万物不为主，可名于 大。是以圣人终不为大，故能成其大。


三十五章： 执大象，天下往。往而不害，安平太。乐与饵，过客止。

道出言，淡无味，视不足 见，听不足闻，用不可既。


三十六章： 将欲翕之，必故张之；将欲弱之，必故强之；将欲废之，必固兴之；

将欲夺之，必 固与之。是谓微明。柔胜刚，弱胜强。鱼不可脱于渊，国有利器，不可示人。


三十七章： 道常无为而无不为。侯王若能守，万物将自化。化而欲作，吾将镇之以无名之朴。

无名之朴，亦将不欲。不欲以静，天下将自正。


三十八章： 上德不德，是以有德。下德不失德，是以无德。上德无为而无以为，

下德无为而有以为。上仁为之而无以为，上义为之而有以为。上礼为之而莫之应，

则攘臂而仍之。故失道而后德，失德而后仁，失仁而后义，失义而后礼。夫礼者，忠信之薄，而乱

之首。前识者，道之华，而愚之始。是以大丈夫处其厚不处其薄，居其实不居其华。 故去彼取此。


三十九章： 昔之得一者：天得一以清，地得一以宁，神得一以灵，谷得一以盈，万物得一以生，

侯王得一以为天下正。天无以清，将恐裂；地无以宁，将恐发；神无以灵，将恐歇；

谷无以盈，将恐竭；万物无以生，将恐灭；侯王无以贞，将恐蹶。故贵以贱为本，

高以下为基。是以侯王自谓孤、寡、不毂，此其以贱为本耶非？故致数车无车。

不欲琭（lù）琭如玉，落落如石。


四十章： 反者道之动，弱者道之用。天下万物生于有，有生于无。


四十一章： 上士闻道，勤而行之；中士闻道，若存若亡；下士闻道，大笑之。不笑不足以为道。

故建言有之：明道若昧，进道若退，夷道若类，上德若谷，大白若辱，

广德若不足， 建德若偷，质真若渝，大方无隅，大器晚成，大音希声，大象无形。

道隐无名。夫 唯道，善贷且善。


四十二章： 道生一，一生二，二生三，三生万物。万物负阴而抱阳，冲气以为和。

人之所恶， 唯孤、寡、不毂，而王公以为称。故物或损之而益，或益之而损。

人之所教，我亦 教之：强梁者不得其死，吾将以为教父。


四十三章： 天下之至柔，驰骋天下之至坚。无有入于无闻。是以知无为有益。

不言之教，无为 之益，天下希及之。


四十四章： 名与身熟亲？身与货熟多？得与亡熟病？是故甚爱必大费，多藏必厚亡。

故知足不 辱，知止不殆，可以长久。


四十五章： 大成若缺，其用不弊。大盈若冲，其用不穷。大直若屈，大巧若拙，大辩若讷。

躁 胜塞，静胜热，清静以为天下正。


四十六章： 天下有道，却走马以粪；天下无道，戎马生于郊。罪莫大于可欲，

祸莫大于不知足， 罪莫大于欲得。故知足之足，常足。


四十七章： 不出户，知天下；不窥牖，见天道。其出弥远，其知弥近。

是以圣人不行而知，不 见而名，不为而成。


四十八章： 为学日益，为道日损，损之又损之，以至于无为。无为无不为。

取天下常以无事， 及其有事，不足以取天下。


四十九章： 圣人无心，以百姓心为心。善者吾善之，不善者吾亦善之，得善。信者吾信之，

不 信者吾亦信之，得信。圣人在天下，怵怵；为天下，浑其心。百姓皆注其耳目，圣 人皆孩之。


五十章： 出生入死。生之徒十有三，死之徒十有三，人之生，动之死地，十有三。

夫何故？ 以其生生之厚。盖闻善摄生者，陆行不遇虎兕，入军不被甲兵。

兕无所投其角，虎 无所措其爪，兵无所容其刃。夫何故？以其无死地。


五十一章： 道生之，德畜之，物形之，势成之。是以万物莫不尊道而贵德。

道之尊，德之贵， 夫莫之命而常自然。故道生之，德畜之，长之育之，

成之熟之，养之覆之。生而不 有，为而不恃，长而不宰，是谓玄德。


五十二章： 天下有始，以为天下母。既知其母，又知其子。既知其子，复守其母。

没身不殆。 塞其兑，闭其门。终身不勤。开其兑，济其事，终身不救。见小曰明，

守柔曰强。 用其光，复归其明，无遗身殃，是谓习常。


五十三章： 使我介然有知，行于大道，唯施是畏。大道甚夷，而人好 径。朝甚除，

田甚芜，仓 甚虚，服文彩，带利剑，厌饮食，财货有余，是谓盗夸。非道也哉！

 

五十四章： 善建者不拔，善抱者不脱，子孙祭祀不辍。修之身，其乃德真；修之家，

其德有余； 修之乡，其德乃长；修之于国，其德乃丰；修之于天下，其德乃普。

故以身观身， 以家观家，以乡观乡，以国观国，以天下观天下。吾何以知天下之然？以此。


五十五章： 含德之厚，比于赤子。毒虫不螫，猛兽不据， 攫 鸟不搏。骨弱筋柔而握固。

未知牝 牡之合而朘 作，精之至。终日号而不嗄，和之至。知和曰常，知常曰明，

益生曰祥， 心使气曰强。物壮则老，谓之不道，不道早已。


五十六章： 知者不言，言者不知。塞其兑，闭其门，挫其锐，解其忿，

和其光，同其尘，是谓 玄同。故不可得而亲，不可得而疏；不可得而利，

亦不可得而害，不可得而贵，亦 不可得而贱。故为天下贵。


五十七章： 以正治国，以奇用兵，以无事取天下。吾何以知其然？以此。

天下多忌讳，而人弥 贫；人多利器，国家滋昏；人多伎巧，奇物滋起；法物滋彰，盗贼多有。

故圣人云：” 我无为，人自化；我好静，人自正；我无事，人自富；我无欲，人自朴。”


五十八章： 其政闷闷，其人醇醇；其政察察，其人缺缺。祸，福之所倚；福，祸之所伏。

熟知 其极？其无正。政复为奇，善复为妖。人之迷，其日固久。是以圣人方

而不割，廉 而不害，直而不肆，光而不曜。


五十九章： 治人事天，莫若啬。夫唯啬，是谓早服。早服谓之重积德。

重积德则无不克，无不 克则莫知其极。莫知其极，可以有国。有国之母，

可以长久。是谓深根、固柢 、长 生、久视之道。


六十章： 治大国若亨小鲜。以道莅天下，其鬼不神。非其鬼不神，其神不伤人。非其神不伤 人，

圣人亦不伤人。夫两不相伤，故得交归。


六十一章： 大国者下流，天下之交，天下之牝。牡常以静胜牝，以静为下。

故大国以下小国， 则取小国；小国以下大国，则取大国。故或下以取，或下如取。

大国不过欲兼畜人， 小国不过欲入事人。此两者各得其所欲，大者宜为下。


六十二章： 道者，万物之奥。善，人之宝；不善，人之所不保。美言可以市尊，

行可以加人。 人之不善，何弃之有？故立天子，置三公，虽有拱璧以先驷马，

不如坐进此道。古 之所以贵此道者何？不曰求以得，有罪以勉，故为天下贵。


六十三章： 为无为，事无事，味无味。大小多少，报怨以德。图难于易，

为大于细。天下难事， 必作于易；天下大事，必作于细。是以圣人终不为大，

故能成其大。夫轻诺必寡信， 多易必多难，是以圣人犹难之，故终无难。


六十四章： 其安易持，其未兆易谋，其脆易破，其微易散。为之于未有，治之于未乱。

合抱之 木，生于毫末；九层之台，起于累土；千里之行，始于足下。为者败之，

执者失之。 是以圣人无为，故无败；无执，故无失。民之从事，常于几成而败之。

慎终如始， 则无败事。是以圣人欲不欲，不贵难得之货；学不学，复众人之所过。

以辅万物之 自然而不敢为。


六十五章： 古之善为道者，非以明人，将以愚之。民之难治，以其多智。以智治国，

国之贼； 不以智治国，国之福。知此两者，亦揩式。常知揩式，是谓玄德。玄德深远，

与物 反，然后乃至大顺。


六十六章： 江海所以能为百谷王，以其善下之，故能为百谷王。是以圣人欲上人，必以言下之；

欲先人，必以身后之。是以圣人处上而人不重，处前而人不害，是以天下乐推而不 厌。


以其不争，故天下莫与之争。


六十七章： 天下皆谓我大，不肖。夫唯大，故不肖。若肖，久矣其细！我有三宝，持而宝之：

一曰慈，二曰俭，三曰不敢为天下先。夫慈，故能勇；俭，故能广；不敢为天下先， 故能成器长。

今舍慈且勇，舍俭且广，舍后且先，死矣。夫慈，以战则胜，以守则 固。天将救之，以慈卫之。


六十八章： 古之善为士者不武，善战者不怒，善胜敌者不争，善用仁者为下。是谓不争之德，

是以用人之力，是谓配天古之极。


六十九章： 用兵有言：“吾不敢为主而为客，不敢进寸而退尺。”是谓行无行，攘无臂，仍无 敌，

执无兵。祸莫大于轻敌，轻敌几丧吾宝。故抗兵相加，则哀者胜。

七十章： 吾言甚易知，甚易行。天下莫能知，莫能行。言有宗，事有君。

夫唯无知，是以不 我知。知我者希，则我者贵。是以圣人被褐怀玉。


七十一章： 知不知上，不知知，病。是以圣人不病。以其病病，是以不病。


七十二章： 民不畏威，大威至。无狭其所居，无厌其所生。夫唯不厌，是以不厌。

是以圣人自 知不自见，自爱不自贵。故去彼取此。


七十三章： 勇于敢则杀，勇于不敢则活，知此两者或利或害。天之所恶，孰知其故？

天之道， 不争而善胜，不言而善应，不召而自来，坦然而善谋。天网恢恢，疏而不漏。


七十四章： 民不畏死，奈何以死惧之？若使常畏死，而为奇者，吾执得而杀之，熟敢？

常有司 杀者杀。夫代司杀者杀，是谓代大匠 靳。夫代大匠 靳，希有不伤其手。

七十五章： 民之饥，以其上食税之多，是以饥。民之难治，以其上有为，是以难治。

人之轻死，以其生生之厚，是以轻死。夫唯无以生为者，是贤于贵生。


七十六章： 人生之柔弱，其死坚强。万物草木生之柔脆，其死枯槁。故坚强者死之徒，

柔弱者 生之徒。是以兵强则不胜，木强则共。故坚强处下，柔弱处上。


七十七章： 天之道，其犹张弓！高者抑之，下者举之，有余者损之，不足者与之。

天之道，损 有余而补不足；人道则不然，损不足，奉有余。熟能有余以奉天下？

其唯有道者。是以圣人为而不恃，功成不处，斯不见贤。


七十八章： 天下柔弱莫过于水，而攻坚；强莫之能先。其无以易之。

故弱胜强，柔胜刚，天下 莫能知，莫能行。故圣人云：

受国之垢，是谓社稷主；受国不祥，是谓天下王。正言若反。


七十九章： 和大怨，必有余怨，安可以为善？是以圣人执左契，不责于人。

故有德司契，无德 司彻。天道无亲，常与善人。


八十章： 小国寡人，使有什伯之器而不用，使人重死而不远徙。虽有舟 舆，无所乘之；

虽有 甲兵，无所陈之。使民复结绳而用之。甘其食，美其服，

安其居，乐其俗，邻国相 望，鸡狗之声相闻，民至老死，不相往来。


八十一章： 信言不美，美言不信。善者不辩，辩者不善。知者不博，博者不知。圣人不积，

既以为人己愈有，既以与人己愈多。天之道，利而不害。圣人之道，为而不争。"

let test_builtin_channels () =
  let out_chan = open_out_bin "test/Dao.txt" in
  OutChannel.mput out_chan sample_text;
  OutChannel.close out_chan;
  let chan = open_in_bin "test/Dao.txt" in
  let buf = Buffer.create 0 in
  try (while true do
    let s = InChannel.mget chan 10 in
    Buffer.add_string buf s;
  done) with
    End_of_file -> 
      assert_equal (Buffer.contents buf) sample_text

let test_string_channel () =
  let chan = StringInChannel.of_string sample_text in
  let buf = Buffer.create 0 in
  try (while true do
    let s = StringInChannel.mget chan 10 in
    Buffer.add_string buf s;
  done) with
    End_of_file -> 
      assert_equal (Buffer.contents buf) sample_text

let test_buffer_channel () =
  let buf = Buffer.create 0 in
  BufferOutChannel.mput buf sample_text;
  BufferOutChannel.mput buf sample_text;
  assert_equal (Buffer.contents buf) (sample_text ^ sample_text)


let suite = 
  "ulib test" >:::
  ["test UChar" >:::
   ["chr<->uchar" >:::
    ["uchar<-char" >:: test_char1;
     "char<-uchar" >:: test_char2];
    "uchar<->code" >:: test_int_uchar;
    "test_uchar_eq" >:: test_uchar_eq;
    "test_uchar_compare" >:: test_uchar_compare];
   "test UTF8" >::: 
   ["random test" >:: test_utf8_random;
    "random string test" >:: test_utf8_random_string;
    "valid strings" >::: (List.map test_utf8_valid_utf8_string utf8_valid_pairs);
    "invalid strings" >::: (List.map test_utf8_invalid_utf8_string utf8_brokens)];
   "test Text" >::: 
   ["random test" >:: test_text_random;
    "random string test" >:: test_text_random_string;
    "valid strings" >::: (List.map test_text_valid_utf8_string utf8_valid_pairs);
    "invalid strings" >::: (List.map test_text_invalid_utf8_string utf8_brokens)];
   "test IO" >:::
   ["InChannel/OutChannel" >:: test_builtin_channels;
    "StringInChannel" >:: test_string_channel;
    "BufferOutChannel" >:: test_buffer_channel;]
 ]

let _ = 
  run_test_tt_main suite

