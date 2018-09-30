open OUnit2;;

(* Status:
 Currently, this is checking the registers are correct.
 In future, it should check that the output is correct, also.
 *)

let print_expected s =
    print_endline "Expected:";
    Array.iter (Printf.printf ":%d ") s;
    print_newline ()

let print_actual s =
    print_endline "Actual:";
    Array.iter (Printf.printf ":%d ") s;
    print_newline ()

let print_result title expected actual =
    print_endline title;
    print_expected expected;
    print_actual actual

(* Test 1: Must print 5432 *)
let test1 test_ctxt = 
    let test_case = "+++++.>++++.>+++.>+++++---." in
    let expected = [|5;4;3;2;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0|] in
    let actual = (Brainfucaml.parse test_case) in
    
    print_result "TEST ONE..." expected actual;
    assert_equal actual expected

(* Test 2: Must print 50 *)
let test2 test_ctxt =
    let test_case = "> +++++ [ > +++++ +++++ < - ] > ." in
    let expected = [|0;0;50;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0|] in
    let actual = (Brainfucaml.parse test_case) in

    print_result "TEST TWO..." expected actual;
    assert_equal actual expected

(* Test 3: Must print nothing *)
let test3 test_ctxt = 
    let test_case = 
"dinkadoodledoo 
bacharest" in
    let expected = [|0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0|] in
    let actual = (Brainfucaml.parse test_case) in

    print_result "TEST THREE..." expected actual;
    assert_equal actual expected

(* Test 4: Must print 20 *)
let test4 test_ctxt = 
    let test_case = "> ++ [ > +++++ +++++ < - ] > ." in
    let expected = [|0;0;20;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0|] in
    let actual = (Brainfucaml.parse test_case) in

    print_result "TEST FOUR..." expected actual;
    assert_equal actual expected

(* Test 5: Must print 72 101 108 108 111 ("Hello" without spaces) *)
let test5 test_ctxt = 
    let test_case = "
> +++++++ [ > +++++ +++++ < - ] > ++ .
> +++++ +++++ [ > +++++ +++++ < - ] > + .
> +++++ +++++ [ > +++++ +++++ + < - ] > -- .
> +++++ +++++ [ > +++++ +++++ + < - ] > -- .
> +++++ +++++ [ > +++++ +++++ + < - ] > + .
" in
    let expected = [|0;0;72;0;101;0;108;0;108;0;111;0;0;0;0;0;0;0;0;0|] in
    let actual = (Brainfucaml.parse test_case) in

    print_result "TEST FIVE..." expected actual;
    assert_equal actual expected

(* The following couple of tests come from here:
 *  https://github.com/rdebath/Brainfuck/blob/master/bitwidth.b
 *)

(* Test 6: should set first register to 256 *)
let test6 test_ctxt = 
    let test_case = "++++++++[>++++++++<-]>[<++++>-]
+<[>-<" in
    let expected = [|256;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0|] in
    let actual = (Brainfucaml.parse test_case) in

    print_result "TEST SIX..." expected actual;
    assert_equal actual expected

(* Test 7: set the first register to 1, the second to 65536 *)
let test7 test_ctxt =
    let test_case = "++++++++[>++++++++<-]>[<++++>-]
+<[>-<
    [>++++<-]>[<++++++++>-]<[>++++++++<-]
+>[>" in
    let expected = [|1;65536;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0|] in
    let actual = (Brainfucaml.parse test_case) in

    print_result "TEST SEVEN..." expected actual;
    assert_equal actual expected

(* Test 8: Should print A *)
let test8 test_ctxt =
    let test_case = "++++++++[>++++++++<-]>+." in
    let expected = [|0;65;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0;0|] in
    let actual = (Brainfucaml.parse test_case) in

    print_result "TEST EIGHT..." expected actual;
    assert_equal actual expected

let suite = 
    "suite">:::
    [
        "test1">:: test1;
        "test2">:: test2;
        "test3">:: test3;
        "test4">:: test4;
        "test5">:: test5;
        "test6">:: test6;
        "test7">:: test7;
        "test8">:: test8
    ]

let () = 
    run_test_tt_main suite

