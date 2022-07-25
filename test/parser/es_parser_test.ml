open OUnit2
open Es_parser

let tests = "test lexer" >::: [
  "ShebangTrivia" >:: (fun _ -> 
    let lexer = init_lexer ~text:"#! node" in
    
    assert_equal (scan lexer) ShebangTrivia;
  );
  "WhitespaceTrivia" >:: (fun _ ->
    let lexer = init_lexer ~text:"\t" in
    assert_equal (scan lexer) WhitespaceTrivia;

    let lexer = init_lexer ~text:"  " in
    assert_equal (scan lexer) WhitespaceTrivia;

    let lexer = init_lexer ~text:"\t " in
    assert_equal (scan lexer) WhitespaceTrivia;

    (* Maybe should test exotic whitespace? *)
  );
  "StringLiteral" >:: (fun _ ->
    let lexer = init_lexer ~text:{|''|} in
    assert_equal (scan lexer) StringLiteral;
    assert_equal "" lexer.token_string_value ;

    let lexer = init_lexer ~text:{|'ascii'|} in
    assert_equal (scan lexer) StringLiteral;
    assert_equal "ascii" lexer.token_string_value ;

    let lexer = init_lexer ~text:{|"ascii"|} in
    assert_equal (scan lexer) StringLiteral;
    assert_equal "ascii" lexer.token_string_value ;

    let lexer = init_lexer ~text:{|'中文'|} in
    assert_equal (scan lexer) StringLiteral;
    assert_equal "中文" lexer.token_string_value ;

    let lexer = init_lexer ~text:{|"ascii with escaped double quote \""|} in
    assert_equal (scan lexer) StringLiteral;
    assert_equal {|ascii with escaped double quote "|} lexer.token_string_value  ~printer:(fun x -> x);

    let lexer = init_lexer ~text:{|'ascii with escaped double quote \" and text after it'|} in
    assert_equal (scan lexer) StringLiteral;
    assert_equal {|ascii with escaped double quote " and text after it|} lexer.token_string_value  ~printer:(fun x -> x);

    let lexer = init_lexer ~text:{|"\0\b\t\n\v\f\r\'\""|} in
    assert_equal (scan lexer) StringLiteral;
    assert_equal "\x00\b\t\n\x0b\x0c\r\'\"" lexer.token_string_value  ~printer:(fun x -> x);

  )
]

let _ = run_test_tt_main tests
