
module PT = Parser_tests

let all_test_cases =
  PT.test_cases (* @ PT.test_cases *)

let () =
  Alcotest.run "Parser Tests" all_test_cases



