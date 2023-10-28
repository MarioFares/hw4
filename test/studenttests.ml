open Util.Assert
open Gradedtests

(* These tests are provided by you -- they will be graded manually *)

(* You should also add additional test cases here to help you   *)
(* debug your program.                                          *)

(* Tests marked as easy were used in early stages of the        *)
(* compiler. Most have a single function, the program function  *)
(* These tests have been used for early stages of building the  *)
(* compiler to ensure the correctness along the way. We are     *)
(* returning expression. *)
(* Note the tests are valid even at the final step of finishing *)
(* the compiler. They are just used to ensure things are right  *)
(* before implementing more features. *)
let easy_ret_tests = [
  ("hw4programs/my_tests/ret_add.oat",  "", "20")
; ("hw4programs/my_tests/ret_sub.oat",  "", "10")
; ("hw4programs/my_tests/ret_mul.oat",  "", "42")
; ("hw4programs/my_tests/ret_iand.oat", "",  "4")
; ("hw4programs/my_tests/ret_ior.oat",  "",  "5")
; ("hw4programs/my_tests/ret_shl.oat",  "",  "4")
; ("hw4programs/my_tests/ret_shr.oat",  "",  "2")
; ("hw4programs/my_tests/ret_sar.oat",  "",  "4")
; ("hw4programs/my_tests/ret_bitnot.oat",  "",  "1")
]

let easy_call_tests = [
  ("hw4programs/my_tests/call_1.oat", "", "10")
; ("hw4programs/my_tests/call_2.oat", "", "11")
]

let easy_decl_tests = [
  ("hw4programs/my_tests/decl_1.oat", "", "9")
; ("hw4programs/my_tests/decl_2.oat", "", "18")
; ("hw4programs/my_tests/decl_3.oat", "", "8")
; ("hw4programs/my_tests/decl_4.oat", "", "3")
; ("hw4programs/my_tests/decl_5.oat", "", "5")
]

let easy_assign_tests = [
  ("hw4programs/my_tests/assign_1.oat", "", "5")
; ("hw4programs/my_tests/assign_2.oat", "", "1")
; ("hw4programs/my_tests/assign_3.oat", "", "30")
; ("hw4programs/my_tests/assign_4.oat", "", "50")
; ("hw4programs/my_tests/assign_5.oat", "", "40")
]

let easy_if_tests = [
  (* Testing True & False as the condition *)
  ("hw4programs/my_tests/if_1.oat", "", "12")
; ("hw4programs/my_tests/if_2.oat", "", "10")
; ("hw4programs/my_tests/if_3.oat", "", "20")
; ("hw4programs/my_tests/if_4.oat", "", "5")
  (* Testing Different Comparison Operators *)
; ("hw4programs/my_tests/if_5.oat", "", "30")
; ("hw4programs/my_tests/if_6.oat", "", "30")
; ("hw4programs/my_tests/if_7.oat", "", "30")
; ("hw4programs/my_tests/if_8.oat", "", "30")
; ("hw4programs/my_tests/if_9.oat", "", "20")
; ("hw4programs/my_tests/if_10.oat", "", "30")
  (* Testing If-Else If-Else Branches Work *)
; ("hw4programs/my_tests/if_11.oat", "", "20")
; ("hw4programs/my_tests/if_12.oat", "", "40")
; ("hw4programs/my_tests/if_13.oat", "", "31")
; ("hw4programs/my_tests/if_14.oat", "", "30")
; ("hw4programs/my_tests/if_15.oat", "", "5")
  (* Testing &, |, ! in Compound Conditiondal *)
; ("hw4programs/my_tests/if_16.oat", "", "5")
; ("hw4programs/my_tests/if_17.oat", "", "10")
; ("hw4programs/my_tests/if_18.oat", "", "50")
; ("hw4programs/my_tests/if_19.oat", "", "5")
]

let easy_while_tests = [
  (* Simple looping and updating *)
  ("hw4programs/my_tests/while_1.oat", "", "5")
; ("hw4programs/my_tests/while_2.oat", "", "1")
; ("hw4programs/my_tests/while_3.oat", "", "50")
; ("hw4programs/my_tests/while_4.oat", "", "50")
; ("hw4programs/my_tests/while_5.oat", "", "20")
; ("hw4programs/my_tests/while_6.oat", "", "0")
; ("hw4programs/my_tests/while_7.oat", "", "10")
; ("hw4programs/my_tests/while_8.oat", "", "0")
; ("hw4programs/my_tests/while_9.oat", "", "11")
]

let easy_for_tests = [
  (* Normal, full-3 for loops *)
  ("hw4programs/my_tests/for_1.oat", "", "5")
; ("hw4programs/my_tests/for_2.oat", "", "0")
; ("hw4programs/my_tests/for_3.oat", "", "5")
  (* Multiple decls for loops *)
; ("hw4programs/my_tests/for_4.oat", "", "19")
  (* No condition *)
; ("hw4programs/my_tests/for_5.oat", "", "10")
; ("hw4programs/my_tests/for_6.oat", "", "10")
  (* No decls *)
; ("hw4programs/my_tests/for_7.oat", "", "10")
; ("hw4programs/my_tests/for_8.oat", "", "0")
  (* No statement *)
; ("hw4programs/my_tests/for_9.oat", "", "10")
; ("hw4programs/my_tests/for_10.oat", "", "0")
  (* Infinite for loops *)
; ("hw4programs/my_tests/for_11.oat", "", "10")
; ("hw4programs/my_tests/for_12.oat", "", "0")
]

let easy_string_tests = [
  (* Testing with built-ins *)
  ("hw4programs/my_tests/string_1.oat", "", "0")
; ("hw4programs/my_tests/string_2.oat", "", "5")
; ("hw4programs/my_tests/string_3.oat", "", "10")
; ("hw4programs/my_tests/string_4.oat", "", "2")
; ("hw4programs/my_tests/string_5.oat", "", "10")
]

let easy_index_tests = [
  (* Simple indexing into globals, diff types *)
  ("hw4programs/my_tests/index_1.oat", "", "1")
; ("hw4programs/my_tests/index_2.oat", "", "2")
; ("hw4programs/my_tests/index_3.oat", "", "10")
; ("hw4programs/my_tests/index_4.oat", "", "10")
]

let provided_tests : suite = [
  Test (">>> easy ret tests", executed_oat_file easy_ret_tests);
  Test (">>> easy call tests", executed_oat_file easy_call_tests);
  Test (">>> easy decl tests", executed_oat_file easy_decl_tests);
  Test (">>> easy assign tests", executed_oat_file easy_assign_tests);
  Test (">>> easy if tests", executed_oat_file easy_if_tests);
  Test (">>> easy while tests", executed_oat_file easy_while_tests);
  Test (">>> easy for tests", executed_oat_file easy_for_tests);
  Test (">>> easy string tests", executed_oat_file easy_string_tests);
  Test (">>> easy index tests", executed_oat_file easy_index_tests);
] 
