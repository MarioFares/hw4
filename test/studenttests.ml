open Util.Assert
open Gradedtests

(* These tests are provided by you -- they will be graded manually *)

(* You should also add additional test cases here to help you   *)
(* debug your program.                                          *)

(* These tests have been used for early stages of building the  *)
(* compiler to ensure the correctness along the way. We are     *)
(* returning expression. *)
let easy_ret_tests = [
  ("hw4programs/my_tests/ret_add.oat",  "", "20")
; ("hw4programs/my_tests/ret_sub.oat",  "", "10")
; ("hw4programs/my_tests/ret_mul.oat",  "", "42")
; ("hw4programs/my_tests/ret_iand.oat", "",  "4")
; ("hw4programs/my_tests/ret_ior.oat",  "",  "5")
]

let provided_tests : suite = [
  Test (">>> easy ret tests", executed_oat_file easy_ret_tests);
] 
