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

(* No arrays involved in these tests though. *)
let easy_string_tests = [
  (* Testing with built-ins *)
  ("hw4programs/my_tests/string_1.oat", "", "0")
; ("hw4programs/my_tests/string_2.oat", "", "5")
; ("hw4programs/my_tests/string_3.oat", "", "10")
; ("hw4programs/my_tests/string_4.oat", "", "2")
; ("hw4programs/my_tests/string_5.oat", "", "10")
]

(* Relies only on global arrays *)
let easy_index_tests = [
  (* Simple indexing into globals, diff types *)
  ("hw4programs/my_tests/index_1.oat", "", "1")
; ("hw4programs/my_tests/index_2.oat", "", "2")
; ("hw4programs/my_tests/index_3.oat", "", "10")
; ("hw4programs/my_tests/index_4.oat", "", "10")
; ("hw4programs/my_tests/index_5.oat", "", "10")
; ("hw4programs/my_tests/index_6.oat", "", "10")
  (* e1[e2] -> Complex e2 *)
; ("hw4programs/my_tests/index_7.oat", "", "2")
; ("hw4programs/my_tests/index_8.oat", "", "3")
; ("hw4programs/my_tests/index_9.oat", "", "20")
; ("hw4programs/my_tests/index_10.oat", "", "0")
; ("hw4programs/my_tests/index_11.oat", "", "2")
]

(* Assignment but for arrays using indices. *)
(* Uses only global arrays. *)
let easy_indassign_tests = [
  ("hw4programs/my_tests/indassign_1.oat", "", "30")
; ("hw4programs/my_tests/indassign_2.oat", "", "0")
; ("hw4programs/my_tests/indassign_3.oat", "", "40")
; ("hw4programs/my_tests/indassign_4.oat", "", "4")
; ("hw4programs/my_tests/indassign_5.oat", "", "50")
]

(* Default initialized arrays. No other types besides *)
(* integers and booleans are allowed. *)
let easy_new_arr_tests = [
  (* Integer Arrays *)
  ("hw4programs/my_tests/newarr_1.oat", "", "1")
; ("hw4programs/my_tests/newarr_2.oat", "", "0")
; ("hw4programs/my_tests/newarr_3.oat", "", "0")
; ("hw4programs/my_tests/newarr_4.oat", "", "1")
; ("hw4programs/my_tests/newarr_5.oat", "", "6")
  (* Boolean Arrays *)
; ("hw4programs/my_tests/newarr_6.oat", "", "0")
; ("hw4programs/my_tests/newarr_7.oat", "", "10")
; ("hw4programs/my_tests/newarr_8.oat", "", "10")
; ("hw4programs/my_tests/newarr_9.oat", "", "0")
; ("hw4programs/my_tests/newarr_10.oat", "", "10")
]

let easy_carr_tests = [
  (* Basic Data Types - 1D Arrays *)
  ("hw4programs/my_tests/carr_1.oat", "", "2")
; ("hw4programs/my_tests/carr_2.oat", "", "20")
; ("hw4programs/my_tests/carr_3.oat", "", "foo0")
; ("hw4programs/my_tests/carr_4.oat", "", "baz4")
  (* 2D Arrays *)
; ("hw4programs/my_tests/carr_5.oat", "", "24")
; ("hw4programs/my_tests/carr_6.oat", "", "10")
; ("hw4programs/my_tests/carr_7.oat", "", "false0")
  (* 3D Arrays *)
; ("hw4programs/my_tests/carr_8.oat", "", "5")
; ("hw4programs/my_tests/carr_9.oat", "", "5")
; ("hw4programs/my_tests/carr_10.oat", "", "10")
]

let global_string_tests = [
  (* Testing with built-ins *)
  ("hw4programs/my_tests/glbl_string_1.oat", "", "0")
; ("hw4programs/my_tests/glbl_string_2.oat", "", "5")
; ("hw4programs/my_tests/glbl_string_3.oat", "", "10")
; ("hw4programs/my_tests/glbl_string_4.oat", "", "10")
; ("hw4programs/my_tests/glbl_string_5.oat", "", "foo0")
; ("hw4programs/my_tests/glbl_string_6.oat", "", "foobar0")
]

let global_int_tests = [
  ("hw4programs/my_tests/glbl_int_1.oat", "", "42")
; ("hw4programs/my_tests/glbl_int_2.oat", "", "42")
; ("hw4programs/my_tests/glbl_int_3.oat", "", "4")
; ("hw4programs/my_tests/glbl_int_4.oat", "", "4")
]

let global_bool_tests = [
  ("hw4programs/my_tests/glbl_bool_1.oat", "", "2")
; ("hw4programs/my_tests/glbl_bool_2.oat", "", "2")
; ("hw4programs/my_tests/glbl_bool_3.oat", "", "2")
; ("hw4programs/my_tests/glbl_bool_4.oat", "", "2")
; ("hw4programs/my_tests/glbl_bool_5.oat", "", "2")
]

let global_array_tests = [
  ("hw4programs/my_tests/glbl_array_1.oat", "", "2")
; ("hw4programs/my_tests/glbl_array_2.oat", "", "5")
; ("hw4programs/my_tests/glbl_array_3.oat", "", "10")
  (* 2D *)
; ("hw4programs/my_tests/glbl_array_4.oat", "", "0")
  (* 3D *)
; ("hw4programs/my_tests/glbl_array_8.oat", "", "5")

]

let rand = [
  ("hw4programs/austin.oat", "", "0")
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
  Test (">>> easy index assign tests", executed_oat_file easy_indassign_tests);
  Test (">>> easy new array tests", executed_oat_file easy_new_arr_tests);
  Test (">>> easy carr tests", executed_oat_file easy_carr_tests);
  Test (">>> global string tests", executed_oat_file global_string_tests);
  Test (">>> global int tests", executed_oat_file global_int_tests);
  Test (">>> global bool tests", executed_oat_file global_bool_tests);
  Test (">>> global array tests", executed_oat_file global_array_tests);
  Test (">>> rand", executed_oat_file rand);
] 
