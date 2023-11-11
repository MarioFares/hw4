@arr = global i64* bitcast (i64* @_global_arr14 to { i64, [1 x { i64, [0 x { i64, [0 x i64] }*] }*] }*)
@_global_arr14 = global { i64, [1 x { i64, [0 x { i64, [0 x i64] }*] }*] } { i64 1, [1 x { i64, [0 x { i64, [0 x i64] }*] }*] [ { i64, [0 x { i64, [0 x i64] }*] }* @_glbl13 ] }
@_global_arr12 = global { i64, [1 x { i64, [0 x i64] }*] } { i64 1, [1 x { i64, [0 x i64] }*] [ { i64, [0 x i64] }* @_glbl11 ] }
@_global_arr10 = global { i64, [1 x i64] } { i64 1, [1 x i64] [ i64 1 ] }
@_glbl11 = global i64* bitcast (i64* @_global_arr10 to { i64, [1 x i64] }*)
@_glbl13 = global i64* bitcast (i64* @_global_arr12 to { i64, [1 x { i64, [0 x i64] }*] }*)

define i64 @main(i64 %argc, { i64, [0 x i8*] }* %argv) {
  %_argc1 = alloca i64
  store i64 %argc, i64* %_argc1
  %_argv2 = alloca { i64, [0 x i8*] }*
  store { i64, [0 x i8*] }* %argv, { i64, [0 x i8*] }** %_argv2
  %_id3 = load { i64, [0 x { i64, [0 x { i64, [0 x i64] }*] }*] }*, { i64, [0 x { i64, [0 x { i64, [0 x i64] }*] }*] }** @arr
  %_arr_loc4 = getelementptr { i64, [0 x { i64, [0 x { i64, [0 x i64] }*] }*] }, { i64, [0 x { i64, [0 x { i64, [0 x i64] }*] }*] }* %_id3, i32 0, i32 1, i32 0
  %_index5 = load { i64, [0 x { i64, [0 x i64] }*] }*, { i64, [0 x { i64, [0 x i64] }*] }** %_arr_loc4
  %_arr_loc6 = getelementptr { i64, [0 x { i64, [0 x i64] }*] }, { i64, [0 x { i64, [0 x i64] }*] }* %_index5, i32 0, i32 1, i32 0
  %_index7 = load { i64, [0 x i64] }*, { i64, [0 x i64] }** %_arr_loc6
  %_arr_loc8 = getelementptr { i64, [0 x i64] }, { i64, [0 x i64] }* %_index7, i32 0, i32 1, i32 0
  %_index9 = load i64, i64* %_arr_loc8
  ret i64 %_index9
}