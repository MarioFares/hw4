open Ll
open Llutil
open Ast

(* instruction streams ------------------------------------------------------ *)

(* As in the last project, we'll be working with a flattened representation
   of LLVMlite programs to make emitting code easier. This version
   additionally makes it possible to emit elements that will be gathered up and
   "hoisted" to specific parts of the constructed CFG
   - G of gid * Ll.gdecl: allows you to output global definitions in the middle
     of the instruction stream. You will find this useful for compiling string
     literals
   - E of uid * insn: allows you to emit an instruction that will be moved up
     to the entry block of the current function. This will be useful for 
     compiling local variable declarations
*)

type elt = 
  | L of Ll.lbl             (* block labels *)
  | I of uid * Ll.insn      (* instruction *)
  | T of Ll.terminator      (* block terminators *)
  | G of gid * Ll.gdecl     (* hoisted globals (usually strings) *)
  | E of uid * Ll.insn      (* hoisted entry block instructions *)

(* The type of streams of LLVMLite instructions. Note that to improve performance,
 * we will emit the instructions in reverse order. That is, the LLVMLite code:
 *     %1 = mul i64 2, 2
 *     %2 = add i64 1, %1
 *     br label %l1
 * would be constructed as a stream as follows:
 *     I ("1", Binop (Mul, I64, Const 2L, Const 2L))
 *     >:: I ("2", Binop (Add, I64, Const 1L, Id "1"))
 *     >:: T (Br "l1")
 *)
type stream = elt list
let ( >@ ) x y = y @ x
let ( >:: ) x y = y :: x
let lift : (uid * insn) list -> stream = List.rev_map (fun (x,i) -> I (x,i))

(* Build a CFG and collection of global variable definitions from a stream *)
let cfg_of_stream (code:stream) : Ll.cfg * (Ll.gid * Ll.gdecl) list  =
    let gs, einsns, insns, term_opt, blks = List.fold_left
      (fun (gs, einsns, insns, term_opt, blks) e ->
        match e with
        | L l ->
           begin match term_opt with
           | None -> 
              if (List.length insns) = 0 then (gs, einsns, [], None, blks)
              else failwith @@ Printf.sprintf "build_cfg: block labeled %s has\
                                               no terminator" l
           | Some term ->
              (gs, einsns, [], None, (l, {insns; term})::blks)
           end
        | T t  -> (gs, einsns, [], Some (Llutil.Parsing.gensym "tmn", t), blks)
        | I (uid,insn)  -> (gs, einsns, (uid,insn)::insns, term_opt, blks)
        | G (gid,gdecl) ->  ((gid,gdecl)::gs, einsns, insns, term_opt, blks)
        | E (uid,i) -> (gs, (uid, i)::einsns, insns, term_opt, blks)
      ) ([], [], [], None, []) code
    in
    match term_opt with
    | None -> failwith "build_cfg: entry block has no terminator" 
    | Some term -> 
       let insns = einsns @ insns in
       ({insns; term}, blks), gs


(* compilation contexts ----------------------------------------------------- *)

(* To compile OAT variables, we maintain a mapping of source identifiers to the
   corresponding LLVMlite operands. Bindings are added for global OAT variables
   and local variables that are in scope. *)

module Ctxt = struct

  type t = (Ast.id * (Ll.ty * Ll.operand)) list
  let empty = []

  (* Add a binding to the context *)
  let add (c:t) (id:id) (bnd:Ll.ty * Ll.operand) : t = (id,bnd)::c

  (* Lookup a binding in the context *)
  let lookup (id:Ast.id) (c:t) : Ll.ty * Ll.operand =
    List.assoc id c

  (* Lookup a function, fail otherwise *)
  let lookup_function (id:Ast.id) (c:t) : Ll.ty * Ll.operand =
    match List.assoc id c with
    | Ptr (Fun (args, ret)), g -> Ptr (Fun (args, ret)), g
    | _ -> failwith @@ id ^ " not bound to a function"

  let lookup_function_option (id:Ast.id) (c:t) : (Ll.ty * Ll.operand) option =
    try Some (lookup_function id c) with _ -> None
  
end

(* compiling OAT types ------------------------------------------------------ *)

(* The mapping of source types onto LLVMlite is straightforward. Booleans and ints
   are represented as the corresponding integer types. OAT strings are
   pointers to bytes (I8). Arrays are the most interesting type: they are
   represented as pointers to structs where the first component is the number
   of elements in the following array.

   The trickiest part of this project will be satisfying LLVM's rudimentary type
   system. Recall that global arrays in LLVMlite need to be declared with their
   length in the type to statically allocate the right amount of memory. The 
   global strings and arrays you emit will therefore have a more specific type
   annotation than the output of cmp_rty. You will have to carefully bitcast
   gids to satisfy the LLVM type checker.
*)

let rec cmp_ty : Ast.ty -> Ll.ty = function
  | Ast.TBool  -> I1
  | Ast.TInt   -> I64
  | Ast.TRef r -> Ptr (cmp_rty r)

and cmp_rty : Ast.rty -> Ll.ty = function
  | Ast.RString  -> I8
  | Ast.RArray u -> Struct [I64; Array(0, cmp_ty u)]
  | Ast.RFun (ts, t) -> 
      let args, ret = cmp_fty (ts, t) in
      Fun (args, ret)

and cmp_ret_ty : Ast.ret_ty -> Ll.ty = function
  | Ast.RetVoid  -> Void
  | Ast.RetVal t -> cmp_ty t

and cmp_fty (ts, r) : Ll.fty =
  List.map cmp_ty ts, cmp_ret_ty r


let typ_of_binop : Ast.binop -> Ast.ty * Ast.ty * Ast.ty = function
  | Add | Mul | Sub | Shl | Shr | Sar | IAnd | IOr -> (TInt, TInt, TInt)
  | Eq | Neq | Lt | Lte | Gt | Gte -> (TInt, TInt, TBool)
  | And | Or -> (TBool, TBool, TBool)

let typ_of_unop : Ast.unop -> Ast.ty * Ast.ty = function
  | Neg | Bitnot -> (TInt, TInt)
  | Lognot       -> (TBool, TBool)

(* Compiler Invariants

   The LLVM IR type of a variable (whether global or local) that stores an Oat
   array value (or any other reference type, like "string") will always be a
   double pointer.  In general, any Oat variable of Oat-type t will be
   represented by an LLVM IR value of type Ptr (cmp_ty t).  So the Oat variable
   x : int will be represented by an LLVM IR value of type i64*, y : string will
   be represented by a value of type i8**, and arr : int[] will be represented
   by a value of type {i64, [0 x i64]}**.  Whether the LLVM IR type is a
   "single" or "double" pointer depends on whether t is a reference type.

   We can think of the compiler as paying careful attention to whether a piece
   of Oat syntax denotes the "value" of an expression or a pointer to the
   "storage space associated with it".  This is the distinction between an
   "expression" and the "left-hand-side" of an assignment statement.  Compiling
   an Oat variable identifier as an expression ("value") does the load, so
   cmp_exp called on an Oat variable of type t returns (code that) generates a
   LLVM IR value of type cmp_ty t.  Compiling an identifier as a left-hand-side
   does not do the load, so cmp_lhs called on an Oat variable of type t returns
   and operand of type (cmp_ty t)*.  Extending these invariants to account for
   array accesses: the assignment e1[e2] = e3; treats e1[e2] as a
   left-hand-side, so we compile it as follows: compile e1 as an expression to
   obtain an array value (which is of pointer of type {i64, [0 x s]}* ).
   compile e2 as an expression to obtain an operand of type i64, generate code
   that uses getelementptr to compute the offset from the array value, which is
   a pointer to the "storage space associated with e1[e2]".

   On the other hand, compiling e1[e2] as an expression (to obtain the value of
   the array), we can simply compile e1[e2] as a left-hand-side and then do the
   load.  So cmp_exp and cmp_lhs are mutually recursive.  [[Actually, as I am
   writing this, I think it could make sense to factor the Oat grammar in this
   way, which would make things clearerhw, I may do that for next time around.]]

 
   Consider globals7.oat (in hw4programs)

   /--------------- globals7.oat ------------------ 
   global arr = int[] null;

   int foo() { 
     var x = new int[3]; 
     arr = x; 
     x[2] = 3; 
     return arr[2]; 
   }
   /------------------------------------------------

   The translation (given by cmp_ty) of the type int[] is {i64, [0 x i64}* so
   the corresponding LLVM IR declaration will look like:

   @arr = global { i64, [0 x i64] }* null

   This means that the type of the LLVM IR identifier @arr is {i64, [0 x i64]}**
   which is consistent with the type of a locally-declared array variable.

   The local variable x would be allocated and initialized by (something like)
   the following code snippet.  Here %_x7 is the LLVM IR uid containing the
   pointer to the "storage space" for the Oat variable x.

   %_x7 = alloca { i64, [0 x i64] }*                              ;; (1)
   %_raw_array5 = call i64*  @oat_alloc_array(i64 3)              ;; (2)
   %_array6 = bitcast i64* %_raw_array5 to { i64, [0 x i64] }*    ;; (3)
   store { i64, [0 x i64]}* %_array6, { i64, [0 x i64] }** %_x7   ;; (4)

   (1) note that alloca uses cmp_ty (int[]) to find the type, so %_x7 has 
       the same type as @arr 

   (2) @oat_alloc_array allocates len+1 i64's 

   (3) we have to bitcast the result of @oat_alloc_array so we can store it
        in %_x7 

   (4) stores the resulting array value (itself a pointer) into %_x7 

  The assignment arr = x; gets compiled to (something like):

  %_x8 = load { i64, [0 x i64] }*, { i64, [0 x i64] }** %_x7     ;; (5)
  store {i64, [0 x i64] }* %_x8, { i64, [0 x i64] }** @arr       ;; (6)

  (5) load the array value (a pointer) that is stored in the address pointed 
      to by %_x7 

  (6) store the array value (a pointer) into @arr 

  The assignment x[2] = 3; gets compiled to (something like):

  %_x9 = load { i64, [0 x i64] }*, { i64, [0 x i64] }** %_x7      ;; (7)
  %_index_ptr11 = getelementptr { i64, [0 x  i64] }, 
                  { i64, [0 x i64] }* %_x9, i32 0, i32 1, i32 2   ;; (8)
  store i64 3, i64* %_index_ptr11                                 ;; (9)

  (7) as above, load the array value that is stored %_x7 

  (8) calculate the offset from the array using GEP

  (9) store 3 into the array

  Finally, return arr[2]; gets compiled to (something like) the following.
  Note that the way arr is treated is identical to x.  (Once we set up the
  translation, there is no difference between Oat globals and locals, except
  how their storage space is initially allocated.)

  %_arr12 = load { i64, [0 x i64] }*, { i64, [0 x i64] }** @arr    ;; (10)
  %_index_ptr14 = getelementptr { i64, [0 x i64] },                
                 { i64, [0 x i64] }* %_arr12, i32 0, i32 1, i32 2  ;; (11)
  %_index15 = load i64, i64* %_index_ptr14                         ;; (12)
  ret i64 %_index15

  (10) just like for %_x9, load the array value that is stored in @arr 

  (11)  calculate the array index offset

  (12) load the array value at the index 

*)

(* Global initialized arrays:

  There is another wrinkle: to compile global initialized arrays like in the
  globals4.oat, it is helpful to do a bitcast once at the global scope to
  convert the "precise type" required by the LLVM initializer to the actual
  translation type (which sets the array length to 0).  So for globals4.oat,
  the arr global would compile to (something like):

  @arr = global { i64, [0 x i64] }* bitcast 
           ({ i64, [4 x i64] }* @_global_arr5 to { i64, [0 x i64] }* ) 
  @_global_arr5 = global { i64, [4 x i64] } 
                  { i64 4, [4 x i64] [ i64 1, i64 2, i64 3, i64 4 ] }

*) 



(* Some useful helper functions *)

(* Generate a fresh temporary identifier. Since OAT identifiers cannot begin
   with an underscore, these should not clash with any source variables *)
let gensym : string -> string =
  let c = ref 0 in
  fun (s:string) -> incr c; Printf.sprintf "_%s%d" s (!c)

(* Amount of space an Oat type takes when stored in the satck, in bytes.  
   Note that since structured values are manipulated by reference, all
   Oat values take 8 bytes on the stack.
*)
let size_oat_ty (t : Ast.ty) = 8L

(* Generate code to allocate a zero-initialized array of source type TRef (RArray t) of the
   given size. Note "size" is an operand whose value can be computed at
   runtime *)
let oat_alloc_array (t:Ast.ty) (size:Ll.operand) : Ll.ty * operand * stream =
  let ans_id, arr_id = gensym "array", gensym "raw_array" in
  let ans_ty = cmp_ty @@ TRef (RArray t) in
  let arr_ty = Ptr I64 in
  ans_ty, Id ans_id, lift
    [ arr_id, Call(arr_ty, Gid "oat_alloc_array", [I64, size])
    ; ans_id, Bitcast(arr_ty, Id arr_id, ans_ty) ]




(* Compiles an expression exp in context c, outputting the Ll operand that will
   recieve the value of the expression, and the stream of instructions
   implementing the expression. 

   Tips:
   - use the provided cmp_ty function!

   - string literals (CStr s) should be hoisted. You'll need to make sure
     either that the resulting gid has type (Ptr I8), or, if the gid has type
     [n x i8] (where n is the length of the string), convert the gid to a 
     (Ptr I8), e.g., by using getelementptr.

   - use the provided "oat_alloc_array" function to implement literal arrays
     (CArr) and the (NewArr) expressions

*)
module Exp = struct 
  let oat_to_llbinop = function 
    | Add -> Ll.Add
    | Sub -> Ll.Sub
    | Mul -> Ll.Mul
    | And -> Ll.And
    | Or -> Ll.Or
    | IAnd -> Ll.And
    | IOr -> Ll.Or 
    | Shl -> Ll.Shl
    | Shr -> Ll.Lshr
    | Sar -> Ll.Ashr
    | _ -> failwith "improper arithmetic/logical binop"

  let oatbinop_to_cnd = function 
    | Eq -> Ll.Eq
    | Neq -> Ll.Ne
    | Lt -> Ll.Slt
    | Lte -> Ll.Sle
    | Gt -> Ll.Sgt
    | Gte -> Ll.Sge
    | _ -> failwith "improper conditional binop"

  let dptr = function 
    | Ptr t -> t 
    | _ -> failwith "expected a pointer"

  let get_id = function
    | {elt=Id id} -> id 
    | failwith -> "expected id"

  let llfty_to_frety = function
    | Ptr (Fun (_, rety)) -> rety 
    | _ -> failwith "expected function type" 
end

let rec cmp_exp (c:Ctxt.t) ({elt=exp}:Ast.exp node) : Ll.ty * Ll.operand * stream =
  match exp with 
  | CNull oat_rty -> Ptr (cmp_rty oat_rty), Null, []
  | CBool oat_bl -> I1, Const (if oat_bl then 1L else 0L), []
  | CInt oat_i64 -> I64, Const oat_i64, []
  | CStr str -> failwith ""
  | CArr (ty, expn_lst) -> failwith ""
  | NewArr (ty, expn) -> failwith ""
  | Id oat_id -> cmp_id c oat_id
  | Index (expn1, expn2) -> failwith ""
  | Call (oat_fn_name, oat_fn_args) -> cmp_call c oat_fn_name oat_fn_args 
  | Bop (oat_binop, oat_e1, oat_e2) -> cmp_bop c oat_binop oat_e1 oat_e2
  | Uop (oat_unop, oat_e) -> cmp_uop c oat_unop oat_e
      
and cmp_call (c : Ctxt.t) (oat_fn_name : exp node) (oat_fn_args : exp node list) : Ll.ty * Ll.operand * stream =
  let fn_name = Exp.get_id oat_fn_name in 
  let ll_ty, ll_op = Ctxt.lookup_function fn_name c in 
  let ll_frety = Exp.llfty_to_frety ll_ty in 
  let ll_args = List.map (cmp_exp c) oat_fn_args in 
  let ll_args' = List.map (fun (ty, op, _) -> ty, op) ll_args in
  let ll_arg_streams = List.map (fun (_, _, stream) -> stream) ll_args in
  let ll_uid = gensym "fn_retval" in 
  let ll_stream = 
    List.flatten ll_arg_streams 
    >:: (I (ll_uid, Call (ll_frety, ll_op, ll_args'))) 
  in
    ll_frety, Id ll_uid, ll_stream

and cmp_id (c : Ctxt.t) (oat_id : id) : Ll.ty * Ll.operand * stream = 
  let ll_ty, ll_op = Ctxt.lookup oat_id c in 
  let ll_uid = gensym "id" in 
  let ll_stream = [I (ll_uid, Load (ll_ty, ll_op))]
  in 
    Exp.dptr ll_ty, Id ll_uid, ll_stream

and cmp_bop (c : Ctxt.t) (oat_binop : binop) (oat_e1 : exp node) (oat_e2 : exp node) : Ll.ty * Ll.operand * stream = 
  let _, ll_op1, ll_stream1 = cmp_exp c oat_e1 in 
  let _, ll_op2, ll_stream2 = cmp_exp c oat_e2 in 
  let oat_ty1, oat_ty2, oat_ty = typ_of_binop oat_binop in  
  let ll_ty = cmp_ty oat_ty in  
  let ll_uid = gensym "binop_temp" in 
  let ll_stream = begin
    match oat_ty1, oat_ty2, oat_ty with
    | TInt, TInt, TInt
    | TBool, TBool, TBool -> 
      let ll_binop = Exp.oat_to_llbinop oat_binop in 
      I (ll_uid, Binop (ll_binop, ll_ty, ll_op1, ll_op2))
    | TInt, TInt, TBool ->
      let ll_cnd = Exp.oatbinop_to_cnd oat_binop in 
      I (ll_uid, Icmp (ll_cnd, cmp_ty TInt, ll_op1, ll_op2))
    | _ -> failwith "invalid types for binops"
  end in
  let ll_stream = 
    ll_stream1
    >@ ll_stream2 
    >:: ll_stream
  in 
    ll_ty, Id ll_uid, ll_stream  

and cmp_uop (c : Ctxt.t) (oat_unop : unop) (oat_e : exp node) : Ll.ty * Ll.operand * stream = 
  let _, ll_op, ll_stream1 = cmp_exp c oat_e in
  let _, oat_ty = typ_of_unop oat_unop in 
  let ll_ty = cmp_ty oat_ty in 
  let ll_uid = gensym "unop_temp" in 
  let ll_stream2 = begin
    match oat_unop with 
    | Neg    -> [I (ll_uid, Binop (Ll.Mul, ll_ty, ll_op, Const (-1L)))]
    | Bitnot -> [I (ll_uid, Binop (Ll.Xor, ll_ty, ll_op, Const (-1L)))]
    | Lognot -> [I (ll_uid, Binop (Ll.Xor, ll_ty, ll_op, Const (1L)))]
  end in
  let ll_stream = 
    ll_stream1
    >@ ll_stream2 
  in 
    ll_ty, Id ll_uid, ll_stream 

(* Compile a statement in context c with return typ rt. Return a new context, 
   possibly extended with new local bindings, and the instruction stream
   implementing the statement.

   Left-hand-sides of assignment statements must either be OAT identifiers,
   or an index into some arbitrary expression of array type. Otherwise, the
   program is not well-formed and your compiler may throw an error.

   Tips:
   - for local variable declarations, you will need to emit Allocas in the
     entry block of the current function using the E() constructor.

   - don't forget to add a bindings to the context for local variable 
     declarations
   
   - you can avoid some work by translating For loops to the corresponding
     While loop, building the AST and recursively calling cmp_stmt

   - you might find it helpful to reuse the code you wrote for the Call
     expression to implement the SCall statement

   - compiling the left-hand-side of an assignment is almost exactly like
     compiling the Id or Index expression. Instead of loading the resulting
     pointer, you just need to store to it!

 *)

let rec cmp_stmt (c:Ctxt.t) (rt:Ll.ty) ({elt=stmt}:Ast.stmt node) : Ctxt.t * stream =
  match stmt with 
  | Assn (e1, e2) -> cmp_assn c e1 e2
  | Decl vdecl -> cmp_decl c vdecl
  | Ret en_opt -> cmp_ret c en_opt
  | SCall (oat_fn_name, oat_fn_args) -> cmp_scall c oat_fn_name oat_fn_args
  | If (oat_exp, oat_true_block,  oat_else_block) -> cmp_if c oat_exp oat_true_block oat_else_block
  | For (vdecl_lst, exp_opt, stmt_opt, stmt_lst) -> cmp_for c vdecl_lst exp_opt stmt_opt stmt_lst
  | While (oat_exp, oat_block) -> cmp_while c oat_exp oat_block

(* Compile a series of statements *)
and cmp_block (c:Ctxt.t) (rt:Ll.ty) (stmts:Ast.block) : Ctxt.t * stream =
  List.fold_left (fun (c, code) s -> 
      let c, stmt_code = cmp_stmt c rt s in
      c, code >@ stmt_code
    ) (c,[]) stmts

and cmp_assn (c : Ctxt.t) ({elt=lhs} : exp node) (e : exp node) : Ctxt.t * stream =
  let ll_e_ty, ll_e_op, ll_e_stream = cmp_exp c e in
  match lhs with 
  | Id oat_id -> 
    let ll_id_ty, ll_id_op = Ctxt.lookup oat_id c in
    let ll_stream = 
      ll_e_stream 
      >:: I ("", Store (ll_e_ty, ll_e_op, ll_id_op)) 
    in
      c, ll_stream
  | Index _ -> failwith "index assn not implemented"
  | _ -> failwith "improper lhs"

and cmp_decl (c : Ctxt.t) ((id, elt) : vdecl) : Ctxt.t * stream = 
  let ll_ty, ll_op, ll_stream1 = cmp_exp c elt in 
  let ll_uid = gensym "decl" in
  let ll_stream = 
    ll_stream1
    >:: E (ll_uid, Alloca ll_ty)
    >:: E ("", Store (ll_ty, ll_op, Id ll_uid))
  in
  let c = Ctxt.add c id (Ptr ll_ty, Id ll_uid) in 
    c, ll_stream
    
and cmp_ret (c : Ctxt.t) (en_opt : exp node option) : Ctxt.t * stream = 
  match en_opt with 
  | None -> c, [T (Ret (Void, None))]
  | Some oat_e -> 
    let ll_ty, ll_op, ll_stream1 = cmp_exp c oat_e in 
    let ll_stream = ll_stream1 >@ [T (Ret (ll_ty, Some ll_op))] 
    in 
      c, ll_stream

and cmp_scall (c : Ctxt.t) (oat_fn_name : exp node) (oat_fn_args : exp node list) : Ctxt.t * stream = 
  let _, _, ll_stream = cmp_exp c (no_loc @@ Call (oat_fn_name, oat_fn_args)) 
  in
    c, ll_stream

and cmp_if (c : Ctxt.t) (oat_exp : exp node) (oat_true_block : Ast.block) (oat_else_block : Ast.block) : Ctxt.t * stream = 
  let _, ll_op, ll_stream1 = cmp_exp c oat_exp in 
  let c, ll_true_stream = cmp_block c Void oat_true_block in
  let c, ll_else_stream = cmp_block c Void oat_else_block in
  let ll_uid = gensym "cmp_res" in 
  let ll_true_id = gensym "true_block" in 
  let ll_else_id = gensym "else_block" in
  let ll_end_id = gensym "if_end" in 
  let ll_stream = 
    ll_stream1
    >:: I (ll_uid, Icmp (Ll.Eq, Ll.I1, ll_op, Const 1L))
    >:: T (Cbr (Id ll_uid, ll_true_id, ll_else_id))
    >:: L ll_true_id
    >@ ll_true_stream
    >:: T (Br ll_end_id)
    >:: L ll_else_id
    >@ ll_else_stream
    >:: T (Br ll_end_id)
    >:: L ll_end_id 
  in
    c, ll_stream

and cmp_for (c : Ctxt.t) (vdecl_lst : vdecl list) (exp_opt : exp node option) (stmt_opt : stmt node option) (stmt_lst : stmt node list) : Ctxt.t * stream = 
  let oat_cnd = match exp_opt with None -> no_loc (CBool true) | Some e -> e in 
  let oat_incs = match stmt_opt with None -> [] | Some stmt -> [stmt] in 
  let oat_while_loop = 
    List.map (fun x -> no_loc @@ Decl x) vdecl_lst 
    @ [
      no_loc @@ While (oat_cnd, stmt_lst @ oat_incs) 
    ]
  in
    cmp_block c Ll.Void oat_while_loop

and cmp_while (c : Ctxt.t) (oat_exp : exp node) (oat_block : Ast.block) : Ctxt.t * stream = 
  let _, ll_op, ll_stream1 = cmp_exp c oat_exp in
  let c, ll_body_stream = cmp_block c Ll.Void oat_block in 
  let ll_uid = gensym "cmp_res" in 
  let ll_entry_uid = gensym "while_start" in
  let ll_body_uid = gensym "while_body" in
  let ll_exit_uid = gensym "while_end" in 
  let ll_stream =
    []
    >:: T (Br ll_entry_uid)
    >:: L ll_entry_uid 
    >@ ll_stream1
    >:: I (ll_uid, Icmp (Ll.Eq, Ll.I1, ll_op, Const 1L))
    >:: T (Cbr (Id ll_uid, ll_body_uid, ll_exit_uid))
    >:: L ll_body_uid
    >@ ll_body_stream
    >:: T (Br ll_entry_uid)
    >:: L ll_exit_uid
  in
    c, ll_stream


let gdecl_to_ctxt (c : Ctxt.t) {elt={name;init={elt;_}}} : Ctxt.t = 
  let t = 
    match elt with 
    | CNull rty -> Ptr (cmp_rty rty) 
    | CBool _ -> Ptr I1 
    | CInt _ -> Ptr I64 
    | CStr _ -> Ptr (Ptr I8)
    | CArr (t, _) -> Ptr (Ptr (Struct [I64; Array (0, cmp_ty t)]))
    | _ -> failwith "invalid gdecl"
  in Ctxt.add c name (t, Gid name)

let fdecl_to_ctxt c {elt={frtyp;fname;args}} = 
  let ft = TRef (RFun (List.map fst args, frtyp)) in 
  Ctxt.add c fname (cmp_ty ft, Gid fname) 

(* Adds each function identifer to the context at an
   appropriately translated type.  

   NOTE: The Gid of a function is just its source name
*)
let cmp_function_ctxt (c:Ctxt.t) (p:Ast.prog) : Ctxt.t =
    List.fold_left (fun c -> function
      | Ast.Gfdecl gdecl -> fdecl_to_ctxt c gdecl
      | _ -> c
    ) c p 

(* Populate a context with bindings for global variables 
   mapping OAT identifiers to LLVMlite gids and their types.

   Only a small subset of OAT expressions can be used as global initializers
   in well-formed programs. (The constructors starting with C). 
*)
let cmp_global_ctxt (c:Ctxt.t) (p:Ast.prog) : Ctxt.t =
  List.fold_left (fun acc decl -> 
    match decl with 
    | Gvdecl gdecl -> gdecl_to_ctxt acc gdecl
    | Gfdecl fdecl -> fdecl_to_ctxt acc fdecl
  ) c p

(* Compile a function declaration in global context c. Return the LLVMlite cfg
   and a list of global declarations containing the string literals appearing
   in the function.

   You will need to
   1. Allocate stack space for the function parameters using Alloca
   2. Store the function arguments in their corresponding alloca'd stack slot
   3. Extend the context with bindings for function variables
   4. Compile the body of the function using cmp_block
   5. Use cfg_of_stream to produce a LLVMlite cfg from 
 *)
let cmp_fdecl (c:Ctxt.t) ({elt}:Ast.fdecl node) : Ll.fdecl * (Ll.gid * Ll.gdecl) list =
  let frtyp, fname, args, body = elt.frtyp, elt.fname, elt.args, elt.body in 
  let f_rty = cmp_ret_ty frtyp in 
  let c, arg_setup = 
    List.fold_left (fun (c, stream) (ty, id) ->
      let id' = gensym id in
      let ty' = cmp_ty ty in 
      let stream = 
        stream
        >:: E (id', Alloca ty')
        >:: E ("", Store (ty', Id id, Id id')) in
      let c = Ctxt.add c id (Ptr ty', Id id')
        in c, stream
    ) (c, []) args
  in    
  let c, body_stream = cmp_block c f_rty body in
  let f_cfg, decl_list = cfg_of_stream (arg_setup >@ body_stream) in 
  let f_ty = List.map (fun x -> cmp_ty @@ fst x) args, f_rty in 
  let f_param = List.map snd args in 
  {f_ty; f_param; f_cfg}, decl_list


(* Compile a global initializer, returning the resulting LLVMlite global
   declaration, and a list of additional global declarations.

   Tips:  
   - Only CNull, CBool, CInt, CStr, and CArr can appear as global initializers
     in well-formed OAT programs. Your compiler may throw an error for the other
     cases

   - OAT arrays are always handled via pointers. A global array of arrays will
     be an array of pointers to arrays emitted as additional global declarations.
*)
let rec cmp_gexp c ({elt;}:Ast.exp node) : Ll.gdecl * (Ll.gid * Ll.gdecl) list = 
  match elt with 
  | CNull rty -> (cmp_ty @@ Ast.TRef rty, GNull), []
  | CBool b -> (Ll.I1, GInt (if b then 1L else 0L)), [] 
  | CInt i -> (Ll.I64, GInt i), []
  | CStr s -> (Ll.Array (String.length s + 1, I8), GString s), []
  | CArr (t, lst) -> 
    let arr_len = List.length lst in
    let el_ty = cmp_ty t in 
    let arr_typ = Ll.Struct [Ll.I64; Ll.Array (arr_len, el_ty)] 
    in 
      (arr_typ, GStruct [
      (Ll.I64, GInt (Int64.of_int @@ arr_len))
    ; (Ll.Array (arr_len, arr_typ), GArray (List.map (fun x -> let (ty, ginit), _ = cmp_gexp c x in ty, ginit) lst))]
    ), []
  | _ -> failwith "cmp_gexp: invalid element"


(* Oat internals function context ------------------------------------------- *)
let internals = [
    "oat_alloc_array",         Ll.Fun ([I64], Ptr I64)
  ]

(* Oat builtin function context --------------------------------------------- *)
let builtins =
  [ "array_of_string",  cmp_rty @@ RFun ([TRef RString], RetVal (TRef(RArray TInt)))
  ; "string_of_array",  cmp_rty @@ RFun ([TRef(RArray TInt)], RetVal (TRef RString))
  ; "length_of_string", cmp_rty @@ RFun ([TRef RString],  RetVal TInt)
  ; "string_of_int",    cmp_rty @@ RFun ([TInt],  RetVal (TRef RString))
  ; "string_cat",       cmp_rty @@ RFun ([TRef RString; TRef RString], RetVal (TRef RString))
  ; "print_string",     cmp_rty @@ RFun ([TRef RString],  RetVoid)
  ; "print_int",        cmp_rty @@ RFun ([TInt],  RetVoid)
  ; "print_bool",       cmp_rty @@ RFun ([TBool], RetVoid)
  ]

(* Compile a OAT program to LLVMlite *)
let cmp_prog (p:Ast.prog) : Ll.prog =
  (* add built-in functions to context *)
  let init_ctxt = 
    List.fold_left (fun c (i, t) -> Ctxt.add c i (Ll.Ptr t, Gid i))
      Ctxt.empty builtins
  in
  let fc = cmp_function_ctxt init_ctxt p in

  (* build global variable context *)
  let c = cmp_global_ctxt fc p in

  (* compile functions and global variables *)
  let fdecls, gdecls = 
    List.fold_right (fun d (fs, gs) ->
        match d with
        | Ast.Gvdecl { elt=gd } -> 
           let ll_gd, gs' = cmp_gexp c gd.init in
           (fs, (gd.name, ll_gd)::gs' @ gs)
        | Ast.Gfdecl fd ->
           let fdecl, gs' = cmp_fdecl c fd in
           (fd.elt.fname,fdecl)::fs, gs' @ gs
      ) p ([], [])
  in

  (* gather external declarations *)
  let edecls = internals @ builtins in
  { tdecls = []; gdecls; fdecls; edecls }
