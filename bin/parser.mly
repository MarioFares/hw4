%{
open Ast
open Lexing

let loc (startpos:Lexing.position) (endpos:Lexing.position) (elt:'a) : 'a node =
  { elt ; loc=Range.mk_lex_range startpos endpos }

%}

/* Declare your tokens here. */
%token EOF
%token <int64>  INT
%token NULL
%token <string> STRING
%token <string> IDENT

%token TINT     /* int    */
%token TVOID    /* void   */
%token TSTRING  /* string */
%token TBOOL    /* bool   */
%token IF       /* if     */
%token ELSE     /* else   */
%token WHILE    /* while  */
%token FOR      /* for    */
%token RETURN   /* return */
%token VAR      /* var    */
%token TRUE     /* true   */
%token FALSE    /* false  */
%token NEW      /* new    */
%token SEMI     /*   ;    */
%token COMMA    /*   ,    */
%token LBRACE   /*   {    */
%token RBRACE   /*   }    */
%token PLUS     /*   +    */
%token LSHIFT   /*   <<   */
%token LRSHIFT  /*   >>   */
%token ARSHIFT  /*   >>>  */
%token LT       /*   <    */
%token LTEQ     /*   <=   */
%token GT       /*   >    */
%token GTEQ     /*   >=   */
%token BANGEQ   /*   !=   */
%token AND      /*   &    */
%token OR       /*   |    */
%token BAND     /*  [&]   */
%token BOR      /*  [|]   */
%token DASH     /*   -    */
%token STAR     /*   *    */
%token EQEQ     /*   ==   */
%token EQ       /*   =    */
%token LPAREN   /*   (    */
%token RPAREN   /*   )    */
%token LBRACKET /*   [    */
%token RBRACKET /*   ]    */
%token TILDE    /*   ~    */
%token BANG     /*   !    */
%token GLOBAL   /* global */

%left BOR                     /*  20 */
%left BAND                    /*  30 */
%left OR                      /*  40 */
%left AND                     /*  50 */
%left EQEQ BANGEQ             /*  60 */
%left LT GT LTEQ GTEQ         /*  70 */
%left LSHIFT LRSHIFT ARSHIFT  /*  80 */
%left PLUS DASH               /*  90 */
%left STAR                    /* 100 */
%nonassoc BANG
%nonassoc TILDE
%nonassoc LBRACKET
%nonassoc LPAREN

/* ---------------------------------------------------------------------- */

%start prog
%start exp_top
%start stmt_top
%type <Ast.exp Ast.node> exp_top
%type <Ast.stmt Ast.node> stmt_top

%type <Ast.prog> prog
%type <Ast.exp Ast.node> exp
%type <Ast.stmt Ast.node> stmt
%type <Ast.block> block
%type <Ast.ty> ty
%%

exp_top:
  | e=exp EOF { e }

stmt_top:
  | s=stmt EOF { s }

prog:
  | p=list(decl) EOF  { p }

decl:
  | GLOBAL name=IDENT EQ init=gexp SEMI
    { Gvdecl (loc $startpos $endpos { name; init }) }
  | frtyp=ret_ty fname=IDENT LPAREN args=arglist RPAREN body=block
    { Gfdecl (loc $startpos $endpos { frtyp; fname; args; body }) }

arglist:
  | l=separated_list(COMMA, pair(ty,IDENT)) { l }
    
ty:
  | TINT   { TInt }
  | TBOOL  { TBool }
  | r=rtyp { TRef r } 

%inline ret_ty:
  | TVOID  { RetVoid }
  | t=ty   { RetVal t }

%inline rtyp:
  | TSTRING { RString }
  | t=ty LBRACKET RBRACKET { RArray t }

%inline bop:
  | PLUS    { Add  }
  | DASH    { Sub  }
  | STAR    { Mul  }
  | EQEQ    { Eq   } 
  | BANGEQ  { Neq  }
  | LT      { Lt   }
  | LTEQ    { Lte  }
  | GT      { Gt   }
  | GTEQ    { Gte  }
  | AND     { And  }
  | OR      { Or   }
  | BAND    { IAnd }
  | BOR     { IOr  }
  | LSHIFT  { Shl  }
  | LRSHIFT { Shr  }
  | ARSHIFT { Sar  } 

%inline uop:
  | DASH  { Neg    }
  | BANG  { Lognot }
  | TILDE { Bitnot }

gexp:
  | i=INT        { loc $startpos $endpos @@ CInt i } 
  | s=STRING     { loc $startpos $endpos @@ CStr s }
  | t=rtyp NULL  { loc $startpos $endpos @@ CNull t }
  | t=TRUE       { loc $startpos $endpos @@ CBool true }
  | f=FALSE      { loc $startpos $endpos @@ CBool false }
  | NEW t=ty LBRACKET RBRACKET LBRACE g=separated_list(COMMA, gexp) RBRACE { loc $startpos $endpos @@ CArr (t, g)}


lhs:  
  | id=IDENT            { loc $startpos $endpos @@ Id id }
  | e=exp LBRACKET i=exp RBRACKET
                        { loc $startpos $endpos @@ Index (e, i) }

exp:
  | id=IDENT                       { loc $startpos $endpos @@ Id id }
  | i=INT                          { loc $startpos $endpos @@ CInt i }
  | s=STRING                       { loc $startpos $endpos @@ CStr s }
  | t=rtyp NULL                    { loc $startpos $endpos @@ CNull t }
  | t=TRUE                         { loc $startpos $endpos @@ CBool true }
  | f=FALSE                        { loc $startpos $endpos @@ CBool false }
  | e=exp LBRACKET i=exp RBRACKET  { loc $startpos $endpos @@ Index (e, i) }
  | e=exp LPAREN es=separated_list(COMMA, exp) RPAREN { loc $startpos $endpos @@ Call (e,es) }
  | NEW t=ty LBRACKET RBRACKET LBRACE es=separated_list(COMMA, exp) RBRACE { loc $startpos $endpos @@ CArr (t, es)}
  | NEW t=ty LBRACKET e=exp RBRACKET  { loc $startpos $endpos @@ NewArr (t, e)}
  | e1=exp b=bop e2=exp               { loc $startpos $endpos @@ Bop (b, e1, e2) }
  | u=uop e=exp                       { loc $startpos $endpos @@ Uop (u, e) }
  | LPAREN e=exp RPAREN               { e } 

vdecl:
  | VAR id=IDENT EQ init=exp { (id, init) }

stmt: 
  | d=vdecl SEMI                                           { loc $startpos $endpos @@ Decl(d) }
  | p=lhs EQ e=exp SEMI                                    { loc $startpos $endpos @@ Assn(p,e) }
  | e=exp LPAREN es=separated_list(COMMA, exp) RPAREN SEMI { loc $startpos $endpos @@ SCall (e, es) }
  | ifs=if_stmt                                            { ifs }
  | RETURN SEMI                                            { loc $startpos $endpos @@ Ret(None) }
  | RETURN e=exp SEMI                                      { loc $startpos $endpos @@ Ret(Some e) }
  | WHILE LPAREN e=exp RPAREN b=block                      { loc $startpos $endpos @@ While(e, b) } 
  | FOR LPAREN ds=separated_list(COMMA, vdecl) SEMI 
               e=exp SEMI 
               s=stmt 
        RPAREN
               b=block  { loc $startpos $endpos @@ For(ds, Some e, Some s, b)}
  | FOR LPAREN ds=separated_list(COMMA, vdecl) SEMI
               SEMI 
               s=stmt 
        RPAREN b=block { loc $startpos $endpos @@ For(ds, None, Some s, b)}
  | FOR LPAREN ds=separated_list(COMMA, vdecl) SEMI
               e=exp SEMI
        RPAREN b=block { loc $startpos $endpos @@ For(ds, Some e, None, b)}
  | FOR LPAREN ds=separated_list(COMMA, vdecl) 
               SEMI
               SEMI 
        RPAREN b=block { loc $startpos $endpos @@ For(ds, None, None, b)}

block:
  | LBRACE stmts=list(stmt) RBRACE { stmts }

if_stmt:
  | IF LPAREN e=exp RPAREN b1=block b2=else_stmt
    { loc $startpos $endpos @@ If(e,b1,b2) }

else_stmt:
  | (* empty *)       { [] }
  | ELSE b=block      { b }
  | ELSE ifs=if_stmt  { [ ifs ] }
