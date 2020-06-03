%{
  
  open Core_kernel
  open Reporting
  open Source
  
%}

%token<Reporting.Region.t> LPAREN RPAREN COMMA SEMICOLON COLON BANG DOT
%token<Reporting.Region.t> IMPL QRY 
%token<Reporting.Region.t> EQ VBAR SUBTY TYPE PRED

%token <int * Reporting.Region.t> INTLIT 
%token <string * Reporting.Region.t> REALLIT
%token <bool * Reporting.Region.t> BOOLLIT
%token <string * Reporting.Region.t> SYMLIT
%token <string * Reporting.Region.t> VAR
%token <Reporting.Region.t> WILDCARD
%token <string * Reporting.Region.t> PREDSYM
%token<Reporting.Region.t> TYSYMBOL TYREAL TYINT TYBOOL

%token EOF



%left SEMICOLON
%left COMMA
%nonassoc unary_over_binary


(* -- Start ----------------------------------------------------------------- *)

%start <Source.Program.t> program
%%



(* -- Program --------------------------------------------------------------- *)

program : stmts=list(statement) EOF { {stmts} }

(* -- Statements are either clause, queries, facts or declarations ---------- *)

statement:
  (* clause *)
  | head=head_term IMPL body=body DOT {       
    Statement.clause head body
  }

  (* query *)
  | QRY body=body DOT { 
    Statement.query body
  }

  (* fact *)
  | head=head_symbol DOT {    
    Statement.fact head 
  }

  (* declaration *)
  | d=declaration { Statement.decl d }


(* -- Head subgoals ------------------------------------------------------------ 
   In this Datalog, no operations are allowed in heads of clauses so they must
   always be atoms
*)

head_term:
  | atom=atom_term {
      
      Head.Term.atom atom 
  }


head_symbol:
  | atom=atom_symbol {
      Head.Symbol.atom atom 
  }


(* -- Body subgoal ---------------------------------------------------------- *)

body : 
  | atom=atom_term {     
    Body.atom atom 
    
  }
  | op=unop body=body %prec unary_over_binary {       
      Body.unOp op body
      
  }
  | l=body op=connective r=body { 
    Body.binOp op l r     
  }

  | s=delimited(LPAREN,body,RPAREN) { 
    s 
  }

unop: region=BANG { Located.{elem=OpSet.Body.Unary.Neg;region}}

%inline connective : 
  | region=COMMA     { Located.{elem=OpSet.Body.Binary.Conj;region} }
  | region=SEMICOLON { Located.{elem=OpSet.Body.Binary.Disj;region} }


(* -- Atomic formula -------------------------------------------------------- *)

atom_term : 
  | predSym=predSym LPAREN terms=separated_list(COMMA,term) end_region=RPAREN { 
      let region = Region.(merge predSym.region end_region) in
      let atom = Located.locate ~region @@ Atom.Term.atom predSym terms in 
      atom
  } 

atom_symbol : 
  | predSym=predSym LPAREN terms=separated_list(COMMA,term) end_region=RPAREN { 
      let region = Region.(merge predSym.region end_region) in
      let atom = Located.locate ~region @@ Atom.Symbol.atom predSym terms in 
      atom
  } 

(* -- Term ------------------------------------------------------------------ *)

term : 
  | sym=symbol { 
      let s,region = sym in 
      Core.Term.TSym(s,region)
  }
  | v=tmvar {
    let tv,region = v in 
    Core.Term.TVar(tv,region)
  }
  | region=WILDCARD { 
     Core.Term.TWild region 
  }

(* -- Term variables -------------------------------------------------------- *)

tmvar : v = VAR { 
  let name,region = v in 
  Core.Tmvar.from_string @@ String.chop_prefix_exn name ~prefix:"?", region 
}

(* -- Predicate symbols ----------------------------------------------------- *)
predSym : id=PREDSYM { 
    Located.{
        elem= Core.PredSymbol.from_string @@ fst id 
        ; region = snd id }
    }
(* -- Symbol ---------------------------------------------------------------- *)

symbol : 
  | sym=SYMLIT { 
      let str,region = sym in 
      let symbol= Core.Symbol.SText str in 
      (symbol,region)
  }
  | sym=INTLIT { 
      let n,region = sym in 
      let symbol = Core.Symbol.SInt n in  
      (symbol,region)
  }
  | sym=REALLIT { 
      let str,region = sym in 
      let symbol = Core.Symbol.SReal str in 
      (symbol,region)
  }
  | sym=BOOLLIT { 
      let b,region = sym in 
      let symbol = Core.Symbol.SBool b in       
      (symbol,region)
  }

(* -- Declarations are either type declarations or clause (rule) declarations *)

declaration: 
  | tydecl=typeDecl { Decl.ty tydecl }
  | prdecl=predDecl { Decl.pred prdecl}


(* -- Type declarations are either subtypes or primitives or unions --------- *)

typeDecl: 
  | start_=TYPE name=typeName EQ first_ty=typeName VBAR
    rest_tys=separated_nonempty_list(VBAR,typeName)  {
    let end_ = Reporting.Located.region_of @@ List.last_exn rest_tys in
    let region = Reporting.Region.merge start_ end_ 
    and defn = Decl.TyDecl.Defn.Union(first_ty, Lib.NonEmpty.from_list_exn rest_tys) in 
    let decl = Decl.TyDecl.{name;defn} in 
    Reporting.Located.locate ~region decl
  }
  | start_=TYPE name=typeName SUBTY primty=primType  {
    let end_ = Reporting.Located.region_of primty in
    let region = Reporting.Region.merge start_ end_
    and defn = Decl.TyDecl.Defn.SubTy primty in 
    let decl = Decl.TyDecl.{name;defn} in 
    Reporting.Located.locate ~region decl
  }

primType:
  | region=TYSYMBOL { Reporting.Located.locate ~region Ty.Prim.TySymbol }
  | region=TYREAL   { Reporting.Located.locate ~region Ty.Prim.TyReal }
  | region=TYINT    { Reporting.Located.locate ~region Ty.Prim.TyInt }
  | region=TYBOOL   { Reporting.Located.locate ~region Ty.Prim.TyBool }

typeName:
  | nm=SYMLIT { 
      Reporting.Located.locate ~region:(snd nm) @@ 
        Ty.Name.from_string @@ fst nm 
  }


(* -- Clause declarations specify predicate symbol and arguments ------------ *)

predDecl:
 | start_=PRED name=predSym  LPAREN 
   params=separated_list(COMMA,predArg) end_=RPAREN  { 
     let region = Reporting.Region.merge start_ end_ 
     and decl = Decl.PredDecl.{name;params} in 
     Reporting.Located.locate ~region decl
   }

predArg : tv=tmvar COLON ty=ty { (fst tv, ty) }

ty:
  | ty=primType { 
      Reporting.Located.locate ~region:ty.region @@ Ty.Prim ty.elem      
    }
  | ty=typeName { 
      Reporting.Located.locate ~region:ty.region @@ Ty.Named ty.elem 
    }