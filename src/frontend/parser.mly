%{
  
  open Core_kernel
  open Reporting
  
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
%token <string * Reporting.Region.t> TYNAME
%token EOF



%left SEMICOLON
%left COMMA
%nonassoc unary_over_binary


(* -- Start ----------------------------------------------------------------- *)

%start <Program.t> program
%%



(* -- Program --------------------------------------------------------------- *)

program : stmts=list(statement) EOF { {stmts} }

(* -- Statements are either sentences or declarations ----------------------- *)

statement:
  | s=sentence { Statement.sentence s }
  | d=declaration { Statement.decl d }

(* -- Sentences are clauses, facts or queries ------------------------------- *)

sentence:
  (* clause *)
  | a=atom IMPL body=subgoal DOT { 
    let (atom,atom_region) = a in
    let region = Region.merge atom_region @@ Subgoal.region body in
    let head = Subgoal.atom atom ~region  in
    let elem = Clause.{ head ; body } in
    Sentence.SClause Located.{ elem  ; region }
  }

  (* fact *)
  | a=atom DOT {
    let (atom,region) = a in
    let head = Subgoal.atom atom ~region  in
    let elem = Fact.{ head } in 
    Sentence.SFact Located.{elem ; region}
  }

  (* query *)
  | QRY body=subgoal DOT { 
    let region = Subgoal.region body in     
    let elem = Query.{ head = None; body } in 
    Sentence.SQuery Located.{ elem ; region }
  }

(* -- Subgoal --------------------------------------------------------------- *)

subgoal : 
  | a=atom { 
    let (atom,region) = a in
    Subgoal.atom atom ~region
    
  }
  | op=unop s=subgoal %prec unary_over_binary { 
      let region = Region.merge op.region  @@ Subgoal.region s in
      Subgoal.unop op s ~region 
      
  }
  | l=subgoal op=connective r=subgoal { 
    let region = Region.merge (Subgoal.region l) (Subgoal.region r) in 
    Subgoal.binop op l r ~region
    
  }
  | s=delimited(LPAREN,subgoal,RPAREN) { s }

unop: region=BANG { Located.{elem=Op.Neg;region}}

%inline connective : 
  | region=COMMA     { Located.{elem=Op.Conj;region} }
  | region=SEMICOLON { Located.{elem=Op.Disj;region} }

(* -- Atomic formula -------------------------------------------------------- *)

atom : 
  | predSym=predSym terms=delimited(LPAREN,separated_list(COMMA,term),RPAREN) { 
      let region = Region.(merge predSym.region @@  merge_many @@ List.map ~f:(fun {region;_} -> region) terms) in 
      let elem = Atom.atom predSym terms in 
      ( elem , region )
  } 

(* -- Term ------------------------------------------------------------------ *)

term : 
  | sym=symbol { 
      let s,region = sym in 
      Located.{ elem = Core.Term.TSym s; region }
  }
  | v=tmvar {
    let tv,region = v in 
    Located.{ elem = Core.Term.TVar tv ; region }
  }
  | region=WILDCARD { 
     Located.{ elem = Core.Term.TWild; region }
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
  | nm=TYNAME { 
      Reporting.Located.locate ~region:(snd nm) @@ 
        Ty.Name.from_string @@ 
        String.chop_prefix_exn ~prefix:"@" @@ fst nm 
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