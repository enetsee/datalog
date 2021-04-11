%{
  
  open Core_kernel
  open Reporting
  open Source
  
%}

%token<Reporting.Region.t> LPAREN RPAREN COMMA SEMICOLON COLON BANG DOT IMPL
%token<Reporting.Region.t> DATA PARAM TYPE EXPORT EXTENDS


%token <int * Reporting.Region.t> INTLIT 
%token <string * Reporting.Region.t> REALLIT
%token <bool * Reporting.Region.t> BOOLLIT
%token <string * Reporting.Region.t> SYMLIT

%token <string * Reporting.Region.t> VAR
%token <string option * Reporting.Region.t> WILDCARD
%token <string * Reporting.Region.t> PARAMNAME
%token <string * Reporting.Region.t> TYNAME
%token <string * Reporting.Region.t> NAME

%token<Reporting.Region.t> TYSYMBOL TYREAL TYINT TYBOOL TYDATE TYSPAN

%token EOF



%left SEMICOLON
%left COMMA
%nonassoc unary_over_binary


(* -- Start ----------------------------------------------------------------- *)

%start <Source.Program.t> program
%%



(* -- Program --------------------------------------------------------------- *)

program : stmts=list(statement) EOF { Source.Program.{stmts} }

(* -- Statements are either clause, queries, facts or declarations ---------- *)

statement:
  | st=clause { st }
  | st=fact { st }
  | st=ty_defn { st }
  | st=data_decl { st }
  | st=param_decl { st }
  | st=export_decl { st }


clause: 
  | head=head_term IMPL body=body DOT {       
    Statement.clause head body
  }

fact:
  | head=head_symbol DOT {    
    Statement.fact head 
  }




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
  | pred_nm=pred_name LPAREN terms=separated_list(COMMA,term) end_region=RPAREN { 
      let region = Region.(merge pred_nm.region end_region) in
      let atom = Located.locate ~region @@ Atom.Term.atom pred_nm terms in 
      atom
  } 

atom_symbol : 
  | pred_nm=pred_name LPAREN terms=separated_list(COMMA,term) end_region=RPAREN { 
      let region = Region.(merge pred_nm.region end_region) in
      let atom = Located.locate ~region @@ Atom.Symbol.atom pred_nm terms in 
      atom
  } 

pred_name : 
  | n=NAME { 
    let (str,region) = n in 
    Reporting.Located.locate ~region @@ Name.from_string str
  }
(* -- Term ------------------------------------------------------------------ *)


term : 
  | sym=symbol { 
      let s,region = sym in 
      Core.Term.sym s ~region
  }
  | v=VAR {
    let str,region = v in 
    Core.Term.var str ~region
  }
  | p=PARAMNAME { 
    let (str,region) = p in  
    Core.Term.param  ~region @@ Name.from_string str
  }
  | w=WILDCARD { 
    let (str_opt,region) = w in 
    
     Core.Term.wild  ~region @@ Option.map ~f:Name.from_string str_opt
  }


(* -- Symbol ---------------------------------------------------------------- *)

(* TODO: Date and Span literals *)
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






export_decl :
  | EXPORT nm=NAME { 
      let loc_name = 
        let (str,region) = nm in 
        Reporting.Located.locate ~region @@ Name.from_string str
      in 
      Statement.export loc_name
    }

param_decl : 
  | start_=PARAM nm=PARAMNAME COLON ty=ty { 
    let loc_name = 
        let (str,region) = nm in 
        Reporting.Located.locate ~region  @@ Name.from_string str
    in 
    let region = Reporting.Region.merge start_ ty.region in 
    Statement.param ~region loc_name ty 
  }



ty_defn: 
  | start_=TYPE tyname=ty_name EXTENDS superty=ty  {
    
    let end_ = Reporting.Located.region_of superty in
    let region = Reporting.Region.merge start_ end_ in     
    let name = 
      let (nm,region) = tyname in
      Reporting.Located.locate ~region nm
    in
    Source.Statement.tydefn ~region name superty
  }




ty :
  | region=TYSYMBOL { Reporting.Located.locate ~region Type.Ty.Symbol }
  | region=TYREAL   { Reporting.Located.locate ~region Type.Ty.Real }
  | region=TYINT    { Reporting.Located.locate ~region Type.Ty.Int }
  | region=TYBOOL   { Reporting.Located.locate ~region Type.Ty.Bool }
  | region=TYDATE   { Reporting.Located.locate ~region Type.Ty.Date }
  | region=TYSPAN   { Reporting.Located.locate ~region Type.Ty.Span }
  | named=ty_name    { let (nm,region) = named in Reporting.Located.locate ~region @@ Type.Ty.Named nm }

ty_name : 
  | nm = TYNAME {
    let (str,region) = nm in 
    Name.from_string str , region
  }

data_decl : 
  | start_=DATA nm=NAME LPAREN attrs=separated_nonempty_list(COMMA,data_attribute) end_ = RPAREN { 
      let region = Reporting.Region.merge start_ end_ in
      let loc_name = 
        let (str,region) = nm in 
        Reporting.Located.locate  ~region @@ Name.from_string str
      in 
      Statement.data ~region loc_name  attrs
  }


data_attribute: 
  | nm=NAME COLON ty=ty {

    let loc_name = 
        let (str,region) = nm in 
        Reporting.Located.locate ~region @@ Name.from_string str
      in 
    loc_name,ty
  }
