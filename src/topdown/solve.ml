(* open Core
open Core_kernel

let subquery lit = 
    Lit.Adorned.(pred_of lit , bpatt_of lit) 
    
let clauses_with deps (pr,patt) = 
  List.filter 
    ~f:(fun cls ->               
      Binding.equal patt @@ 
      Lit.Adorned.bpatt_of  @@ 
      Clause.Adorned.head_of cls 
    ) 
  (Dependency.Adorned.clauses_of deps pr)

let clauses_for deps lit = 
  clauses_with deps @@ subquery lit


let unfold_clause deps cls = 
  Tree.unfold cls
    ~f:(fun cls -> 
        let sq = subquery lit in 
        sq, clau
          (),()) 

let solve_strata qry ~clauses  = 
  let prog = Program.Adorned.program clauses [qry] in
  let deps = Dependency.Adorned.from_program prog in 
  let patt = Binding.mk_free qry in 
  List.map ~f:(unfold_clause deps) @@ clauses_with deps (qry,patt)


let solve ~prog:_ ~kb:_ _query =
  
  ()
;;
 (* List.filter ~f:(fun cls -> 
              let hd = Clause.Adorned.head_of cls 
              Binding.equal bp @@ Lit.Adorned.bpatt_of hd 
            )
            @@  *) *)
