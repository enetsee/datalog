open Core_kernel

module Alice = struct
  let vx = Core.Term.var "X"
  let mk_lit pr = Core.Lit.Raw.(lit pr [ vx ])
  let pr_a = Core.Pred.(pred ~arity:1 @@ Core.Name.from_string "a")
  let lit_a = mk_lit pr_a
  let pr_b = Core.Pred.(pred ~arity:1 @@ Core.Name.from_string "b")
  let lit_b = mk_lit pr_b
  let pr_c = Core.(Pred.pred ~arity:1 @@ Name.from_string "c")
  let lit_c = mk_lit pr_c
  let pr_d = Core.(Pred.pred ~arity:1 @@ Name.from_string "d")
  let lit_d = mk_lit pr_d
  let pr_e = Core.(Pred.pred ~arity:1 @@ Name.from_string "e")
  let lit_e = mk_lit pr_e
  let pr_s = Core.(Pred.pred ~arity:1 @@ Name.from_string "s")
  let lit_s = mk_lit pr_s
  let pr_t = Core.(Pred.pred ~arity:1 @@ Name.from_string "t")
  let lit_t = mk_lit pr_t
  let pr_u = Core.(Pred.pred ~arity:1 @@ Name.from_string "u")
  let lit_u = mk_lit pr_u
  let pr_v = Core.(Pred.pred ~arity:1 @@ Name.from_string "v")
  let lit_v = mk_lit pr_v
  let pr_qry = Core.(Pred.pred ~arity:0 @@ Name.from_string "query")
  let lit_qry = Core.Lit.Raw.lit pr_qry []

  let cl_s =
    Core.(
      Clause.Adorned.(
        clause
          Lit.Adorned.(from_raw lit_s ~bpatt:Binding.(from_list [ Bound ]))
          Lit.Adorned.
            [ from_raw lit_b ~bpatt:Binding.(from_list [ Bound ])
            ; neg @@ from_raw lit_a ~bpatt:Binding.(from_list [ Bound ])
            ]))
  ;;

  let cl_t =
    Core.(
      Clause.Adorned.(
        clause
          Lit.Adorned.(from_raw lit_t ~bpatt:Binding.(from_list [ Bound ]))
          Lit.Adorned.
            [ from_raw lit_c ~bpatt:Binding.(from_list [ Bound ])
            ; neg @@ from_raw lit_a ~bpatt:Binding.(from_list [ Bound ])
            ]))
  ;;

  let cl_u =
    Core.(
      Clause.Adorned.(
        clause
          Lit.Adorned.(from_raw lit_u ~bpatt:Binding.(from_list [ Bound ]))
          Lit.Adorned.
            [ from_raw lit_d ~bpatt:Binding.(from_list [ Bound ])
            ; neg @@ from_raw lit_t ~bpatt:Binding.(from_list [ Bound ])
            ]))
  ;;

  let cl_v =
    Core.(
      Clause.Adorned.(
        clause
          Lit.Adorned.(from_raw lit_v ~bpatt:Binding.(from_list [ Free ]))
          Lit.Adorned.
            [ from_raw lit_e ~bpatt:Binding.(from_list [ Free ])
            ; neg @@ from_raw lit_s ~bpatt:Binding.(from_list [ Bound ])
            ; neg @@ from_raw lit_u ~bpatt:Binding.(from_list [ Bound ])
            ]))
  ;;

  let cl_qry =
    Core.(
      Clause.Adorned.(
        clause
          Lit.Adorned.(from_raw lit_qry ~bpatt:Binding.(from_list []))
          Lit.Adorned.[ from_raw lit_v ~bpatt:Binding.(from_list [ Free ]) ]))
  ;;

  let cls_good = [ cl_s; cl_t; cl_u; cl_v; cl_qry ]
  let strata = [ [ cl_t ]; [ cl_u ]; [ cl_s ]; [ cl_v ]; [ cl_qry ] ]
  let queries = [ pr_qry ]

  (** Example 15.2.5, adapted
  
    s(X) :- b(X), not a(X).
    t(X) :- c(X), not a(X).
    u(X) :- d(x), not t(X).
    v(X) :- e(x), not s(X), not u(X).
    query() :- v(X).
  *)
  let prg_good = Core.Program.Adorned.(sorted @@ program cls_good queries [] [])

  let prg_good_stratified =
    Core.Program.Stratified.(sorted { strata; queries; data = []; params = [] })
  ;;

  let cls_bad =
    Core.(
      Clause.Adorned.
        [ clause
            Lit.Adorned.(from_raw lit_s ~bpatt:Binding.(from_list [ Bound ]))
            Lit.Adorned.
              [ from_raw lit_a ~bpatt:Binding.(from_list [ Bound ])
              ; neg @@ from_raw lit_t ~bpatt:Binding.(from_list [ Bound ])
              ]
        ; clause
            Lit.Adorned.(from_raw lit_t ~bpatt:Binding.(from_list [ Bound ]))
            Lit.Adorned.
              [ from_raw lit_b ~bpatt:Binding.(from_list [ Bound ])
              ; neg @@ from_raw lit_s ~bpatt:Binding.(from_list [ Bound ])
              ]
        ; clause
            Lit.Adorned.(from_raw lit_qry ~bpatt:Binding.(from_list []))
            Lit.Adorned.[ from_raw lit_t ~bpatt:Binding.(from_list [ Free ]) ]
        ])
  ;;

  (** Example 15.1.1, adapted

    s(X) :- a(X), not t(X).
    t(X) :- b(X), not s(X).
    query() :- t(X).

  *)
  let prg_bad = Core.Program.Adorned.(sorted @@ program cls_bad queries [] [])

  let cycles = [ pr_t, pr_s; pr_s, pr_t ]
end

module Comp = struct
  let pr_n = Core.(Pred.pred ~arity:1 @@ Name.from_string "n")
  let lit_n1 = Core.(Lit.Raw.lit pr_n Term.[ var "X" ])
  let lit_n2 = Core.(Lit.Raw.lit pr_n Term.[ var "Y" ])
  let pr_g = Core.(Pred.pred ~arity:2 @@ Name.from_string "g")
  let lit_g1 = Core.(Lit.Raw.lit pr_g Term.[ var "X"; var "Y" ])
  let lit_g2 = Core.(Lit.Raw.lit pr_g Term.[ var "X"; var "Z" ])
  let pr_t = Core.(Pred.pred ~arity:2 @@ Name.from_string "t")
  let lit_t_head = Core.(Lit.Raw.lit pr_t Term.[ var "X"; var "Y" ])
  let lit_t_body = Core.(Lit.Raw.lit pr_t Term.[ var "Z"; var "Y" ])
  let pr_ct = Core.(Pred.pred ~arity:2 @@ Name.from_string "ct")
  let lit_ct = Core.(Lit.Raw.lit pr_ct Term.[ var "X"; var "Y" ])
  let pr_qry = Core.(Pred.pred ~arity:0 @@ Name.from_string "qry")
  let lit_qry = Core.(Lit.Raw.lit pr_qry [])
  let bb = Core.Binding.(from_list [ Bound; Bound ])
  let ff = Core.Binding.(from_list [ Free; Free ])
  let bf = Core.Binding.(from_list [ Bound; Free ])
  let f = Core.Binding.(from_list [ Free ])

  let cl_t1 =
    Core.(
      Clause.Adorned.clause
        Lit.Adorned.(from_raw lit_t_head ~bpatt:bb)
        Lit.Adorned.[ from_raw lit_g1 ~bpatt:bb ])
  ;;

  let cl_t2 =
    Core.(
      Clause.Adorned.clause
        Lit.Adorned.(from_raw lit_t_head ~bpatt:bb)
        Lit.Adorned.[ from_raw lit_g2 ~bpatt:bf; from_raw lit_t_body ~bpatt:bb ])
  ;;

  let cl_ct =
    Core.(
      Clause.Adorned.clause
        Lit.Adorned.(from_raw lit_ct ~bpatt:ff)
        Lit.Adorned.
          [ from_raw lit_n1 ~bpatt:f
          ; from_raw lit_n2 ~bpatt:f
          ; neg @@ from_raw lit_t_head ~bpatt:bb
          ])
  ;;

  let cls_adorned = [ cl_t1; cl_t2; cl_ct ]
  let strata = [ [ cl_t1; cl_t2 ]; [ cl_ct ] ]
  let queries = [ pr_ct ]
  let prg = Core.Program.Adorned.(sorted @@ program cls_adorned queries [] [])

  let prg_strat =
    Core.Program.Stratified.(sorted { strata; queries; params = []; data = [] })
  ;;
end

module Stratified = struct
  module SocialInsurance = struct
    let ty_employee = Core.Ty.(named "employee")
    let ty_regular = Core.Ty.(named "regular")
    let ty_student = Core.Ty.(named "student")

    let subtys =
      [ ty_employee, Core.Ty.Symbol
      ; ty_regular, ty_employee
      ; ty_student, ty_employee
      ]
    ;;

    let trg =
      Core.Ty.Map.of_alist_exn
        Core.Ty.
          [ Symbol, Set.of_list [ Symbol; ty_employee; ty_regular; ty_student ]
          ; ty_employee, Set.of_list [ ty_employee; ty_regular; ty_student ]
          ; ty_regular, Set.singleton ty_regular
          ; ty_student, Set.singleton ty_student
          ]
    ;;

    (* -- Names ------------------------------------------------------------- *)

    let nm_regular = Core.Name.from_string "regular"
    let nm_student = Core.Name.from_string "student"
    let nm_age = Core.Name.from_string "age"
    let nm_employee = Core.Name.from_string "employee"
    let nm_salary = Core.Name.from_string "salary"
    let nm_socins = Core.Name.from_string "socins"
    let nm_mult = Core.Name.from_string "mult"
    let nm_eq = Core.Name.from_string "eq"
    let nm_query = Core.Name.from_string "query"

    (* -- Predicates -------------------------------------------------------- *)
    let pr_regular = Core.(Pred.pred ~arity:1 nm_regular)
    let pr_student = Core.(Pred.pred ~arity:1 nm_student)
    let pr_age = Core.(Pred.pred ~arity:2 nm_age)
    let pr_employee = Core.(Pred.pred ~arity:1 nm_employee)
    let pr_salary = Core.(Pred.pred ~arity:2 nm_salary)
    let pr_socins = Core.(Pred.pred ~arity:2 nm_socins)
    let pr_mult = Core.(Pred.pred ~arity:3 nm_mult)
    let pr_eq = Core.(Pred.pred ~arity:2 nm_eq)
    let pr_query = Core.(Pred.pred ~arity:2 nm_query)

    (* -- TTCs -------------------------------------------------------------- *)

    let ttc_regular = Core.TTC.ttc [ ty_regular ]
    let ttc_student = Core.TTC.ttc [ ty_student ]
    let ttc_age = Core.TTC.ttc [ ty_employee; Core.Ty.Real ]

    let datas =
      [ nm_regular, ttc_regular; nm_student, ttc_student; nm_age, ttc_age ]
    ;;

    (* -- Typings ----------------------------------------------------------- *)
    let typing_mult =
      Core.Typing.singleton
      @@ Core.TTC.ttc
           Core.Ty.[ Number; Number; Number ]
           ~equiv:Core.Partition.(singleton @@ Int.Set.of_list [ 0; 1; 2 ])
    ;;

    let typing_eq =
      Core.Typing.singleton
      @@ Core.TTC.ttc
           Core.Ty.[ Top; Top ]
           ~equiv:Core.Partition.(singleton @@ Int.Set.of_list [ 0; 1 ])
    ;;

    let typing_employee =
      Core.Typing.of_list Core.TTC.[ ttc [ ty_regular ]; ttc [ ty_student ] ]
    ;;

    let typing_salary =
      Core.Typing.of_list
        Core.TTC.
          [ ttc [ ty_regular; Core.Ty.Real ]; ttc [ ty_student; Core.Ty.Real ] ]
    ;;

    let typing_socins =
      Core.Typing.of_list
        Core.TTC.
          [ ttc [ ty_regular; Core.Ty.Real ]; ttc [ ty_student; Core.Ty.Real ] ]
    ;;

    let typing_query =
      Core.Typing.of_list Core.TTC.[ ttc [ ty_student; Core.Ty.Real ] ]
    ;;

    (* -- Extralogical predicate information -------------------------------- *)

    let ti_mult =
      Core.(
        TypingEnv.
          { typing = typing_mult
          ; nature = Nature.Extralogical []
          ; cstr =
              Constraint.(
                of_list
                  Atomic.
                    [ of_list [ 0; 1 ]; of_list [ 0; 2 ]; of_list [ 1; 2 ] ])
          })
    ;;

    let ti_eq =
      Core.(
        TypingEnv.
          { typing = typing_eq
          ; nature = Nature.Extralogical []
          ; cstr = Constraint.(of_list Atomic.[ of_list [ 0 ]; of_list [ 1 ] ])
          })
    ;;

    (* -- Extensional typings ----------------------------------------------- *)

    let tyenv0 =
      Core.TypingEnv.typing_env
        ~params:[]
        ~datas
        ~preds:
          [ nm_mult, Some ti_mult
          ; nm_eq, Some ti_eq
          ; nm_employee, None
          ; nm_salary, None
          ; nm_socins, None
          ; nm_query, None
          ]
    ;;

    let tyenv1 =
      Core.TypingEnv.update_pred_typing_exn
        tyenv0
        ~name:nm_employee
        ~typing:typing_employee
    ;;

    let tyenv2 =
      Core.TypingEnv.update_pred_typing_exn
        tyenv1
        ~name:nm_salary
        ~typing:typing_salary
    ;;

    let tyenv3 =
      Core.TypingEnv.update_pred_typing_exn
        tyenv2
        ~name:nm_socins
        ~typing:typing_socins
    ;;

    let tyenv4 =
      Core.TypingEnv.update_pred_typing_exn
        tyenv3
        ~name:nm_query
        ~typing:typing_query
    ;;

    (* -- Literals appearing ------------------------------------------------ *)

    let vx, vy, vz, int20, int1, int50 =
      Core.Term.(
        ( var "x"
        , var "y"
        , var "z"
        , sym @@ Core.Symbol.from_float 20.0
        , sym @@ Core.Symbol.from_float 0.1
        , sym @@ Core.Symbol.from_float 50.0 ))
    ;;

    let f = Core.Binding.from_list [ Free ]
    let ff = Core.Binding.from_list [ Free; Free ]
    let bf = Core.Binding.from_list [ Bound; Free ]
    let bbf = Core.Binding.from_list [ Bound; Bound; Free ]

    let lit_employee_x =
      Core.Lit.Adorned.from_raw ~bpatt:f @@ Core.Lit.Raw.lit pr_employee [ vx ]
    ;;

    let lit_regular_x =
      Core.Lit.Adorned.from_raw ~bpatt:f @@ Core.Lit.Raw.lit pr_regular [ vx ]
    ;;

    let lit_student_x =
      Core.Lit.Adorned.from_raw ~bpatt:f @@ Core.Lit.Raw.lit pr_student [ vx ]
    ;;

    let lit_not_student_x =
      Core.Lit.Adorned.from_raw ~bpatt:f
      @@ Core.Lit.Raw.lit ~pol:Neg pr_student [ vx ]
    ;;

    let lit_salary_x_y =
      Core.Lit.Adorned.from_raw ~bpatt:ff
      @@ Core.Lit.Raw.lit pr_salary [ vx; vy ]
    ;;

    let lit_salary_x_z =
      Core.Lit.Adorned.from_raw ~bpatt:ff
      @@ Core.Lit.Raw.lit pr_salary [ vx; vz ]
    ;;

    let lit_socins_x_y =
      Core.Lit.Adorned.from_raw ~bpatt:ff
      @@ Core.Lit.Raw.lit pr_socins [ vx; vy ]
    ;;

    let lit_age_x_z =
      Core.Lit.Adorned.from_raw ~bpatt:ff @@ Core.Lit.Raw.lit pr_age [ vx; vz ]
    ;;

    let lit_mult_20_z_y =
      Core.Lit.Adorned.from_raw ~bpatt:bbf
      @@ Core.Lit.Raw.lit pr_mult [ int20; vz; vy ]
    ;;

    let lit_mult_1_z_y =
      Core.Lit.Adorned.from_raw ~bpatt:bbf
      @@ Core.Lit.Raw.lit pr_mult [ int1; vz; vy ]
    ;;

    let lit_eq_50_y =
      Core.Lit.Adorned.from_raw ~bpatt:bf
      @@ Core.Lit.Raw.lit pr_eq [ int50; vy ]
    ;;

    let lit_query_x_y =
      Core.Lit.Adorned.from_raw ~bpatt:ff
      @@ Core.Lit.Raw.lit pr_query [ vx; vy ]
    ;;

    (* -- Program strata ---------------------------------------------------- *)
    let stratum1 =
      Core.Clause.Adorned.
        [ clause lit_employee_x [ lit_regular_x ]
        ; clause lit_employee_x [ lit_student_x ]
        ]
    ;;

    let stratum2 =
      Core.Clause.Adorned.
        [ clause lit_salary_x_y [ lit_age_x_z; lit_mult_20_z_y; lit_employee_x ]
        ]
    ;;

    let stratum3 =
      Core.Clause.Adorned.
        [ clause
            lit_socins_x_y
            [ lit_salary_x_z
            ; lit_mult_1_z_y
            ; lit_employee_x
            ; lit_not_student_x
            ]
        ; clause lit_socins_x_y [ lit_eq_50_y; lit_student_x ]
        ]
    ;;

    let rel_socins =
      Core.Relation.(
        union
          (project ~flds:[ 0; 3 ]
          @@ restrict ~equiv:(1, 2)
          @@ restrict ~equiv:(0, 4)
          @@ restrict ~equiv:(0, 5)
          @@ product
               (product
                  (product
                     (pred pr_salary)
                     (project ~flds:[ 1; 2 ]
                     @@ pred pr_mult ~ty_idxs:[ 0, Core.Ty.Real ]))
                  (pred pr_employee))
               (comp @@ pred pr_student))
          (project ~flds:[ 1; 0 ]
          @@ product
               (project ~flds:[ 1 ] @@ pred pr_eq ~ty_idxs:[ 0, Core.Ty.Real ])
               (pred pr_student)))
    ;;

    let stratum4 =
      Core.Clause.Adorned.
        [ clause lit_query_x_y [ lit_socins_x_y; lit_student_x ] ]
    ;;

    let strata = [ stratum1; stratum2; stratum3; stratum4 ]
    let queries = [ pr_query ]

    let prog =
      Core.Program.Stratified.{ strata; queries; params = []; data = [] }
    ;;
  end

  module BikeShop = struct
    (* -- Types used & subtyping relationships ------------------------------ *)
    let ty_cycle = Core.Ty.(named "cycle")
    let ty_bicycle = Core.Ty.(named "bicycle")
    let ty_unicycle = Core.Ty.(named "unicycle")
    let ty_pedals = Core.Ty.(named "pedals")
    let ty_saddle = Core.Ty.(named "saddle")
    let ty_wheel = Core.Ty.(named "wheel")
    let ty_spokes = Core.Ty.(named "spokes")
    let ty_tire = Core.Ty.(named "tire")
    let ty_tube = Core.Ty.(named "tube")

    let subtys =
      Core.Ty.
        [ ty_cycle, Core.Ty.Symbol
        ; ty_bicycle, ty_cycle
        ; ty_unicycle, ty_cycle
        ; ty_pedals, Symbol
        ; ty_saddle, Symbol
        ; ty_wheel, Symbol
        ; ty_spokes, Symbol
        ; ty_tire, Symbol
        ; ty_tube, Symbol
        ]
    ;;

    let closure =
      Core.Ty.Map.of_alist_exn
        Core.Ty.
          [ ( Symbol
            , Set.of_list
                [ Symbol
                ; ty_cycle
                ; ty_bicycle
                ; ty_unicycle
                ; ty_pedals
                ; ty_saddle
                ; ty_wheel
                ; ty_spokes
                ; ty_tire
                ; ty_tube
                ] )
          ; ty_cycle, Set.of_list [ ty_cycle; ty_bicycle; ty_unicycle ]
          ; ty_bicycle, Set.singleton ty_bicycle
          ; ty_unicycle, Set.singleton ty_unicycle
          ; ty_pedals, Set.singleton ty_pedals
          ; ty_saddle, Set.singleton ty_saddle
          ; ty_wheel, Set.singleton ty_wheel
          ; ty_spokes, Set.singleton ty_spokes
          ; ty_tire, Set.singleton ty_tire
          ; ty_tube, Set.singleton ty_tube
          ]
    ;;

    (* -- Names ------------------------------------------------------------- *)
    let nm_cycle = Core.Name.from_string "cycle"
    let nm_unicycle = Core.Name.from_string "unicycle"
    let nm_bicycle = Core.Name.from_string "bicycle"
    let nm_wheel = Core.Name.from_string "wheel"
    let nm_tire = Core.Name.from_string "tire"
    let nm_hasPart = Core.Name.from_string "hasPart"
    let nm_hasPartTC = Core.Name.from_string "hasPart+"
    let nm_query = Core.Name.from_string "query"

    (* -- Predicates -------------------------------------------------------- *)
    let pr_cycle = Core.(Pred.pred ~arity:1 nm_cycle)
    let pr_unicycle = Core.(Pred.pred ~arity:4 nm_unicycle)
    let pr_bicycle = Core.(Pred.pred ~arity:4 nm_bicycle)
    let pr_wheel = Core.(Pred.pred ~arity:3 nm_wheel)
    let pr_tire = Core.(Pred.pred ~arity:2 nm_tire)
    let pr_hasPart = Core.(Pred.pred ~arity:2 nm_hasPart)
    let pr_hasPartTC = Core.(Pred.pred ~arity:2 nm_hasPartTC)
    let pr_query = Core.(Pred.pred ~arity:2 nm_query)

    (* -- TTCs -------------------------------------------------------------- *)
    let ttc_cycle = Core.TTC.ttc [ ty_cycle ]

    let ttc_unicycle =
      Core.TTC.ttc [ ty_unicycle; ty_saddle; ty_wheel; ty_pedals ]
    ;;

    let ttc_bicycle = Core.TTC.ttc [ ty_bicycle; ty_wheel; ty_wheel; ty_pedals ]
    let ttc_wheel = Core.TTC.ttc [ ty_wheel; ty_tire; ty_spokes ]
    let ttc_tire = Core.TTC.ttc [ ty_tire; ty_tube ]

    let datas =
      [ nm_cycle, ttc_cycle
      ; nm_unicycle, ttc_unicycle
      ; nm_bicycle, ttc_bicycle
      ; nm_wheel, ttc_wheel
      ; nm_tire, ttc_tire
      ]
    ;;

    (* -- Typings ----------------------------------------------------------- *)

    let typing_hasPart =
      Core.Typing.of_list
        Core.TTC.
          [ ttc [ ty_bicycle; ty_pedals ]
          ; ttc [ ty_bicycle; ty_wheel ]
          ; ttc [ ty_unicycle; ty_saddle ]
          ; ttc [ ty_unicycle; ty_wheel ]
          ; ttc [ ty_unicycle; ty_pedals ]
          ; ttc [ ty_wheel; ty_tire ]
          ; ttc [ ty_wheel; ty_spokes ]
          ; ttc [ ty_tire; ty_tube ]
          ]
    ;;

    let typing_hasPart_2 =
      Core.Typing.of_list
        Core.TTC.
          [ ttc [ ty_bicycle; ty_pedals; ty_bicycle; ty_pedals ]
          ; ttc [ ty_bicycle; ty_pedals; ty_bicycle; ty_wheel ]
          ; ttc [ ty_bicycle; ty_pedals; ty_unicycle; ty_saddle ]
          ; ttc [ ty_bicycle; ty_pedals; ty_unicycle; ty_wheel ]
          ; ttc [ ty_bicycle; ty_pedals; ty_unicycle; ty_pedals ]
          ; ttc [ ty_bicycle; ty_pedals; ty_wheel; ty_tire ]
          ; ttc [ ty_bicycle; ty_pedals; ty_wheel; ty_spokes ]
          ; ttc [ ty_bicycle; ty_pedals; ty_tire; ty_tube ]
          ; ttc [ ty_bicycle; ty_wheel; ty_bicycle; ty_pedals ]
          ; ttc [ ty_bicycle; ty_wheel; ty_bicycle; ty_wheel ]
          ; ttc [ ty_bicycle; ty_wheel; ty_unicycle; ty_saddle ]
          ; ttc [ ty_bicycle; ty_wheel; ty_unicycle; ty_wheel ]
          ; ttc [ ty_bicycle; ty_wheel; ty_unicycle; ty_pedals ]
          ; ttc [ ty_bicycle; ty_wheel; ty_wheel; ty_tire ]
          ; ttc [ ty_bicycle; ty_wheel; ty_wheel; ty_spokes ]
          ; ttc [ ty_bicycle; ty_wheel; ty_tire; ty_tube ]
          ; ttc [ ty_unicycle; ty_pedals; ty_bicycle; ty_pedals ]
          ; ttc [ ty_unicycle; ty_pedals; ty_bicycle; ty_wheel ]
          ; ttc [ ty_unicycle; ty_pedals; ty_unicycle; ty_saddle ]
          ; ttc [ ty_unicycle; ty_pedals; ty_unicycle; ty_wheel ]
          ; ttc [ ty_unicycle; ty_pedals; ty_unicycle; ty_pedals ]
          ; ttc [ ty_unicycle; ty_pedals; ty_wheel; ty_tire ]
          ; ttc [ ty_unicycle; ty_pedals; ty_wheel; ty_spokes ]
          ; ttc [ ty_unicycle; ty_pedals; ty_tire; ty_tube ]
          ; ttc [ ty_unicycle; ty_saddle; ty_bicycle; ty_pedals ]
          ; ttc [ ty_unicycle; ty_saddle; ty_bicycle; ty_wheel ]
          ; ttc [ ty_unicycle; ty_saddle; ty_unicycle; ty_saddle ]
          ; ttc [ ty_unicycle; ty_saddle; ty_unicycle; ty_wheel ]
          ; ttc [ ty_unicycle; ty_saddle; ty_unicycle; ty_pedals ]
          ; ttc [ ty_unicycle; ty_saddle; ty_wheel; ty_tire ]
          ; ttc [ ty_unicycle; ty_saddle; ty_wheel; ty_spokes ]
          ; ttc [ ty_unicycle; ty_saddle; ty_tire; ty_tube ]
          ; ttc [ ty_unicycle; ty_wheel; ty_bicycle; ty_pedals ]
          ; ttc [ ty_unicycle; ty_wheel; ty_bicycle; ty_wheel ]
          ; ttc [ ty_unicycle; ty_wheel; ty_unicycle; ty_saddle ]
          ; ttc [ ty_unicycle; ty_wheel; ty_unicycle; ty_wheel ]
          ; ttc [ ty_unicycle; ty_wheel; ty_unicycle; ty_pedals ]
          ; ttc [ ty_unicycle; ty_wheel; ty_wheel; ty_tire ]
          ; ttc [ ty_unicycle; ty_wheel; ty_wheel; ty_spokes ]
          ; ttc [ ty_unicycle; ty_wheel; ty_tire; ty_tube ]
          ; ttc [ ty_wheel; ty_spokes; ty_bicycle; ty_pedals ]
          ; ttc [ ty_wheel; ty_spokes; ty_bicycle; ty_wheel ]
          ; ttc [ ty_wheel; ty_spokes; ty_unicycle; ty_saddle ]
          ; ttc [ ty_wheel; ty_spokes; ty_unicycle; ty_wheel ]
          ; ttc [ ty_wheel; ty_spokes; ty_unicycle; ty_pedals ]
          ; ttc [ ty_wheel; ty_spokes; ty_wheel; ty_tire ]
          ; ttc [ ty_wheel; ty_spokes; ty_wheel; ty_spokes ]
          ; ttc [ ty_wheel; ty_spokes; ty_tire; ty_tube ]
          ; ttc [ ty_wheel; ty_tire; ty_bicycle; ty_pedals ]
          ; ttc [ ty_wheel; ty_tire; ty_bicycle; ty_wheel ]
          ; ttc [ ty_wheel; ty_tire; ty_unicycle; ty_saddle ]
          ; ttc [ ty_wheel; ty_tire; ty_unicycle; ty_wheel ]
          ; ttc [ ty_wheel; ty_tire; ty_unicycle; ty_pedals ]
          ; ttc [ ty_wheel; ty_tire; ty_wheel; ty_tire ]
          ; ttc [ ty_wheel; ty_tire; ty_wheel; ty_spokes ]
          ; ttc [ ty_wheel; ty_tire; ty_tire; ty_tube ]
          ; ttc [ ty_tire; ty_tube; ty_bicycle; ty_pedals ]
          ; ttc [ ty_tire; ty_tube; ty_bicycle; ty_wheel ]
          ; ttc [ ty_tire; ty_tube; ty_unicycle; ty_saddle ]
          ; ttc [ ty_tire; ty_tube; ty_unicycle; ty_wheel ]
          ; ttc [ ty_tire; ty_tube; ty_unicycle; ty_pedals ]
          ; ttc [ ty_tire; ty_tube; ty_wheel; ty_tire ]
          ; ttc [ ty_tire; ty_tube; ty_wheel; ty_spokes ]
          ; ttc [ ty_tire; ty_tube; ty_tire; ty_tube ]
          ]
    ;;

    let typing_hasPart_2_eq_1_2 =
      let equiv =
        Core.Partition.(
          of_list Int.Set.[ singleton 0; of_list [ 1; 2 ]; singleton 3 ])
      in
      Core.Typing.of_list
        Core.TTC.
          [ ttc [ ty_bicycle; ty_wheel; ty_wheel; ty_tire ] ~equiv
          ; ttc [ ty_bicycle; ty_wheel; ty_wheel; ty_spokes ] ~equiv
          ; ttc [ ty_unicycle; ty_wheel; ty_wheel; ty_tire ] ~equiv
          ; ttc [ ty_unicycle; ty_wheel; ty_wheel; ty_spokes ] ~equiv
          ; ttc [ ty_wheel; ty_tire; ty_tire; ty_tube ] ~equiv
          ]
    ;;

    let typing_hasPart_2_eq_1_2_proj_0_4 =
      Core.Typing.of_list
        Core.TTC.
          [ ttc [ ty_bicycle; ty_tire ]
          ; ttc [ ty_bicycle; ty_spokes ]
          ; ttc [ ty_unicycle; ty_tire ]
          ; ttc [ ty_unicycle; ty_spokes ]
          ; ttc [ ty_wheel; ty_tube ]
          ]
    ;;

    let typing_hasPartTC =
      Core.Typing.of_list
        Core.TTC.
          [ ttc [ ty_bicycle; ty_pedals ]
          ; ttc [ ty_bicycle; ty_wheel ]
          ; ttc [ ty_bicycle; ty_tire ]
          ; ttc [ ty_bicycle; ty_spokes ]
          ; ttc [ ty_bicycle; ty_tube ]
          ; ttc [ ty_unicycle; ty_saddle ]
          ; ttc [ ty_unicycle; ty_wheel ]
          ; ttc [ ty_unicycle; ty_tire ]
          ; ttc [ ty_unicycle; ty_spokes ]
          ; ttc [ ty_unicycle; ty_tube ]
          ; ttc [ ty_unicycle; ty_pedals ]
          ; ttc [ ty_wheel; ty_tire ]
          ; ttc [ ty_wheel; ty_spokes ]
          ; ttc [ ty_wheel; ty_tube ]
          ; ttc [ ty_tire; ty_tube ]
          ]
    ;;

    let typing_query =
      Core.Typing.of_list
        Core.TTC.
          [ ttc [ ty_wheel; ty_tire ]
          ; ttc [ ty_wheel; ty_spokes ]
          ; ttc [ ty_wheel; ty_tube ]
          ]
    ;;

    (* -- Typing environment -------------------------------------------------*)

    let tyenv0 =
      Core.TypingEnv.typing_env
        ~datas
        ~params:[]
        ~preds:[ nm_hasPart, None; nm_hasPartTC, None; nm_query, None ]
    ;;

    let tyenv1 =
      Core.TypingEnv.update_pred_typing_exn
        tyenv0
        ~name:nm_hasPart
        ~typing:typing_hasPart
    ;;

    let tyenv2 =
      Core.TypingEnv.update_pred_typing_exn
        tyenv0
        ~name:nm_hasPartTC
        ~typing:typing_hasPartTC
    ;;

    let tyenv3 =
      Core.TypingEnv.update_pred_typing_exn
        tyenv0
        ~name:nm_query
        ~typing:typing_query
    ;;

    (* - Literals used ------------------------------------------------------ *)

    let vx, vy, vz = Core.Term.(var "x", var "y", var "z")
    let ff = Core.Binding.(from_list [ Free; Free ])
    let fff = Core.Binding.(from_list [ Free; Free; Free ])
    let ffff = Core.Binding.(from_list [ Free; Free; Free; Free ])

    let lit_hasPartHead =
      Core.Lit.Adorned.from_raw ~bpatt:ff
      @@ Core.Lit.Raw.lit pr_hasPart [ vx; vy ]
    ;;

    let lit_hasPartBody =
      Core.Lit.Adorned.from_raw ~bpatt:ff
      @@ Core.Lit.Raw.lit pr_hasPart [ vz; vy ]
    ;;

    let lit_unicycle_12 =
      Core.Lit.Adorned.from_raw ~bpatt:ffff
      @@ Core.Lit.Raw.lit pr_unicycle Core.Term.[ vx; vy; wild (); wild () ]
    ;;

    let lit_unicycle_13 =
      Core.Lit.Adorned.from_raw ~bpatt:ffff
      @@ Core.Lit.Raw.lit pr_unicycle Core.Term.[ vx; wild (); vy; wild () ]
    ;;

    let lit_unicycle_14 =
      Core.Lit.Adorned.from_raw ~bpatt:ffff
      @@ Core.Lit.Raw.lit pr_unicycle Core.Term.[ vx; wild (); wild (); vy ]
    ;;

    let lit_bicycle_12 =
      Core.Lit.Adorned.from_raw ~bpatt:ffff
      @@ Core.Lit.Raw.lit pr_bicycle Core.Term.[ vx; vy; wild (); wild () ]
    ;;

    let lit_bicycle_13 =
      Core.Lit.Adorned.from_raw ~bpatt:ffff
      @@ Core.Lit.Raw.lit pr_bicycle Core.Term.[ vx; wild (); vy; wild () ]
    ;;

    let lit_bicycle_14 =
      Core.Lit.Adorned.from_raw ~bpatt:ffff
      @@ Core.Lit.Raw.lit pr_bicycle Core.Term.[ vx; wild (); wild (); vy ]
    ;;

    let lit_wheel_12 =
      Core.Lit.Adorned.from_raw ~bpatt:fff
      @@ Core.Lit.Raw.lit pr_wheel Core.Term.[ vx; vy; wild () ]
    ;;

    let lit_wheel_13 =
      Core.Lit.Adorned.from_raw ~bpatt:fff
      @@ Core.Lit.Raw.lit pr_wheel Core.Term.[ vx; wild (); vy ]
    ;;

    let lit_wheel_1 =
      Core.Lit.Adorned.from_raw ~bpatt:fff
      @@ Core.Lit.Raw.lit pr_wheel Core.Term.[ vx; wild (); wild () ]
    ;;

    let lit_tire =
      Core.Lit.Adorned.from_raw ~bpatt:ff @@ Core.Lit.Raw.lit pr_tire [ vx; vy ]
    ;;

    let lit_hasPartTC =
      Core.Lit.Adorned.from_raw ~bpatt:ff
      @@ Core.Lit.Raw.lit pr_hasPartTC [ vx; vy ]
    ;;

    let lit_hasPartTCBody =
      Core.Lit.Adorned.from_raw ~bpatt:ff
      @@ Core.Lit.Raw.lit pr_hasPartTC [ vx; vz ]
    ;;

    let lit_query =
      Core.Lit.Adorned.from_raw ~bpatt:ff
      @@ Core.Lit.Raw.lit pr_query [ vx; vy ]
    ;;

    (* -- Strata ---------------------------------------------------------------- *)

    let stratum1 =
      Core.Clause.Adorned.
        [ clause lit_hasPartHead [ lit_unicycle_12 ]
        ; clause lit_hasPartHead [ lit_unicycle_13 ]
        ; clause lit_hasPartHead [ lit_unicycle_14 ]
        ; clause lit_hasPartHead [ lit_bicycle_12 ]
        ; clause lit_hasPartHead [ lit_bicycle_13 ]
        ; clause lit_hasPartHead [ lit_bicycle_14 ]
        ; clause lit_hasPartHead [ lit_wheel_12 ]
        ; clause lit_hasPartHead [ lit_wheel_13 ]
        ; clause lit_hasPartHead [ lit_tire ]
        ]
    ;;

    let stratum2 =
      Core.Clause.Adorned.
        [ clause lit_hasPartTC [ lit_hasPartHead ]
        ; clause lit_hasPartTC [ lit_hasPartTCBody; lit_hasPartBody ]
        ]
    ;;

    let rel_hasPartTC =
      Core.Relation.(
        union
          (project ~flds:[ 0; 1 ] @@ pred pr_hasPart)
          (project ~flds:[ 0; 3 ]
          @@ restrict ~equiv:(1, 2)
          @@ product (pred pr_hasPartTC) (pred pr_hasPart)))
    ;;

    let stratum3 =
      Core.Clause.Adorned.[ clause lit_query [ lit_wheel_1; lit_hasPartTC ] ]
    ;;

    (* -- Program --------------------------------------------------------------- *)
    let strata = [ stratum1; stratum2; stratum3 ]
    let queries = [ pr_query ]

    let prog =
      Core.Program.Stratified.{ strata; queries; data = []; params = [] }
    ;;
  end
end
