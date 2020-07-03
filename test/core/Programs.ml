open Core_kernel

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

    (* -- Predicates -------------------------------------------------------- *)
    let pr_regular = Core.(Pred.pred ~arity:1 @@ Name.from_string "regular")
    let pr_student = Core.(Pred.pred ~arity:1 @@ Name.from_string "student")
    let pr_age = Core.(Pred.pred ~arity:2 @@ Name.from_string "age")
    let pr_employee = Core.(Pred.pred ~arity:1 @@ Name.from_string "employee")
    let pr_salary = Core.(Pred.pred ~arity:2 @@ Name.from_string "salary")
    let pr_socins = Core.(Pred.pred ~arity:2 @@ Name.from_string "socins")
    let pr_mult = Core.(Pred.pred ~arity:3 @@ Name.from_string "mult")
    let pr_eq = Core.(Pred.pred ~arity:2 @@ Name.from_string "eq")
    let pr_query = Core.(Pred.pred ~arity:2 @@ Name.from_string "query")

    (* -- Typings ----------------------------------------------------------- *)

    let typing_regular = Core.Typing.of_schema [ ty_regular ]
    let typing_student = Core.Typing.of_schema [ ty_student ]
    let typing_age = Core.Typing.of_schema [ ty_employee; Core.Ty.Real ]

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

    (* -- Extensional typings ----------------------------------------------- *)

    let edb0 =
      Core.Pred.Map.of_alist_exn
        [ pr_regular, typing_regular
        ; pr_student, typing_student
        ; pr_age, typing_age
        ; pr_mult, typing_mult
        ; pr_eq, typing_eq
        ]
    ;;

    let edb1 = Core.Pred.Map.add_exn ~key:pr_employee ~data:typing_employee edb0
    let edb2 = Core.Pred.Map.add_exn ~key:pr_salary ~data:typing_salary edb1
    let edb3 = Core.Pred.Map.add_exn ~key:pr_socins ~data:typing_socins edb2
    let edb4 = Core.Pred.Map.add_exn ~key:pr_query ~data:typing_query edb3

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

    let program =
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

    (* -- Predicates -------------------------------------------------------- *)
    let pr_cycle = Core.(Pred.pred ~arity:1 @@ Name.from_string "cycle")
    let pr_unicycle = Core.(Pred.pred ~arity:4 @@ Name.from_string "unicycle")
    let pr_bicycle = Core.(Pred.pred ~arity:4 @@ Name.from_string "bicycle")
    let pr_wheel = Core.(Pred.pred ~arity:3 @@ Name.from_string "wheel")
    let pr_tire = Core.(Pred.pred ~arity:2 @@ Name.from_string "tire")
    let pr_hasPart = Core.(Pred.pred ~arity:2 @@ Name.from_string "hasPart")
    let pr_hasPartTC = Core.(Pred.pred ~arity:2 @@ Name.from_string "hasPart+")
    let pr_query = Core.(Pred.pred ~arity:2 @@ Name.from_string "query")

    (* -- Typings ----------------------------------------------------------- *)

    let typing_cycle = Core.Typing.of_schema [ ty_cycle ]

    let typing_unicycle =
      Core.Typing.of_schema [ ty_unicycle; ty_saddle; ty_wheel; ty_pedals ]
    ;;

    let typing_bicycle =
      Core.Typing.of_schema [ ty_bicycle; ty_wheel; ty_wheel; ty_pedals ]
    ;;

    let typing_wheel = Core.Typing.of_schema [ ty_wheel; ty_tire; ty_spokes ]
    let typing_tire = Core.Typing.of_schema [ ty_tire; ty_tube ]

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

    (* -- Extensional schema -------------------------------------------------*)
    let edb0 =
      Core.Pred.Map.of_alist_exn
        [ pr_cycle, typing_cycle
        ; pr_unicycle, typing_unicycle
        ; pr_bicycle, typing_bicycle
        ; pr_wheel, typing_wheel
        ; pr_tire, typing_tire
        ]
    ;;

    let edb1 =
      Core.Pred.Map.of_alist_exn
        [ pr_cycle, typing_cycle
        ; pr_unicycle, typing_unicycle
        ; pr_bicycle, typing_bicycle
        ; pr_wheel, typing_wheel
        ; pr_tire, typing_tire
        ; pr_hasPart, typing_hasPart
        ]
    ;;

    let edb2 =
      Core.Pred.Map.of_alist_exn
        [ pr_cycle, typing_cycle
        ; pr_unicycle, typing_unicycle
        ; pr_bicycle, typing_bicycle
        ; pr_wheel, typing_wheel
        ; pr_tire, typing_tire
        ; pr_hasPart, typing_hasPart
        ; pr_hasPartTC, typing_hasPartTC
        ]
    ;;

    let edb3 =
      Core.Pred.Map.of_alist_exn
        [ pr_cycle, typing_cycle
        ; pr_unicycle, typing_unicycle
        ; pr_bicycle, typing_bicycle
        ; pr_wheel, typing_wheel
        ; pr_tire, typing_tire
        ; pr_hasPart, typing_hasPart
        ; pr_hasPartTC, typing_hasPartTC
        ; pr_query, typing_query
        ]
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
