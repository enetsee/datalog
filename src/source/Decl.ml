open Core_kernel
open Lib
open Reporting

module TyDecl = struct
  module Defn = struct
    type t =
      | SubTy of Ty.Prim.t Located.t
      | Union of Ty.Name.t Located.t * Ty.Name.t Located.t NonEmpty.t
    [@@deriving eq, compare]

    let fmt_vbar = Fmt.any " |@, "

    let pp ppf = function
      | SubTy prim -> (Located.pp Ty.Prim.pp) ppf prim
      | Union (ty, tys) ->
        Fmt.(
          hovbox
          @@ pair ~sep:fmt_vbar (Located.pp Ty.Name.pp)
          @@ list ~sep:fmt_vbar (Located.pp Ty.Name.pp))
          ppf
          (ty, NonEmpty.to_list tys)
    ;;

    include Pretty.Make0 (struct
      type nonrec t = t

      let pp = `NoPrec pp
    end)
  end

  type t =
    { name : Ty.Name.t Located.t
    ; defn : Defn.t
    }
  [@@deriving eq, compare]

  let pp ppf { name; defn } =
    match defn with
    | Defn.SubTy _ ->
      Fmt.(
        hbox
        @@ pair ~sep:(any " <: ") (any "type " ++ Located.pp Ty.Name.pp) Defn.pp)
        ppf
        (name, defn)
    | _ ->
      Fmt.(
        hovbox
        @@ pair
             ~sep:(any " =@, ")
             (any "type " ++ Located.pp Ty.Name.pp)
             Defn.pp)
        ppf
        (name, defn)
  ;;

  include Pretty.Make0 (struct
    type nonrec t = t

    let pp = `NoPrec pp
  end)
end

module PredDecl = struct
  type t =
    { name : Core.Pred.Name.t Located.t
    ; params : (Core.Tmvar.t * Ty.t Located.t) list
    }
  [@@deriving eq, compare]

  let pp_param ppf (v, ty) =
    Fmt.(hbox @@ pair ~sep:(any " : ") Core.Tmvar.pp (Located.pp Ty.pp))
      ppf
      (v, ty)
  ;;

  let pp ppf { name; params } =
    Fmt.(
      hovbox
      @@ pair
           (any "pred " ++ Located.pp Core.Pred.Name.pp)
           (hovbox @@ parens @@ list ~sep:comma pp_param))
      ppf
      (name, params)
  ;;

  include Pretty.Make0 (struct
    type nonrec t = t

    let pp = `NoPrec pp
  end)
end

type t =
  | DTy of TyDecl.t Located.t
  | DPred of PredDecl.t Located.t
[@@deriving eq, compare]

let pp ppf = function
  | DTy tydecl -> (Located.pp TyDecl.pp) ppf tydecl
  | DPred preddecl -> (Located.pp PredDecl.pp) ppf preddecl
;;

include Pretty.Make0 (struct
  type nonrec t = t

  let pp = `NoPrec pp
end)

(* -- Constructors ---------------------------------------------------------- *)

let ty decl = DTy decl
let pred decl = DPred decl

(* -- Destructors 0---------------------------------------------------------- *)

let lower_ty = function
  | DTy decl -> Some decl
  | _ -> None
;;

let lower_pred = function
  | DPred decl -> Some decl
  | _ -> None
;;
