module type S = sig
  module Atom : Atom.S
  include Subgoal.S with module Ops := OpSet.Head and module Atom := Atom
  include HasCoreRepr.S with type t := t and type repr := Atom.repr

  val check_extralogical_clash : t -> unit MonadCompile.t
end

module Make (Atom : Atom.S) : S with module Atom := Atom = struct
  include Subgoal.Make (OpSet.Head) (Atom)

  let check_extralogical_clash t =
    let aux = function
      | SubgoalF.SAtom { elem; _ } -> Atom.check_extralogical_clash elem
      | _ -> MonadCompile.return ()
    in
    cata aux t
  ;;

  let to_core t =
    match proj t with
    | SAtom { elem; _ } -> Atom.to_core elem
    | _ -> MonadCompile.(fail Err.(HeadNotAtom (region t)))
  ;;
end

module Term = Make (Atom.Term)
module Tmvar = Make (Atom.Tmvar)
module Symbol = Make (Atom.Symbol)
