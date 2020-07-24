module type S = sig
  module Atom : Atom.S
  include Subgoal.S with module Ops := OpSet.Head and module Atom := Atom
  include HasCoreRepr.S with type t := t and type repr := Atom.repr
end

module Make (Atom : Atom.S) : S with module Atom := Atom = struct
  include Subgoal.Make (OpSet.Head) (Atom)

  module Make (M : SourceM.S) = struct
    module AtomM = Atom.Make (M)

    let to_core t =
      match proj t with
      | SAtom { elem; _ } -> AtomM.to_core elem
      | _ -> M.err_head_not_atom (region t)
    ;;
  end
end

module Term = Make (Atom.Term)
module Tmvar = Make (Atom.Tmvar)
module Symbol = Make (Atom.Symbol)
