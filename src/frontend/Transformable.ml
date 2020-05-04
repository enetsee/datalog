open Core_kernel

module type S0 = sig
  type t

  val transform_atom : t -> f:('a Atom.t -> 'a Atom.t) -> t

  (** Apply pure subgoal transformation to the head of a sentence where it 
      exists *)
  val transform_head : t -> f:('a Subgoal.t -> 'a Subgoal.t) -> t

  (** Apply pure subgoal transformation to the body of a sentence where it 
      exists *)
  val transform_body : t -> f:('a Subgoal.t -> 'a Subgoal.t) -> t

  val transform_clause : t -> f:(Clause.t -> Clause.t) -> t
  val transform_query : t -> f:(Query.t -> Query.t) -> t
  val transform_fact : t -> f:(Fact.t -> Fact.t) -> t

  module Effect (M : sig
    include Monad.S
    include Applicative.S with type 'a t := 'a t
  end) : sig
    val transform_atom : t -> f:('a Atom.t -> 'a Atom.t M.t) -> t M.t
    val transform_head : t -> f:('a Subgoal.t -> 'a Subgoal.t M.t) -> t M.t
    val transform_body : t -> f:('a Subgoal.t -> 'a Subgoal.t M.t) -> t M.t
    val transform_clause : t -> f:(Clause.t -> Clause.t M.t) -> t M.t
    val transform_query : t -> f:(Query.t -> Query.t M.t) -> t M.t
    val transform_fact : t -> f:(Fact.t -> Fact.t M.t) -> t M.t
  end
end
