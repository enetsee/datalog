module type S = sig
  type t

  val terms_of : t -> Term.t list
end
