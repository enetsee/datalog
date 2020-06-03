module type S = sig
  type t

  val preds_of : t -> Pred.t list
end
