open Core_kernel

module type S1 = sig
  type 'a t

  module Traversable (A : Applicative.S) : sig
    val traverse : 'a t -> f:('a -> 'b A.t) -> 'b t A.t
    val sequence : 'a A.t t -> 'a t A.t
  end

  module Traversable2 (A : Applicative.S2) : sig
    val traverse : 'a t -> f:('a -> ('b, 'e) A.t) -> ('b t, 'e) A.t
    val sequence : ('a, 'e) A.t t -> ('a t, 'e) A.t
  end

  module Traversable3 (A : Applicative.S3) : sig
    val traverse : 'a t -> f:('a -> ('b, 'd, 'e) A.t) -> ('b t, 'd, 'e) A.t
    val sequence : ('a, 'd, 'e) A.t t -> ('a t, 'd, 'e) A.t
  end
end

module type S2 = sig
  type ('a, 'b) t

  module Traversable (A : Applicative.S) : sig
    val traverse : ('a, 'b) t -> f:('a -> 'c A.t) -> ('c, 'b) t A.t
    val sequence : ('a A.t, 'b) t -> ('a, 'b) t A.t
  end

  module Traversable2 (A : Applicative.S2) : sig
    val traverse : ('a, 'b) t -> f:('a -> ('c, 'e) A.t) -> (('c, 'b) t, 'e) A.t
    val sequence : (('a, 'e) A.t, 'b) t -> (('a, 'b) t, 'e) A.t
  end

  module Traversable3 (A : Applicative.S3) : sig
    val traverse
      :  ('a, 'b) t
      -> f:('a -> ('c, 'd, 'e) A.t)
      -> (('c, 'b) t, 'd, 'e) A.t

    val sequence : (('a, 'd, 'e) A.t, 'b) t -> (('a, 'b) t, 'd, 'e) A.t
  end
end

module List : S1 with type 'a t := 'a list = struct
  module Traversable (A : Applicative.S) = struct
    let traverse t ~f =
      List.fold_right
        t
        ~init:A.(return [])
        ~f:A.(fun x accu -> map2 ~f:List.cons (f x) accu)
    ;;

    let sequence x = traverse x ~f:Fn.id
  end

  module Traversable2 (A : Applicative.S2) = struct
    let traverse t ~f =
      List.fold_right
        t
        ~init:A.(return [])
        ~f:A.(fun x accu -> map2 ~f:List.cons (f x) accu)
    ;;

    let sequence x = traverse x ~f:Fn.id
  end

  module Traversable3 (A : Applicative.S3) = struct
    let traverse t ~f =
      List.fold_right
        t
        ~init:A.(return [])
        ~f:A.(fun x accu -> map2 ~f:List.cons (f x) accu)
    ;;

    let sequence x = traverse x ~f:Fn.id
  end
end

module Option : S1 with type 'a t := 'a option = struct
  module Traversable (A : Applicative.S) = struct
    let traverse t ~f =
      Option.value_map
        t
        ~default:A.(return None)
        ~f:Fn.(compose A.(map ~f:Option.some) f)
    ;;

    let sequence x = traverse x ~f:Fn.id
  end

  module Traversable2 (A : Applicative.S2) = struct
    let traverse t ~f =
      Option.value_map
        t
        ~default:A.(return None)
        ~f:Fn.(compose A.(map ~f:Option.some) f)
    ;;

    let sequence x = traverse x ~f:Fn.id
  end

  module Traversable3 (A : Applicative.S3) = struct
    let traverse t ~f =
      Option.value_map
        t
        ~default:A.(return None)
        ~f:Fn.(compose A.(map ~f:Option.some) f)
    ;;

    let sequence x = traverse x ~f:Fn.id
  end
end

module Result : S2 with type ('a, 'b) t := ('a, 'b) result = struct
  module Traversable (A : Applicative.S) = struct
    let traverse t ~f =
      match t with
      | Ok x -> A.map ~f:(fun x -> Ok x) @@ f x
      | Error x -> A.return (Error x)
    ;;

    let sequence x = traverse x ~f:Fn.id
  end

  module Traversable2 (A : Applicative.S2) = struct
    let traverse t ~f =
      match t with
      | Ok x -> A.map ~f:(fun x -> Ok x) @@ f x
      | Error x -> A.return (Error x)
    ;;

    let sequence x = traverse x ~f:Fn.id
  end

  module Traversable3 (A : Applicative.S3) = struct
    let traverse t ~f =
      match t with
      | Ok x -> A.map ~f:(fun x -> Ok x) @@ f x
      | Error x -> A.return (Error x)
    ;;

    let sequence x = traverse x ~f:Fn.id
  end
end
