include Fix_intf

module Make (F : Functor.S1) : S with module F := F = struct
  module T = struct
    module F = F

    type t = In of t F.t

    let embed f = In f
    let proj (In f) = f
  end

  include T
  include Recursive.Make (T)
  include Corecursive.Make (T)
  include Proj.Make (T)
end

module Make2 (F : Functor.S2) : S2 with module F := F = struct
  module T = struct
    module F = F

    type 'a t = In of ('a t, 'a) F.t

    let embed f = In f
    let proj (In f) = f
  end

  include T
  include Recursive.Make2 (T)
  include Corecursive.Make2 (T)
  include Proj.Make2 (T)
end
