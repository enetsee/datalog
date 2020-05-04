include Proj_intf

module Make (X : Minimal) : S with type t := X.t and module F := X.F = struct
  let proj = X.proj
  let proj_succ proj_pred x = X.F.map ~f:proj_pred @@ proj x
  let proj2 = proj_succ proj
  let proj3 = proj_succ proj2
  let proj4 = proj_succ proj3
  let proj5 = proj_succ proj4
end

module Make2 (X : Minimal2) : S2 with type 'a t := 'a X.t and module F := X.F =
struct
  let proj = X.proj
  let proj_succ proj_pred x = X.F.map ~f:proj_pred @@ proj x
  let proj2 x = (proj_succ proj) x
  let proj3 x = (proj_succ proj2) x
  let proj4 x = (proj_succ proj3) x
  let proj5 x = (proj_succ proj4) x
end
