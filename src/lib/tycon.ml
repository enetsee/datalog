module type S0 = sig
  type t
end

module type S1 = sig
  type 'a t
end

module type S2 = sig
  type ('a, 'b) t
end

module type S3 = sig
  type ('a, 'b, 'c) t
end
