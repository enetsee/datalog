open Lib

module type S1 = sig
  module F : Tycon.S1
  module G : Tycon.S1

  type nat = { apply : 'a. 'a F.t -> 'a G.t }
end
