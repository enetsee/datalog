module type S = sig
  module Nullary : Op.S
  module Unary : Op.S
  module Binary : Op.S
end

module Head :
  S
    with module Nullary = Op.Never
     and module Unary = Op.Never
     and module Binary = Op.Never = struct
  module Nullary = Op.Never
  module Unary = Op.Never
  module Binary = Op.Never
end

module Body :
  S
    with module Nullary = Op.Never
     and module Binary = Op.Binary
     and module Unary = Op.Unary = struct
  module Nullary = Op.Never
  module Unary = Op.Unary
  module Binary = Op.Binary
end
