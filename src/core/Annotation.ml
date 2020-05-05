open Reporting

type 'a t =
  | ABase of Region.t
  | ADelta of 'a t
  | ADependency of 'a t
  | AAdornment of 'a t
  | ARename of 'a t
  | AProvenance of 'a t

let base region = ABase region
