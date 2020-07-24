open Core_kernel
open Reporting
open Core
open Lib

module X = struct
  type t =
    | Bad_Parse_State
    | Parse_Error of string option * Region.t
    | File_Not_Found of string
    | Reserved_Word of Region.t
    | Name_Shadowed of Name.t * Region.t
    | Head_Not_Atom of Region.t
    | Fact_Has_Wildcard of Region.t
    | Fact_Not_Range_Restricted of Region.t
    | Clause_Null_Op of Region.t
    | Clause_Neg of Region.t
    | Clause_Disj of Region.t
    | Unbound_Param of Name.t * Region.t
    | Unresolved_Export of Name.t * Region.t
    | No_Well_Moded_Order of Binding.t * Region.t
    | Range_Violations of Violation.t list
    | Negative_Cycles of (Pred.t * Pred.t) list
    | Empty_Relations of Pred.t list
  [@@deriving variants, eq]

  let colon = Fmt.(any "@;:@;")

  let pp_cycle ppf (src, dest) =
    Fmt.(
      hovbox
      @@ pair
           ~sep:sp
           (prefix (any "Negative cycle between predicate") @@ quote Pred.pp)
           (prefix (any "and predicate ") @@ quote Pred.pp))
      ppf
      (src, dest)
  ;;

  let pp ppf = function
    | Bad_Parse_State -> Fmt.string ppf "Bad parse state"
    | Parse_Error (None, region) ->
      Fmt.(hovbox @@ pair ~sep:colon Region.pp string)
        ppf
        (region, "A parse error occurred without a corresponding message")
    | Parse_Error (Some msg, region) ->
      Fmt.(hovbox @@ pair ~sep:colon Region.pp string) ppf (region, msg)
    | File_Not_Found path ->
      Fmt.(hovbox @@ (any "File not found@;" ++ quote ~mark:"`" string))
        ppf
        path
    | Reserved_Word region ->
      Fmt.(hovbox @@ pair ~sep:colon Region.pp string)
        ppf
        (region, "This name is reserved and cannot be used as an identifier")
    | Name_Shadowed (name, region) ->
      Fmt.(
        hovbox
        @@ pair ~sep:colon Region.pp
        @@ pair ~sep:sp (quote ~mark:"`" Name.pp) string)
        ppf
        (region, (name, "shadows an existing predicate, parameter or data"))
    | Head_Not_Atom region ->
      Fmt.(hovbox @@ pair ~sep:colon Region.pp string)
        ppf
        (region, "The head of this clause is not an atomic formula")
    | Fact_Has_Wildcard region ->
      Fmt.(hovbox @@ pair ~sep:colon Region.pp string)
        ppf
        ( region
        , "Facts cannot contain wildcards; instead, enumerate all values for \
           this attribute" )
    | Fact_Not_Range_Restricted region ->
      Fmt.(hovbox @@ pair ~sep:colon Region.pp string)
        ppf
        ( region
        , "Facts cannot contain variables; instead, specify a value or values \
           for this attribute" )
    | Clause_Null_Op region ->
      Fmt.(hovbox @@ pair ~sep:colon Region.pp string)
        ppf
        (region, "Nullary operators cannot be compiled")
    | Clause_Neg region ->
      Fmt.(hovbox @@ pair ~sep:colon Region.pp string)
        ppf
        ( region
        , "Negation can only be applied to atoms in core. Clause is not in \
           normal form" )
    | Clause_Disj region ->
      Fmt.(hovbox @@ pair ~sep:colon Region.pp string)
        ppf
        ( region
        , "Found an unexpected disjunction. This clause is not in normal form"
        )
    | No_Well_Moded_Order (bpatt, region) ->
      Fmt.(
        hovbox @@ pair ~sep:colon Region.pp @@ pair ~sep:sp string Binding.pp)
        ppf
        ( region
        , ( "No ordering of the clause is compatible with the binding pattern"
          , bpatt ) )
    | Range_Violations vs ->
      Fmt.(vbox @@ pair ~sep:colon string @@ list ~sep:cut Violation.pp)
        ppf
        ("The following range restriction violations could not be repaired", vs)
    | Negative_Cycles cycles ->
      Fmt.(vbox @@ pair ~sep:colon string @@ list ~sep:cut pp_cycle)
        ppf
        ( "The program cannot be stratified due to negative cyclical \
           dependency predicates"
        , cycles )
    | Unbound_Param (name, region) ->
      Fmt.(
        hovbox
        @@ pair
             ~sep:colon
             Region.pp
             (any "The parameter `#" ++ Name.pp ++ any "` has not been declared"))
        ppf
        (region, name)
    | Empty_Relations ps ->
      Fmt.(vbox @@ pair ~sep:colon string @@ list ~sep:cut Pred.pp)
        ppf
        ("The following predicates will always be empty", ps)
    | Unresolved_Export (name, region) ->
      Fmt.(
        hovbox
        @@ pair ~sep:colon Region.pp
        @@ (any "You are trying to export a predicate named "
           ++ quote ~mark:"`" Name.pp
           ++ any " which is not defined in this module"))
        ppf
        (region, name)
  ;;

  let pp = `NoPrec pp
end

include X
include Pretty.Make0 (X)
