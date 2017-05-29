module OPAM = OpamFile.OPAM

let parse_opam_file filename =
  let contents = Node_fs.readFileSync filename `utf8 in
  OPAM.read_from_string contents

let to_pkgjson_relop (op : OpamParserTypes.relop) =
  match op with
  | `Leq -> " <= "
  | `Lt -> " < "
  | `Neq -> "!="
  | `Eq -> "="
  | `Geq -> " >= "
  | `Gt -> " > "

let rec list_of_formula_atoms depends =
  match depends with
  | OpamFormula.Empty -> []
  | OpamFormula.Atom item -> [item]
  | OpamFormula.Block f -> list_of_formula_atoms f
  | OpamFormula.And (a, b) -> List.append (list_of_formula_atoms a) (list_of_formula_atoms b)
  | OpamFormula.Or (a, b) -> List.append (list_of_formula_atoms a) (list_of_formula_atoms b)

let rec flatten_filter (filter: OpamTypes.filter) =
  match filter with
  | OpamTypes.FBool _ -> ""
  | OpamTypes.FString s -> s
  | OpamTypes.FIdent _ -> ""
  | OpamTypes.FOp (a, op, b) -> (flatten_filter a) ^ (to_pkgjson_relop op) ^ (flatten_filter b)
  | OpamTypes.FAnd (a, b) -> (flatten_filter a) ^ " " ^ (flatten_filter b)
  | OpamTypes.FOr (a, b) -> (flatten_filter a) ^ " || " ^ (flatten_filter b)
  | OpamTypes.FNot _ -> ""
  | OpamTypes.FDefined _ -> ""
  | OpamTypes.FUndef _ -> ""

let to_pkgjson_version_constraint filter =
  let filter = list_of_formula_atoms filter in
  List.map (fun f ->
      match f with
      | OpamTypes.Filter f ->
        flatten_filter f
      | OpamTypes.Constraint (op, f) ->
        (to_pkgjson_relop op) ^ (flatten_filter f)
    ) filter

let to_pkgjson_dependencies depends =
  let depends = list_of_formula_atoms depends in
  let dependencies = List.map (fun (name, constr) ->
      let constr = to_pkgjson_version_constraint constr in
      let constr = String.concat " " constr in
      [name, if constr == "" then "*" else constr]) depends in
  List.concat dependencies

let () =
  let opam = parse_opam_file "./reason.opam" in
  let depends = OPAM.depends opam in
  let dependencies = to_pkgjson_dependencies depends in
  Js.log (Array.of_list dependencies);