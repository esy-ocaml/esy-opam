type result = {
  dependencies: (OpamTypes.name * string) list;
  build: string list;
}

let render_relop (op : OpamParserTypes.relop) =
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

let rec list_of_filter_atoms (filter: OpamTypes.filter) =
  match filter with
  | OpamTypes.FBool _ -> ""
  | OpamTypes.FString s -> s
  | OpamTypes.FIdent _ -> ""
  | OpamTypes.FOp (a, op, b) -> (list_of_filter_atoms a) ^ (render_relop op) ^ (list_of_filter_atoms b)
  | OpamTypes.FAnd (a, b) -> (list_of_filter_atoms a) ^ " " ^ (list_of_filter_atoms b)
  | OpamTypes.FOr (a, b) -> (list_of_filter_atoms a) ^ " || " ^ (list_of_filter_atoms b)
  | OpamTypes.FNot _ -> ""
  | OpamTypes.FDefined _ -> ""
  | OpamTypes.FUndef _ -> ""

let render_version_constraint filter =
  let filter = list_of_formula_atoms filter in
  List.map
    (fun f ->
       match f with
       | OpamTypes.Filter f ->
         list_of_filter_atoms f
       | OpamTypes.Constraint (op, f) ->
         (render_relop op) ^ (list_of_filter_atoms f))
    filter

let render_opam_depends depends =
  let depends = list_of_formula_atoms depends in
  let dependencies = List.map (fun (name, constr) ->
      let constr = render_version_constraint constr in
      let constr = String.concat " " constr in
      [name, if constr == "" then "*" else constr]) depends in
  List.concat dependencies

let render_opam_build env (commands: OpamTypes.command list) =
  let render_args args =
    let args = List.map
        (fun (arg, _filter) -> match arg with
           | OpamTypes.CString arg -> OpamFilter.expand_string env arg
           | OpamTypes.CIdent name -> name)
        args
    in String.concat " " args
  in
  List.map
    (fun (args, _filter) -> render_args args)
    commands

let render_opam package_name opam =
  let env (var: OpamVariable.Full.t) =
    let variable = OpamVariable.Full.variable var in
    let name = OpamVariable.to_string variable in
    let
      t = Some (OpamVariable.B true) and
      f = Some (OpamVariable.B false) and
      s value = Some (OpamVariable.S value)
    in
    match name with
    | "installed" -> t
    | "enable" -> t
    | "ocaml-native" -> t
    | "ocaml-native-dynlink" -> t
    | "name" -> s package_name
    | "make" -> s "make"
    | "jobs" -> s "4"
    | "bin" -> s "$cur__bin"
    | "prefix" -> s "$cur__install"
    | "lib" -> s "$cur__lib"
    | "sbin" -> s "$cur__sbin"
    | "doc" -> s "$cur__doc"
    | "man" -> s "$cur__man"
    | "pinned" ->  f
    | _ -> None
  in
  let
    dependencies = render_opam_depends (OpamFile.OPAM.depends opam) and
    build = render_opam_build env (OpamFile.OPAM.build opam)
  in
  { dependencies = dependencies; build = build }
