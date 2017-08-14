(* Info from OPAM file needed to construct the Esy package.json *)
type t = {
  name : string;
  version : string;
  (* list of (name, constraint) pairs for regular dependencies *)
  dependencies : (string * string) list;
  (* list of (name, constraint) pairs for optional dependencies *)
  optional_dependencies : (string * string) list;
  substs : string list;
  (* list of expanded build commands *)
  build : string list list;
  (* list of expanded install commands *)
  install : string list list;
  (* list of env var name-value pairs *)
  exported_env : (string * string) list;
}

module CleanupRe = struct
  let make_global_re v =
    Js.Re.fromStringWithFlags ~flags:"g" v

  let find_underscore_re =
    make_global_re "(_+)"

  let find_non_numbers_re =
    make_global_re "[^0-9]"

  let find_leading_zeroes_re =
    make_global_re "^0+"

  let find_at_re= make_global_re "@"
  let find_dash_re = make_global_re "\-"
  let find_slash_re = make_global_re "\/"
  let find_dot_re = make_global_re "\."
end

let to_npm_name name =
  "@opam-alpha/" ^ name

let to_env_name name =
  name
  (* This has to be done before the other replacements. *)
  |> Js.String.replaceByRe CleanupRe.find_underscore_re "$1__"
  |> Js.String.replaceByRe CleanupRe.find_at_re ""
  |> Js.String.replaceByRe CleanupRe.find_dot_re "__dot__"
  |> Js.String.replaceByRe CleanupRe.find_slash_re "__slash__"
  |> Js.String.replaceByRe CleanupRe.find_dash_re "_"

let to_npm_version version =
  let norm_version_segment v =
    let v =
      v
      |> Js.String.replaceByRe CleanupRe.find_non_numbers_re ""
      |> Js.String.replaceByRe CleanupRe.find_leading_zeroes_re ""
    in
    if v = "" then "0" else v
  in
  let (version, suffix) = if Js.String.includes "+" version then
      let parts = Js.String.splitAtMost ~limit:2 "+" version in
      let version = Array.get parts 0 and suffix = Array.get parts 1 in
      let suffix = Js.String.replaceByRe CleanupRe.find_non_numbers_re "" suffix in
      (version, suffix)
    else
      (version, "")
  in
  let parts = Array.to_list (Js.String.splitAtMost ~limit:3 "." version) in
  match parts with
  | major::[] ->
    let major = (norm_version_segment major) in
    let minor = (norm_version_segment suffix) in
    major ^ "." ^ minor ^ ".0"
  | major::minor::[] ->
    let major = (norm_version_segment major) in
    let minor = (norm_version_segment (minor ^ suffix)) in
    major ^ "." ^ minor ^ ".0"
  | major::minor::patch::[] ->
    let major = (norm_version_segment major) in
    let minor = (norm_version_segment minor) in
    let patch = (norm_version_segment patch) in
    major ^ "." ^ minor ^ "." ^ patch
  | _ ->
    version

let render_relop (op : OpamParserTypes.relop) =
  match op with
  | `Leq -> " <= "
  | `Lt -> " < "
  | `Neq -> "!="
  | `Eq -> "="
  | `Geq -> " >= "
  | `Gt -> " > "

let rec render_formula formula =
  let rec render_filter (filter: OpamTypes.filter) =
    match filter with
    | OpamTypes.FBool _ -> ""
    | OpamTypes.FString s -> to_npm_version s
    | OpamTypes.FIdent _ -> ""
    | OpamTypes.FOp (a, op, b) -> (render_filter a) ^ (render_relop op) ^ (render_filter b)
    | OpamTypes.FAnd (a, b) -> (render_filter a) ^ " " ^ (render_filter b)
    | OpamTypes.FOr (a, b) -> (render_filter a) ^ " || " ^ (render_filter b)
    | OpamTypes.FNot _ -> ""
    | OpamTypes.FDefined _ -> ""
    | OpamTypes.FUndef _ -> ""
  in
  let render_item item =
    match item with
    | OpamTypes.Filter item ->
      render_filter item
    | OpamTypes.Constraint (op, item) ->
      (render_relop op) ^ (render_filter item)
  in
  match formula with
  | OpamFormula.Empty -> "*"
  | OpamFormula.Atom item -> render_item item
  | OpamFormula.Block f -> render_formula f
  | OpamFormula.And (a, b) -> (render_formula a) ^ " " ^ (render_formula b)
  | OpamFormula.Or (a, b) -> (render_formula a) ^ " || " ^ (render_formula b)

let rec flatten_formula formula =
  match formula with
  | OpamFormula.Empty -> []
  | OpamFormula.Atom item -> [item]
  | OpamFormula.Block f -> flatten_formula f
  | OpamFormula.And (a, b) -> (flatten_formula a) @ (flatten_formula b)
  | OpamFormula.Or (a, b) -> (flatten_formula a) @ (flatten_formula b)

let render_opam_depends depends =
  (** We flatten packages formula because esy-npm cannot express formulas over
      packages. In the current impl. we just assume thatany package mentioned
      in original opam formula is required. *)
  let depends = flatten_formula depends in
  List.map (fun (name, constr) ->
      let name = name |> OpamPackage.Name.to_string |> to_npm_name in
      let constr = render_formula constr in
      (name, constr)
    ) depends

let render_opam_build opam_name env (commands: OpamTypes.command list) =
  let render_args args =
    List.map
      (* XXX: We ignore filters for now *)
      (fun (arg, _filter) -> match arg with
         | OpamTypes.CString arg ->
           OpamFilter.expand_string ~partial:true env arg
         | OpamTypes.CIdent name ->
           (match name with
            | "name" -> opam_name
            | "make" -> "make"
            | "jobs" -> "4"
            | "bin" -> "$cur__bin"
            | "prefix" -> "$cur__install"
            | "lib" -> "$cur__lib"
            | "sbin" -> "$cur__sbin"
            | "doc" -> "$cur__doc"
            | "man" -> "$cur__man"
            | "ocaml-native" -> "true"
            | "ocaml-native-dynlink" -> "true"
            | "pinned" -> "false"
            | name -> name))
      args
  in
  List.map
    (* XXX: We ignore filters for now *)
    (fun (args, _filter) -> render_args args)
    commands

let render_opam opam_name opam_version opam =
  let version = to_npm_version opam_version in
  let env (var: OpamVariable.Full.t) =
    let variable = OpamVariable.Full.variable var in
    let scope = OpamVariable.Full.scope var in
    let name = OpamVariable.to_string variable in
    (* Few helpers for common constructs *)
    let
      t = Some (OpamVariable.bool true) and
      f = Some (OpamVariable.bool false) and
      s value = Some (OpamVariable.string value)
    in
    let res = match (scope, name) with

      | (OpamVariable.Full.Package name, var) ->
        let name = to_env_name (OpamPackage.Name.to_string name) in
        begin match var with
          | "installed" ->
            s ("${" ^ name ^ "_installed:-false}")
          | "enable" ->
            s ("${" ^ name ^ "_enable:-disable}")
          | "version" ->
            s ("${" ^ name ^ "_version}")
          | "bin" ->
            s (name ^ "__bin")
          | "share" ->
            s (name ^ "__share")
          | "lib" ->
            s (name ^ "__lib")
          | _ ->
            s ""
        end

      | (OpamVariable.Full.Global, "ocaml-native") -> t
      | (OpamVariable.Full.Global, "ocaml-native-dynlink") -> t
      | (OpamVariable.Full.Global, "make") -> s "make"
      | (OpamVariable.Full.Global, "jobs") -> s "4"
      | (OpamVariable.Full.Global, "user") -> s "$USER"
      | (OpamVariable.Full.Global, "group") -> s "$USER"
      | (OpamVariable.Full.Global, "pinned") -> f

      (** TODO: Is Self/Global correct here? Not sure if we need to duplicate them *)
      | (OpamVariable.Full.Self, "name") -> s opam_name
      | (OpamVariable.Full.Global, "name") -> s opam_name
      | (OpamVariable.Full.Self, "build") -> s "$cur__target_dir"
      | (OpamVariable.Full.Global, "build") -> s "$cur__target_dir"
      | (OpamVariable.Full.Self, "bin") -> s "$cur__bin"
      | (OpamVariable.Full.Global, "bin") -> s "$cur__bin"
      | (OpamVariable.Full.Self, "prefix") -> s "$cur__install"
      | (OpamVariable.Full.Global, "prefix") -> s "$cur__install"
      | (OpamVariable.Full.Self, "lib") -> s "$cur__lib"
      | (OpamVariable.Full.Global, "lib") -> s "$cur__lib"
      | (OpamVariable.Full.Self, "stublibs") -> s "$cur__lib/stublibs"
      | (OpamVariable.Full.Global, "stublibs") -> s "$cur__lib/stublibs"
      | (OpamVariable.Full.Self, "etc") -> s "$cur__etc"
      | (OpamVariable.Full.Global, "etc") -> s "$cur__etc"
      | (OpamVariable.Full.Self, "sbin") -> s "$cur__sbin"
      | (OpamVariable.Full.Global, "sbin") -> s "$cur__sbin"
      | (OpamVariable.Full.Self, "doc") -> s "$cur__doc"
      | (OpamVariable.Full.Global, "doc") -> s "$cur__doc"
      | (OpamVariable.Full.Self, "man") -> s "$cur__man"
      | (OpamVariable.Full.Global, "man") -> s "$cur__man"
      | (OpamVariable.Full.Self, "share") -> s "$cur__share"
      | (OpamVariable.Full.Global, "share") -> s "$cur__share"

      | (_, _name) -> None
    in res
  in

  let dependencies = render_opam_depends (OpamFile.OPAM.depends opam) in
  let optional_dependencies = render_opam_depends (OpamFile.OPAM.depopts opam) in
  let substs = List.map OpamFilename.Base.to_string (OpamFile.OPAM.substs opam) in
  let build = render_opam_build opam_name env (OpamFile.OPAM.build opam) in
  let install = render_opam_build opam_name env (OpamFile.OPAM.install opam) in
  let exported_env = let prefix = to_env_name opam_name in [
      (prefix ^ "_version", version);
      (prefix ^ "_installed", "true");
      (prefix ^ "_enable", "enable");
    ]
  in

  {
    name = to_npm_name opam_name;
    version;
    dependencies;
    optional_dependencies;
    substs;
    build;
    install;
    exported_env;
  }
