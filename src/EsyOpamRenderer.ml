module Lib = EsyOpamLib
module Version = EsyOpamVersion

(* Info from OPAM file needed to construct the Esy package.json *)
type t = {
  name : string;
  version : string;

  (* list of (name, constraint) pairs for regular dependencies *)
  dependencies : (string * string) list;
  (* list of (name, constraint) pairs for optional dependencies *)
  optional_dependencies : (string * string) list;
  (* list of (name, constraint) pairs for optional dependencies *)
  dev_dependencies : (string * string) list;

  ocaml_version_constaint : string option;

  substs : string list;

  patches : string list;

  (* list of expanded build commands *)
  build : string list list;
  (* list of expanded install commands *)
  install : string list list;
  (* list of env var name-value pairs *)
  exported_env : (string * string) list;
}

let to_npm_name name =
  "@opam/" ^ name

let to_env_name name =
  name
  (* This has to be done before the other replacements. *)
  |> Js.String.replaceByRe Lib.Re.find_underscore_re "$1__"
  |> Js.String.replaceByRe Lib.Re.find_at_re ""
  |> Js.String.replaceByRe Lib.Re.find_dot_re "__dot__"
  |> Js.String.replaceByRe Lib.Re.find_slash_re "__slash__"
  |> Js.String.replaceByRe Lib.Re.find_dash_re "_"


let to_npm_relop (op : OpamParserTypes.relop) =
  match op with
  | `Leq -> Some " <= "
  | `Lt -> Some " < "
  | `Eq -> Some "="
  | `Geq -> Some " >= "
  | `Gt -> Some " > "
  | `Neq -> None

let rec render_filter ?(test_val=false) ?(build_val=false) (filter: OpamTypes.filter) =
  match filter with
  | OpamTypes.FBool enabled -> (enabled, "")
  | OpamTypes.FString s -> (true, Version.opam_to_npm s)
  | OpamTypes.FIdent (_, name,_) ->
      let name = OpamVariable.to_string name in
      let enabled = match name with
        | "test" -> test_val
        | "build" -> build_val
        | _ -> false
      in
      (enabled, "")
  | OpamTypes.FOp (a, op, b) ->
    let (enabled_a, a) = render_filter a in
    let (enabled_b, b) = render_filter b in
    (match to_npm_relop op with
     | Some op ->
       (enabled_a && enabled_b, a ^ op ^ b)
     | None -> (enabled_a && enabled_b, ""))
  | OpamTypes.FAnd (a, b) ->
    let (enabled_a, a) = render_filter a in
    let (enabled_b, b) = render_filter b in
      (enabled_a && enabled_b, a ^ " " ^ b)
  | OpamTypes.FOr (a, b) ->
    let (enabled_a, a) = render_filter a in
    let (enabled_b, b) = render_filter b in
    (enabled_a || enabled_b, a ^ " || " ^ b)
  | OpamTypes.FNot _ -> (true, "")
  | OpamTypes.FDefined _ -> (true, "")
  | OpamTypes.FUndef _ -> (true, "")

let rec render_version_formula (formula : OpamFormula.version_formula) =
  match formula with
  | OpamFormula.Empty -> "*"
  | OpamFormula.Atom (relop, version) ->
      let relop = match to_npm_relop relop with
      | None -> ""
      | Some relop -> relop
      in
      let version = version |> OpamPackage.Version.to_string |> Version.opam_to_npm in
      relop ^ version
  | OpamFormula.Block f -> render_version_formula f
  | OpamFormula.And (a, b) -> (render_version_formula a) ^ " " ^ (render_version_formula b)
  | OpamFormula.Or (a, b) -> (render_version_formula a) ^ " || " ^ (render_version_formula b)

let rec flatten_formula formula =
  match formula with
  | OpamFormula.Empty -> []
  | OpamFormula.Atom item -> [item]
  | OpamFormula.Block f -> flatten_formula f
  | OpamFormula.And (a, b) -> (flatten_formula a) @ (flatten_formula b)
  | OpamFormula.Or (a, b) -> (flatten_formula a) @ (flatten_formula b)

let simplify_formula installed_packages formula =
  let lookup f =
    let (name, _) = f in
    let name = OpamPackage.Name.to_string name in
    match Lib.StringMap.find_opt name installed_packages with
    | Some true -> `True
    | Some false -> `False
    | None -> `Formula (OpamFormula.Atom f)
  in
  match OpamFormula.partial_eval lookup formula with
  | `Formula f -> f
  | _ -> OpamFormula.Empty

let render_opam_depends ~installed_packages env (depends : OpamTypes.filtered_formula) =

  let render ((name : OpamTypes.name), formula) =
    let name = name |> OpamPackage.Name.to_string |> to_npm_name in
    let formula = render_version_formula formula in
    (name, formula)
  in

  let installed_packages = match installed_packages with
  | Some installed_packages -> installed_packages
  | None -> Lib.StringMap.empty
  in

  depends
  |> OpamFilter.filter_formula ~default:true env
  |> simplify_formula installed_packages
  |> flatten_formula
  |> List.map render


(** This is a modified copy of OpamFilter.expand_string function
    See XXX: marks inside for changes we made.
*)
let expand_string ?(partial=false) ?default env text =
  let open OpamTypes in
  let open OpamTypesBase in
  let open OpamStd.Op in

  let escape_expansions =
    Re.replace_string Re.(compile @@ char '%') ~by:"%%"
  in

  let desugar_fident ((_packages,_var,_converter) as fident) =
    fident
    (**
       XXX: We don't desugar it!
       let enable = OpamVariable.of_string "enable" in
       if packages <> [] && var = enable && converter = None then
       packages, OpamVariable.of_string "installed", Some ("enable","disable")
       else fident
    *)
  in

  let resolve_ident ?(no_undef_expand=false) env fident =
    let open OpamStd.Option.Op in
    let packages,var,converter = desugar_fident fident in
    let bool_of_value = function
      | B b -> Some b
      | S s -> try Some (bool_of_string s) with Invalid_argument _ -> None
    in
    let resolve ?scopeNames name =
      let var = match name with
        | Some n -> OpamVariable.Full.create n var
        | None -> OpamVariable.Full.self var
      in
      env ?scopeNames var
    in
    let value_opt : variable_contents option = match packages with
      | [] -> env ?scopeNames:None (OpamVariable.Full.global var)
      | [name] -> resolve name
      | (name::_) as names ->
        begin match resolve ~scopeNames:names name with
        | Some v -> Some v
        | None ->
          List.fold_left (fun acc name ->
              if acc = Some false then acc else
                match resolve name with
                | Some (B true) -> acc
                | v -> v >>= bool_of_value)
            (Some true) names
          >>| fun b -> B b
        end
    in
    match converter, no_undef_expand with
    | Some (iftrue, iffalse), false ->
      (match value_opt >>= bool_of_value with
       | Some true -> FString iftrue
       | Some false -> FString iffalse
       | None -> FString iffalse)
    | _ ->
      (match value_opt with
       | Some (B b) -> FBool b
       | Some (S s) -> FString s
       | None -> FUndef (FIdent fident))
  in

  let to_string t =
    let rec aux ?(context=`Or) t =
      let paren ?(cond=false) f =
        if cond || OpamFormatConfig.(!r.all_parens)
        then Printf.sprintf "(%s)" f else f
      in
      match t with
      | FBool b    -> string_of_bool b
      | FString s  -> Printf.sprintf "%S" s
      | FIdent (pkgs,var,converter) ->
        OpamStd.List.concat_map "+"
          (function None -> "_" | Some p -> OpamPackage.Name.to_string p) pkgs ^
        (if pkgs <> [] then ":" else "") ^
        OpamVariable.to_string var ^
        (match converter with
         | Some (it,ifu) -> "?"^it^":"^ifu
         | None -> "")
      | FOp(e,s,f) ->
        paren ~cond:(context <> `Or && context <> `And)
          (Printf.sprintf "%s %s %s"
             (aux ~context:`Relop e) (OpamPrinter.relop s) (aux ~context:`Relop f))
      | FAnd (e,f) ->
        paren ~cond:(context <> `Or && context <> `And)
          (Printf.sprintf "%s & %s" (aux ~context:`And e) (aux ~context:`And f))
      | FOr (e,f)  ->
        paren ~cond:(context <> `Or)
          (Printf.sprintf "%s | %s" (aux e) (aux f))
      | FNot e     ->
        paren ~cond:(context = `Relop)
          (Printf.sprintf "!%s" (aux ~context:`Not e))
      | FDefined e ->
        paren ~cond:(context = `Relop)
          (Printf.sprintf "?%s" (aux ~context:`Defined e))
      | FUndef f -> Printf.sprintf "#undefined(%s)" (aux f)
    in
    aux t
  in

  let value_string ?default = function
    | FBool b -> string_of_bool b
    | FString s -> s
    | FUndef f ->
      (match default with
       | Some d -> d
       | None -> failwith ("Undefined string filter value: "^to_string f))
    | e -> raise (Invalid_argument ("value_string: "^to_string e))
  in

  let string_interp_regex =
    let open Re in
    let notclose =
      rep (alt [
          diff notnl (set "}");
          seq [char '}'; alt [diff notnl (set "%"); stop] ]
        ])
    in
    compile (alt [
        str "%%";
        seq [str "%{"; group (greedy notclose); opt (group (str "}%"))];
      ])
  in

  let default fident = match default, partial with
    | None, false -> None
    | Some df, false -> Some (df fident)
    | None, true -> Some (Printf.sprintf "%%{%s}%%" fident)
    | Some df, true -> Some (Printf.sprintf "%%{%s}%%" (df fident))
  in
  let env ?scopeNames v =
    if partial then
      match env ?scopeNames v with
      | Some (S s) -> Some (S (escape_expansions s))
      | x -> x
    else env ?scopeNames v
  in
  let f g =
    let str = Re.Group.get g 0 in
    if str = "%%" then (if partial then "%%" else "%")
    else if not (OpamStd.String.ends_with ~suffix:"}%" str) then
      str
    else
      let fident = String.sub str 2 (String.length str - 4) in
      resolve_ident ~no_undef_expand:partial env (filter_ident_of_string fident)
      |> value_string ?default:(default fident)
  in
  Re.replace string_interp_regex ~f text

let render_opam_build opam_name env (commands: OpamTypes.command list) =
  let render_args args =
    List.map
      (* XXX: We ignore filters for now *)
      (fun (arg, _filter) -> match arg with
         | OpamTypes.CString arg ->
           expand_string ~partial:true env arg
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

let render_opam_available (filter: OpamTypes.filter) =
  let rec find_constraints filter result =

    let add_ocaml_constraint op  version =
      let (major, minor, patch, tag) = Version.of_opam version in
      (* This is how release ocaml compilers on npm - add trailing zeros to
       * patch version to allow several revisions of a single OCaml compiler
       * versions.
       *)
      let patch = match patch with | "0" -> "0" | v -> v ^ "000" in
      let version = (major, minor, patch, tag) in
      let npm_version = Version.render version in
      match op, to_npm_relop op, tag with
      | `Eq, _, None ->
        (* Convert `Eq to ~ as we might have different revisions of ocaml on npm
         * so that =4.03.3 means ~4.2.3000 which can match 4.2.3007.
         *
         * Do it only if tag is not provided in a constraint!
         *)
        let constr = "~" ^ npm_version in
        Some constr
      | _, Some op, _ ->
        let constr = op ^ npm_version in
        Some constr
      | _, None, _ -> None
    in

    match filter with

    | OpamTypes.FOp (OpamTypes.FIdent (_, var, _), op, OpamTypes.FString version) ->
      let var = OpamVariable.to_string var in
      if var == "ocaml-version" || var == "compiler" then
        match add_ocaml_constraint op version with
        | Some constr -> constr::result
        | None -> result
      else
        result

    | OpamTypes.FAnd (l,r)
    | OpamTypes.FOr (l,r) ->
      (find_constraints l []) @ (find_constraints r []) @ result

    | OpamTypes.FIdent (_, var, _)
      when OpamVariable.to_string var == "preinstalled" ->
      "=system"::result

    | OpamTypes.FOp _
    | OpamTypes.FBool _
    | OpamTypes.FString _
    | OpamTypes.FIdent _
    | OpamTypes.FNot _
    | OpamTypes.FDefined _
    | OpamTypes.FUndef _ -> result
  in
  match find_constraints filter [] with
  | [] -> None
  | constraints -> Some (String.concat " " constraints)

let render_opam ?installed_packages opam_name opam_version opam =
  let version = Version.opam_to_npm opam_version in

  let env ?scopeNames (var: OpamVariable.Full.t) =
    let variable = OpamVariable.Full.variable var in
    let scope = OpamVariable.Full.scope var in
    let name = OpamVariable.to_string variable in
    (* Few helpers for common constructs *)
    let
      t = Some (OpamVariable.bool true) and
      f = Some (OpamVariable.bool false) and
      s value = Some (OpamVariable.string value)
    in

    let genInstalledCond scopeNames =
      scopeNames
      |> List.map (fun name ->
          match name with
          | Some name ->
            let name = OpamPackage.Name.to_string name in
            let npmname = to_npm_name name in
            {j|$npmname.installed|j}
          | None -> "self.installed")
      |> String.concat " && "
    in

    match scopeNames, name with
    | Some scopeNames, "installed" ->
      let cond = genInstalledCond scopeNames in
      s {j|#{ $cond ? 'true' : 'false'}|j}
    | Some scopeNames, "enable" ->
      let cond = genInstalledCond scopeNames in
      s {j|#{ $cond ? 'enable' : 'disable'}|j}

    | Some _, _ ->
      None

    | None, _ -> begin
      match (scope, name) with

        | (OpamVariable.Full.Package name, var) ->
          let envname = to_env_name (OpamPackage.Name.to_string name) in
          let npmname = to_npm_name (OpamPackage.Name.to_string name) in
          begin match var with
            | "installed" ->
              s {j|#{ $npmname.installed ? 'true' : 'false'}|j}
            | "enable" ->
              s {j|#{ $npmname.installed ? 'enable' : 'disable'}|j}
            | "version" ->
              s ("${" ^ envname ^ "_version}")
            | "bin" ->
              s ("#{" ^ npmname ^ ".bin}")
            | "share" ->
              s ("#{" ^ npmname ^ ".share}")
            | "lib" ->
              s ("#{" ^ npmname ^ ".lib}")
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
      end
  in

  let ocaml_version_constaint = render_opam_available (OpamFile.OPAM.available opam) in

  (** Env to eval filters for dep contraints *)
  let make_depends_env ~build ~test (var: OpamVariable.Full.t) =
    let variable = OpamVariable.Full.variable var in
    let scope = OpamVariable.Full.scope var in
    let name = OpamVariable.to_string variable in
    match (scope, name) with
      | (OpamVariable.Full.Global, "build") -> Some (OpamVariable.bool build)
      | (OpamVariable.Full.Global, "test") -> Some (OpamVariable.bool test)
      | (_, _name) -> None
  in

  let reg_depends_env = make_depends_env ~build:true ~test:false in
  let test_depends_env = make_depends_env ~build:false ~test:true in

  let depends = OpamFile.OPAM.depends opam in
  let depopts = OpamFile.OPAM.depopts opam in

  let dependencies =
    render_opam_depends ~installed_packages reg_depends_env depends
  in
  let optional_dependencies =
    render_opam_depends ~installed_packages reg_depends_env depopts
  in
  let dev_dependencies =
    render_opam_depends ~installed_packages test_depends_env depends
    |> List.filter (fun item -> not (List.mem item dependencies))
  in
  let patches =
    List.map
      (fun (basename, _) -> OpamFilename.Base.to_string basename)
      (OpamFile.OPAM.patches opam)
  in
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
    dev_dependencies;
    ocaml_version_constaint;
    substs;
    patches;
    build;
    install;
    exported_env;
  }
