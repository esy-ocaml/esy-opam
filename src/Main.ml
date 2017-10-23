module OPAM = OpamFile.OPAM

module StringSet = Set.Make(String)

(** This is the place we manually override some of the conversion results *)
let fixup pkg =
  let open EsyOpamRenderer in

  let opam_install = ["sh"; "-c"; "(opam-installer --prefix=$cur__install || true)"] in

  let subst_dependency =
    ("substs", "esy-ocaml/substs")
  in
  let opam_installer_dependency =
    ("opam-installer-bin", "esy-ocaml/opam-installer-bin")
  in

  let fixup_dependencies pkg =
    let exclude_dependencies = StringSet.of_list [
        "@opam/base-no-ppx";
      ]
    in
    let exclude_optional_dependencies = StringSet.of_list [
        "@opam/conf-libev";
        "@opam/lablgtk";
        "@opam/ssl";
        "@opam/mirage-xen";
        "@opam/mirage-xen-ocaml";
        "@opam/tyxml";
        "@opam/reactiveData";
        "@opam/deriving";
        "@opam/js_of_ocaml";
      ]
    in
    let is_not_excluded_with excluded (name, _) =
      not (StringSet.mem name excluded)
    in
    let optional_dependencies = 
      pkg.optional_dependencies
      |> List.filter (is_not_excluded_with exclude_optional_dependencies)
    in
    let dependencies =
      subst_dependency::opam_installer_dependency::pkg.dependencies
      |> List.filter (is_not_excluded_with exclude_dependencies)
    in
    { pkg with dependencies; optional_dependencies; }
  in

  let fixup_build pkg =
    match pkg.substs with
    | [] -> pkg
    | substs ->
      let substs = List.map (fun item -> ["substs"; (item ^ ".in")]) substs in
      { pkg with build = substs @ pkg.build }
  in

  let fixup_install pkg =
    { pkg with
      install = List.concat [pkg.install; [opam_install]] }
  in

  pkg
  |> fixup_dependencies
  |> fixup_build
  |> fixup_install


let parse_opam data =
  OPAM.read_from_string data

let parse_opam_url data =
  OpamFile.URL.read_from_string data

let render_opam_url (opam_url : OpamFile.URL.t) =
  let { OpamUrl. transport; path; hash; _ } = OpamFile.URL.url opam_url in
  let url = match hash with
    | None -> (transport ^ "://" ^ path)
    | Some hash -> (transport ^ "://" ^ path ^ "#" ^ hash)
  in
  let checksum = OpamFile.URL.checksum opam_url in
  let checksum = List.map (fun hash ->
      let kind = OpamHash.kind hash in
      let kind = match kind with
        | `MD5 -> "md5"
        | `SHA512 -> "sha512"
        | `SHA256 -> "sha256"
      in
      let contents = OpamHash.contents hash in
      [%bs.obj {
        kind;
        contents
      }]
    ) checksum in
  [%bs.obj {
    url;
    checksum = Array.of_list checksum
  }]

let render_opam opam_name opam_version opam =
  let pkg = EsyOpamRenderer.render_opam opam_name opam_version opam in
  let pkg = fixup pkg in

  let to_npm_dependencies deps =
    let dependencies = Js.Dict.empty () in
    List.iter (fun (name, constr) ->
        Js.Dict.set dependencies name constr
      ) deps;
    dependencies
  in

  let exportedEnv =
    let exported_env = List.map
        (fun (k, v) -> (k, [%bs.obj {_val = v; scope = "global"}]))
        pkg.exported_env
    in
    Js.Dict.fromList exported_env
  in

  let build =
    let build = Array.of_list (List.map Array.of_list pkg.build) in
    let install = Array.of_list (List.map Array.of_list pkg.install) in
    Array.concat [build; install]
  in

  let dependencies = to_npm_dependencies (pkg.dependencies @
                                          pkg.optional_dependencies) in

  let peerDependencies = match pkg.ocaml_version_constaint with
    | None -> to_npm_dependencies [("ocaml", "*")]
    | Some constr -> to_npm_dependencies [("ocaml", constr)]
  in

  [%bs.obj {
    name = pkg.name;
    version = pkg.version;
    dependencies;
    peerDependencies;
    esy = {
      build;
      exportedEnv;
      buildsInSource = Js.Boolean.to_js_boolean true;
    };
    _esy_opam_patches = Array.of_list pkg.patches;
  }]

let version_compare a b =
  OpamVersionCompare.compare a b
