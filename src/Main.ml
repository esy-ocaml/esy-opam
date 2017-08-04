module OPAM = OpamFile.OPAM

module StringSet = Set.Make(String)

(** This is the place we manually override some of the conversion results *)
let fixup pkg =
  let open EsyOpamRenderer in

  let cleanup = ["sh"; "-c"; "(make clean || true)"] in
  let opam_install = ["sh"; "-c"; "(opam-installer --prefix=$cur__install || true)"] in

  let subst_dependency =
    ("substs", "esy-ocaml/substs")
  in
  let opam_installer_dependency =
    ("opam-installer-bin", "esy-ocaml/opam-installer-bin")
  in
  let ocamlfind_dependency =
    ("@opam-alpha/ocamlfind", "esy-ocaml/ocamlfind#esy/1.7.1-esy3")
  in
  let camlp4_dependency =
    ("@opam-alpha/camlp4", "esy-ocaml/camlp4#esy/4.02+7-TEST")
  in

  let fixup_dependencies pkg =
    let exclude_dependencies = StringSet.of_list [
        "@opam-alpha/base-no-ppx";
      ]
    in
    let exclude_optional_dependencies = StringSet.of_list [
        "@opam-alpha/conf-libev";
        "@opam-alpha/lablgtk";
        "@opam-alpha/ssl";
        "@opam-alpha/mirage-xen";
        "@opam-alpha/mirage-xen-ocaml";
        "@opam-alpha/tyxml";
        "@opam-alpha/reactiveData";
        "@opam-alpha/deriving";
        "@opam-alpha/ocamlbuild";
        "@opam-alpha/js_of_ocaml";
      ]
    in
    let is_not_excluded_with excluded (name, _) =
      not (StringSet.mem name excluded)
    in
    let fixup = fun (name, constr) ->
      match name with
      | "@opam-alpha/ocamlfind" -> ocamlfind_dependency
      | "@opam-alpha/camlp4" -> camlp4_dependency
      | _ -> (name, constr)
    in
    let optional_dependencies = 
      pkg.optional_dependencies
      |> List.map fixup
      |> List.filter (is_not_excluded_with exclude_optional_dependencies)
    in
    let dependencies =
      subst_dependency::opam_installer_dependency::pkg.dependencies
      |> List.map fixup
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

  let export_caml_ld_library_path name stublibs pkg =
    let pkg_name = EsyOpamRenderer.to_env_name pkg.name in
    let dir = match stublibs with
      | `NoStublibs -> name
      | `Stublibs -> "stublibs"
    in
    let caml_ld_library_path = (
      "CAML_LD_LIBRARY_PATH",
      "$" ^ pkg_name ^ "__lib/" ^ dir ^ ":$CAML_LD_LIBRARY_PATH"
    ) in
    { pkg with
      exported_env = caml_ld_library_path::pkg.exported_env }
  in

  let exclude_dependencies excluded pkg =
    let excluded = StringSet.of_list excluded in
    let is_not_excluded (name, _) = not (StringSet.mem name excluded) in 
    { pkg with
      dependencies = List.filter is_not_excluded pkg.dependencies;
      optional_dependencies = List.filter is_not_excluded pkg.optional_dependencies;
    }
  in

  let add_dependencies added pkg =
    let dependencies = List.concat [pkg.dependencies; added] in
    { pkg with dependencies = dependencies }
  in

  let fixup_overrides pkg =
    match pkg.name with
    | "@opam-alpha/conf-gmp" ->
      { pkg with
        build = [
          ["cc"; "-c"; "$CFLAGS"; "-I/usr/local/include test.c"]
        ];
        install = [];
      }
    | "@opam-alpha/typerex-build" ->
      { pkg with
        build = [
          ["./configure"; "--prefix"; "$cur__install"];
          ["make"];
          ["make"; "install"];
          opam_install;
        ];
        install = [];
      }
    | "@opam-alpha/ocamlbuild" ->
      { pkg with
        build = [["true"]];
        install = [];
      }
    | "@opam-alpha/merlin" ->
      let merlin_vim_rtp = (
        "opam_alpha__slash__merlin__vim_rtp",
        "$opam_alpha__slash__merlin__install/share/merlin/vim"
      ) in
      { pkg with
        exported_env = merlin_vim_rtp::pkg.exported_env
      }
    | "@opam-alpha/cppo" ->
      { pkg with
        build =  [
          cleanup;
          ["make"; "all"];
          ["make"; "opt"];
          ["make"; "ocamlbuild"];
          ["make"; "LIBDIR=$cur__lib"; "install-lib"];
          ["make"; "BINDIR=$cur__bin"; "install-bin"];
          opam_install;
        ];
        install = [];
      }
    | "@opam-alpha/zarith" -> export_caml_ld_library_path "zarith" `NoStublibs pkg
    | "@opam-alpha/cstruct" -> export_caml_ld_library_path "cstruct" `NoStublibs pkg
    | "@opam-alpha/launchd" -> export_caml_ld_library_path "launchd" `NoStublibs pkg
    | "@opam-alpha/lwt" -> export_caml_ld_library_path "lwt" `NoStublibs pkg
    | "@opam-alpha/lambda-term" -> export_caml_ld_library_path "lambda-term" `NoStublibs pkg
    | "@opam-alpha/bin_prot" -> export_caml_ld_library_path "bin_prot" `Stublibs pkg
    | "@opam-alpha/core_kernel" -> export_caml_ld_library_path "core_kernel" `Stublibs pkg
    | "@opam-alpha/core" -> export_caml_ld_library_path "core" `Stublibs pkg
    | "@opam-alpha/async_extra" -> export_caml_ld_library_path "async_extra" `Stublibs pkg
    | "@opam-alpha/async_ssl" -> export_caml_ld_library_path "async_ssl" `Stublibs pkg
    | "@opam-alpha/jenga" -> export_caml_ld_library_path "jenga" `Stublibs pkg
    | "@opam-alpha/re2" -> export_caml_ld_library_path "re2" `Stublibs pkg
    | "@opam-alpha/ppx_expect" -> export_caml_ld_library_path "ppx_expect" `Stublibs pkg
    | "@opam-alpha/ocaml_plugin" -> export_caml_ld_library_path "ocaml_plugin" `Stublibs pkg
    | "@opam-alpha/async_unix" -> export_caml_ld_library_path "async_unix" `Stublibs pkg
    | "@opam-alpha/inotify" -> export_caml_ld_library_path "inotify" `NoStublibs pkg
    | "@opam-alpha/io-page" -> export_caml_ld_library_path "io-page" `NoStublibs pkg
    | "@opam-alpha/pcre" -> export_caml_ld_library_path "pcre" `NoStublibs pkg

    | "@opam-alpha/cohttp" -> exclude_dependencies ["@opam-alpha/mirage-net"] pkg
    | "@opam-alpha/conduit" -> exclude_dependencies ["@opam-alpha/mirage-dns"] pkg
    | "@opam-alpha/ocamlgraph" -> exclude_dependencies ["@opam-alpha/conf-gnomecanvas"] pkg
    | "@opam-alpha/utop" -> exclude_dependencies ["@opam-alpha/camlp4"] pkg

    | "@opam-alpha/camomile" ->
      pkg
      |> add_dependencies [camlp4_dependency]

    | "@opam-alpha/conf-pkg-config" ->
      pkg
      |> add_dependencies [("yarn-pkg-config", "reasonml/yarn-pkg-config#esy")]

    | "@opam-alpha/ctypes" ->
      pkg
      |> export_caml_ld_library_path "ctypes" `NoStublibs
      |> add_dependencies [("libffi", "reasonml/libffi#esy")]

    | "@opam-alpha/vchan" ->
      pkg
      |> exclude_dependencies ["@opam-alpha/xen-evtchn"; "@opam-alpha/xen-gnt"]
      |> export_caml_ld_library_path "vchan" `NoStublibs

    | "@opam-alpha/nocrypto" ->
      pkg
      |> exclude_dependencies ["@opam-alpha/mirage-xen";
                               "@opam-alpha/mirage-entropy-xen";
                               "@opam-alpha/zarith-xen"]
      |> export_caml_ld_library_path "nocrypto" `NoStublibs

    | "@opam-alpha/mtime" ->
      pkg
      |> exclude_dependencies ["@opam-alpha/js_of_ocaml"]
      |> export_caml_ld_library_path "mtime" `Stublibs

    | _ -> pkg
  in

  pkg
  |> fixup_dependencies
  |> fixup_build
  |> fixup_install
  |> fixup_overrides


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

  [%bs.obj {
    name = pkg.name;
    version = pkg.version;
    dependencies = to_npm_dependencies (pkg.dependencies @
                                        pkg.optional_dependencies);
    peerDependencies = to_npm_dependencies [("ocaml", "*")];
    esy = {
      build = build;
      buildsInSource = Js.Boolean.to_js_boolean true;
      exportedEnv = exportedEnv;
    }
  }]
