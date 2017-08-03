module OPAM = OpamFile.OPAM

module StringSet = Set.Make(String)

(** This is the place we manually override some of the conversion results *)
let fixup pkg =
  let open EsyOpamRenderer in

  let cleanup = ["sh"; "-c"; "(make clean || true)"] in
  let opam_install = ["sh"; "-c"; "(opam-installer --prefix=$cur__install || true)"] in

  let fixup_deps pkg =
    let dependencies =
      List.map
        (fun (name, constr) ->
           match name with
           | "@opam-alpha/ocamlfind" ->
             (name, "esy-ocaml/ocamlfind#esy/1.7.1-esy3")
           | "@opam-alpha/camlp4" ->
             (name, "esy-ocaml/camlp4#esy/4.02+7-TEST")
           | _ -> (name, constr))
        pkg.dependencies
    in
    { pkg with dependencies = dependencies }
  in

  let fixup_install pkg =
    { pkg with
      install = List.concat [pkg.install; [opam_install]] }
  in

  let export_caml_ld_library_path stublibs pkg =
    let name = EsyOpamRenderer.to_env_name pkg.name in
    let dir = match stublibs with
      | `NoStublibs -> name
      | `Stublibs -> "stublibs"
    in
    let caml_ld_library_path = (
      "CAML_LD_LIBRARY_PATH",
      "$" ^ name ^ "__lib/" ^ dir ^ ":$CAML_LD_LIBRARY_PATH"
    ) in
    { pkg with
      exported_env = caml_ld_library_path::pkg.exported_env }
  in

  let exclude_dependencies excluded pkg =
    let excluded = StringSet.of_list excluded in
    let dependencies =
      List.filter
        (fun (name, _) -> not (StringSet.mem name excluded))
        pkg.dependencies
    in
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
    | "@opam-alpha/ctypes" -> export_caml_ld_library_path `NoStublibs pkg
    | "@opam-alpha/zarith" -> export_caml_ld_library_path `NoStublibs pkg
    | "@opam-alpha/cstruct" -> export_caml_ld_library_path `NoStublibs pkg
    | "@opam-alpha/launchd" -> export_caml_ld_library_path `NoStublibs pkg
    | "@opam-alpha/lwt" -> export_caml_ld_library_path `NoStublibs pkg
    | "@opam-alpha/lambda-term" -> export_caml_ld_library_path `NoStublibs pkg
    | "@opam-alpha/bin_prot" -> export_caml_ld_library_path `Stublibs pkg
    | "@opam-alpha/core_kernel" -> export_caml_ld_library_path `Stublibs pkg
    | "@opam-alpha/core" -> export_caml_ld_library_path `Stublibs pkg
    | "@opam-alpha/async_extra" -> export_caml_ld_library_path `Stublibs pkg
    | "@opam-alpha/async_ssl" -> export_caml_ld_library_path `Stublibs pkg
    | "@opam-alpha/jenga" -> export_caml_ld_library_path `Stublibs pkg
    | "@opam-alpha/re2" -> export_caml_ld_library_path `Stublibs pkg
    | "@opam-alpha/ppx_expect" -> export_caml_ld_library_path `Stublibs pkg
    | "@opam-alpha/ocaml_plugin" -> export_caml_ld_library_path `Stublibs pkg
    | "@opam-alpha/async_unix" -> export_caml_ld_library_path `Stublibs pkg
    | "@opam-alpha/inotify" -> export_caml_ld_library_path `NoStublibs pkg
    | "@opam-alpha/io-page" -> export_caml_ld_library_path `NoStublibs pkg
    | "@opam-alpha/pcre" -> export_caml_ld_library_path `NoStublibs pkg

    | "@opam-alpha/cohttp" -> exclude_dependencies ["mirage-net"] pkg
    | "@opam-alpha/conduit" -> exclude_dependencies ["mirage-dns"] pkg
    | "@opam-alpha/ocamlgraph" -> exclude_dependencies ["conf-gnomecanvas"] pkg
    | "@opam-alpha/utop" -> exclude_dependencies ["camlp4"] pkg

    | "@opam-alpha/vchan" ->
      pkg
      |> exclude_dependencies ["xen-evtchn"; "xen-gnt"]
      |> export_caml_ld_library_path `NoStublibs

    | "@opam-alpha/nocrypto" ->
      pkg
      |> exclude_dependencies ["mirage-xen"; "mirage-entropy-xen"; "zarith-xen"]
      |> export_caml_ld_library_path `NoStublibs

    | "@opam-alpha/mtime" ->
      pkg
      |> exclude_dependencies ["js_of_ocaml"]
      |> export_caml_ld_library_path `Stublibs

    | _ -> pkg
  in

  pkg
  |> fixup_deps
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
    dependencies = to_npm_dependencies pkg.dependencies;
    peerDependencies = to_npm_dependencies [("ocaml", "*")];
    esy = {
      build = build;
      buildsInSource = Js.Boolean.to_js_boolean true;
      exportedEnv = exportedEnv;
    }
  }]
