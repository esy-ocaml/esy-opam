module OPAM = OpamFile.OPAM

module StringSet = Set.Make(String)

(** This is the place we manually override some of the conversion results *)
let fixup pkg =
  let open EsyOpamRenderer in

  let cleanup =
    "sh -c '(make clean || true)'"
  in
  let opam_install =
    "sh -c '(opam-installer --prefix=$cur__install || true)'"
  in

  let export_caml_ld_library_path stublibs pkg =
    let name = EsyOpamRenderer.envnorm_package_name pkg.name in
    let dir = match stublibs with
      | `NoStublibs -> name
      | `Stublibs -> "stublibs"
    in
    let caml_ld_library_path = (
      "CAML_LD_LIBRARY_PATH",
      "$" ^ name ^ "__lib/" ^ dir ^ ":$CAML_LD_LIBRARY_PATH"
    ) in
    { pkg with exported_env = caml_ld_library_path::pkg.exported_env }
  in

  let exclude_dependencies excluded pkg =
    let excluded = StringSet.of_list excluded in
    let dependencies =
      List.filter
        (fun (name, _) -> StringSet.mem name excluded)
        pkg.dependencies
    in
    { pkg with dependencies = dependencies }
  in

  match pkg.name with
  | "conf-gmp" ->
    { pkg with
      build = [
        "cc -c $CFLAGS -I/usr/local/include test.c"
      ];
    }
  | "typerex-build" ->
    { pkg with
      build = [
        "./configure --prefix $cur__install";
        "make";
        "make install";
        "(opam-installer --prefix=$cur__install || true)";
      ];
    }
  | "ocamlbuild" ->
    { pkg with
      build = ["true"];
    }
  | "merlin" ->
    { pkg with
      exported_env = ("opam_alpha__slash__merlin__vim_rtp",
                      "$opam_alpha__slash__merlin__install/share/merlin/vim")::pkg.exported_env
    }
  | "cppo" ->
    { pkg with
      build =  [
        cleanup;
        "make all";
        "make opt";
        "make ocamlbuild";
        "make LIBDIR=$cur__lib install-lib";
        "make BINDIR=$cur__bin install-bin";
        opam_install;
      ];
    }
  | "ctypes" -> export_caml_ld_library_path `NoStublibs pkg
  | "zarith" -> export_caml_ld_library_path `NoStublibs pkg
  | "cstruct" -> export_caml_ld_library_path `NoStublibs pkg
  | "launchd" -> export_caml_ld_library_path `NoStublibs pkg
  | "lwt" -> export_caml_ld_library_path `NoStublibs pkg
  | "lambda-term" -> export_caml_ld_library_path `NoStublibs pkg
  | "bin_prot" -> export_caml_ld_library_path `Stublibs pkg
  | "core_kernel" -> export_caml_ld_library_path `Stublibs pkg
  | "core" -> export_caml_ld_library_path `Stublibs pkg
  | "async_extra" -> export_caml_ld_library_path `Stublibs pkg
  | "async_ssl" -> export_caml_ld_library_path `Stublibs pkg
  | "jenga" -> export_caml_ld_library_path `Stublibs pkg
  | "re2" -> export_caml_ld_library_path `Stublibs pkg
  | "ppx_expect" -> export_caml_ld_library_path `Stublibs pkg
  | "ocaml_plugin" -> export_caml_ld_library_path `Stublibs pkg
  | "async_unix" -> export_caml_ld_library_path `Stublibs pkg
  | "inotify" -> export_caml_ld_library_path `NoStublibs pkg
  | "io-page" -> export_caml_ld_library_path `NoStublibs pkg
  | "pcre" -> export_caml_ld_library_path `NoStublibs pkg

  | "cohttp" -> exclude_dependencies ["mirage-net"] pkg
  | "conduit" -> exclude_dependencies ["mirage-dns"] pkg
  | "ocamlgraph" -> exclude_dependencies ["conf-gnomecanvas"] pkg
  | "utop" -> exclude_dependencies ["camlp4"] pkg

  | "vchan" ->
    pkg
    |> exclude_dependencies ["xen-evtchn"; "xen-gnt"]
    |> export_caml_ld_library_path `NoStublibs

  | "nocrypto" ->
    pkg
    |> exclude_dependencies ["mirage-xen"; "mirage-entropy-xen"; "zarith-xen"]
    |> export_caml_ld_library_path `NoStublibs

  | "mtime" ->
    pkg
    |> exclude_dependencies ["js_of_ocaml"]
    |> export_caml_ld_library_path `Stublibs

  | _ -> pkg

let parse_opam data =
  OPAM.read_from_string data

let render_opam package_name package_version opam =
  EsyOpamRenderer.render_opam package_name package_version opam

let render_opam_to_js package_name package_version opam =
  let opam_scoped name = "@opam-alpha/" ^ name in
  let pkg = render_opam package_name package_version opam in
  let pkg = fixup pkg in

  let dependencies = Js.Dict.empty () in
  List.iter (fun (name, constr) ->
      Js.Dict.set dependencies (opam_scoped name) constr
    ) pkg.dependencies;

  let exportedEnv =
    let exported_env = List.map
        (fun (k, v) -> (k, [%bs.obj {_val = v; scope = "global"}]))
        pkg.exported_env
    in
    Js.Dict.fromList exported_env
  in

  [%bs.obj {
    name = opam_scoped pkg.name;
    version = pkg.version;
    dependencies = dependencies;
    esy = {
      build = Array.of_list pkg.build;
      exportedEnv = exportedEnv;
    }
  }]
