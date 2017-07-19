module OPAM = OpamFile.OPAM

let parse_opam data =
  OPAM.read_from_string data

let render_opam package_name opam =
  EsyOpamRenderer.render_opam package_name opam

let render_opam_to_js package_name opam =
  let res = render_opam package_name opam in
  [%bs.obj {
    name = package_name;
    dependencies = Array.of_list res.dependencies;
    build = Array.of_list res.build;
  }]
