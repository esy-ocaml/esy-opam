module OPAM = OpamFile.OPAM

let parse_opam_file filename =
  let contents = Node_fs.readFileSync filename `utf8 in
  OPAM.read_from_string contents

let () =
  let opam_filename_in = Array.get Sys.argv 2 in
  let opam = parse_opam_file opam_filename_in in
  let res = EsyOpamRenderer.render_opam "package" opam in
  Js.log (Array.of_list res.dependencies);
  Js.log (Array.of_list res.build);
