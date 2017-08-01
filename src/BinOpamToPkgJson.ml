let () =
  let args = Node_process.argv in
  let args = Js.Array.sliceFrom 2 args in
  if Array.length args < 3 then begin
    Js.log "error: provide NAME VERSION OPAM_FILENAME as arguments";
    Node_process.exit 1
  end;
  let name = Array.get args 0 in
  let version = Array.get args 1 in
  let filename = Array.get args 2 in
  let data = Node_fs.readFileSync filename `utf8 in
  let opam = Main.parse_opam data in
  let pkgJson = Main.render_opam_to_js name version opam in
  Js.log pkgJson
