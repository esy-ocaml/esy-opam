module Lib = EsyOpamLib

(**
 * major.minor.patch-tag
 *
 * We don't represent +build as npm ignore it during version comparison so we
 * don't know how useful it is (anyway we order versions by OPAM versions when
 * solving).
 *)
type t = (string * string * string * string option)

let of_opam version =

  let normalize_tag tag =
    tag
    |> Js.String.replaceByRe Lib.Re.find_non_tag_re ""
  in

  let normalize_version_segment version =
    let version =
      version
      |> Js.String.replaceByRe Lib.Re.find_leading_zeroes_re ""
    in
    match version with
    | "" -> "0"
    | _ ->version
  in

  let normalize_version ~has_v_prefix version =
    let parts = Array.to_list (Js.String.splitAtMost ~limit:3 "." version) in

    (* In case version is prefixed with 'v' we bump it's major part with 10000:
     *
     * - OPAM compares versions using alphanumeric order so, for example:
     *     v0.9.1 > 113.30.30
     *
     * - Why 100000000? The longest major version I can think of it XXXX like
     *   20171212 (menhir) and we could handle if menhir starts to prefix it
     *   with 'v': v20191212
     *
     *)
    let normalize_major_version_segment segment =
      let segment = normalize_version_segment segment in
      if has_v_prefix then
        let v = int_of_string segment in
        string_of_int (100000000 + v)
      else
        segment
    in

    match parts with
    | major::[] ->
      let major = normalize_major_version_segment major in
      (major, "0", "0")

    | major::minor::[] ->
      let major = normalize_major_version_segment major in
      let minor = normalize_version_segment minor in
      (major, minor, "0")

    | major::minor::patch::[] ->
      let major = normalize_major_version_segment major in
      let minor = normalize_version_segment minor in
      let patch = normalize_version_segment patch in
      (major, minor, patch)
    | _ ->
      failwith "impossible"
  in

  let converted =
    (* Important to recreate regexes as they are stateful *)
    let find_non_numeric_re = Lib.Re.make_global "[^0-9\.]" in
    let has_v_prefix_re = Lib.Re.make_global "^v[0-9]" in

    (**
     * Strip 'v' prefix here, we'll use that info during normalization.
     *)
    let (has_v_prefix, version) = match Js.Re.exec version has_v_prefix_re with
      | Some _ ->
        let version = Js.String.substringToEnd ~from:1 version in
        (true, version)
      | None ->
        (false, version)
    in

    (**
     * We find the first non-numeric char at start treating the parts after this
     * as "tag", the part before this — as version.
     *)
    match Js.Re.exec version find_non_numeric_re with
    | Some m ->
      let idx = Js.Re.index m in
      let tag = Js.String.substringToEnd ~from:idx version in
      let version = Js.String.substring ~from:0 ~to_:idx version in
      let (major, minor, patch) = normalize_version ~has_v_prefix version in
      let tag = normalize_tag tag in
      (major, minor, patch, Some tag)
    | None ->
      let (major, minor, patch) = normalize_version ~has_v_prefix version in
      (major, minor, patch, None)

  in

  converted

let render (version : t) =
  match version with
  | (major, minor, patch, Some tag) ->
    major ^ "." ^ minor ^ "." ^ patch ^ "-" ^ tag
  | (major, minor, patch, None) ->
    major ^ "." ^ minor ^ "." ^ patch

let opam_to_npm version =
  render (of_opam version)
