module Re = struct
  let make_global v =
    Js.Re.fromStringWithFlags ~flags:"g" v

  let find_underscore_re =
    make_global "(_+)"

  let find_non_numbers_re =
    make_global "[^0-9]"

  let find_non_version_re =
    make_global "[^0-9\.]"

  let find_non_tag_re =
    make_global "[^0-9a-zA-Z\-]+"

  let find_numbers_re =
    make_global "^[0-9]+$"

  let find_leading_zeroes_re =
    make_global "^0+"

  let find_at_re= make_global "@"
  let find_dash_re = make_global "\-"
  let find_slash_re = make_global "\/"
  let find_dot_re = make_global "\."
end

module StringMap = struct
  include  Map.Make(String)

  let find_opt k m =
    try
      Some (find k m)
    with
      Not_found -> None
end

module StringSet = Set.Make(String)
