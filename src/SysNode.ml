class type _node_fs_stat = object
  method isDirectory : unit -> bool
end [@bs]

external node_fs_statSync : string -> _node_fs_stat Js.t = "statSync"  [@@bs.module "fs"]
external node_fs_existsSync: string -> bool = "existsSync"  [@@bs.module "fs"]

let is_directory filename =
  let stat = node_fs_statSync filename in
  stat##isDirectory ()

let file_exists filename =
  node_fs_existsSync filename
