type t

external process : t = "" [@@bs.val "process"]

external cwd : unit -> string = "chdir" [@@bs.module "process"]
external kill : int -> int -> unit = "kill" [@@bs.module "process"]
external chdir : string -> unit = "chdir" [@@bs.module "process"]
external getuid: unit -> int = "getuid" [@@bs.module "process"]
external getgroups: unit -> int array = "getgroups" [@@bs.module "process"]
external pid : int = "" [@@bs.val "process.pid"]

module Env = struct
  type t
  external env : t = "" [@@bs.val "process.env"]
  external get_process_env : t -> string -> string Js.Null_undefined.t = "" [@@bs.get_index]
end

let env name =
  Env.get_process_env Env.env name |> Js.Null_undefined.to_opt
