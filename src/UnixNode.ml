type process_status =
  |	WEXITED of int	(*	The process terminated normally by exit; the argument is the return code.  *)
  |	WSIGNALED of int	(*	The process was killed by a signal; the argument is the signal number.  *)
  |	WSTOPPED of int	(*	The process was stopped by a signal; the argument is the signal number.  *)

type wait_flag = 
  |	WNOHANG	(*	Do not block if no child has died yet, but immediately return with a pid equal to 0.  *)
  |	WUNTRACED	(*	Report also the children that receive stop signals.  *)

type file_kind =
  |	S_REG	(*	Regular file *)
  |	S_DIR	(*	Directory *)
  |	S_CHR	(*	Character device *)
  |	S_BLK	(*	Block device *)
  |	S_LNK	(*	Symbolic link *)
  |	S_FIFO	(*	Named pipe *)
  |	S_SOCK	(*	Socket *)

type file_perm = int

type stats = {
  st_dev : int;	(*	Device number *)
  st_ino : int;	(*	Inode number *)
  st_kind : file_kind;	(*	Kind of the file *)
  st_perm : file_perm;	(*	Access rights *)
  st_nlink : int;	(*	Number of links *)
  st_uid : int;	(*	User id of the owner *)
  st_gid : int;	(*	Group ID of the file's group *)
  st_rdev : int;	(*	Device minor number *)
  st_size : int;	(*	Size in bytes *)
  st_atime : float;	(*	Last access time *)
  st_mtime : float;	(*	Last modification time *)
  st_ctime : float;	(*	Last status change time *)
}

type tm = {
  tm_sec : int;	(*	Seconds 0..60 *)
  tm_min : int;	(*	Minutes 0..59 *)
  tm_hour : int;	(*	Hours 0..23 *)
  tm_mday : int;	(*	Day of month 1..31 *)
  tm_mon : int;	(*	Month of year 0..11 *)
  tm_year : int;	(*	Year - 1900 *)
  tm_wday : int;	(*	Day of week (Sunday is 0) *)
  tm_yday : int;	(*	Day of year 0..365 *)
  tm_isdst : bool;	(*	Daylight time savings in effect *)
}

type open_flag = 
  |	O_RDONLY	(*	Open for reading *)
  |	O_WRONLY	(*	Open for writing *)
  |	O_RDWR	(*	Open for reading and writing *)
  |	O_NONBLOCK	(*	Open in non-blocking mode *)
  |	O_APPEND	(*	Open for append *)
  |	O_CREAT	(*	Create if nonexistent *)
  |	O_TRUNC	(*	Truncate to 0 length if existing *)
  |	O_EXCL	(*	Fail if existing *)
  |	O_NOCTTY	(*	Don't make this dev a controlling tty *)
  |	O_DSYNC	(*	Writes complete as `Synchronised I/O data integrity completion' *)
  |	O_SYNC	(*	Writes complete as `Synchronised I/O file integrity completion' *)
  |	O_RSYNC	(*	Reads complete as writes (depending on O_SYNC/O_DSYNC) *)
  |	O_SHARE_DELETE	(*	Windows only: allow the file to be deleted while still open *)
  |	O_CLOEXEC	(*	Set the close-on-exec flag on the descriptor returned by Unix.openfile *)

type terminal_io = {
  mutable c_ignbrk : bool;	(*	Ignore the break condition.  *)
  mutable c_brkint : bool;	(*	Signal interrupt on break condition.  *)
  mutable c_ignpar : bool;	(*	Ignore characters with parity errors.  *)
  mutable c_parmrk : bool;	(*	Mark parity errors.  *)
  mutable c_inpck : bool;	(*	Enable parity check on input.  *)
  mutable c_istrip : bool;	(*	Strip 8th bit on input characters.  *)
  mutable c_inlcr : bool;	(*	Map NL to CR on input.  *)
  mutable c_igncr : bool;	(*	Ignore CR on input.  *)
  mutable c_icrnl : bool;	(*	Map CR to NL on input.  *)
  mutable c_ixon : bool;	(*	Recognize XON/XOFF characters on input.  *)
  mutable c_ixoff : bool;	(*	Emit XON/XOFF chars to control input flow.  *)
  mutable c_opost : bool;	(*	Enable output processing.  *)
  mutable c_obaud : int;	(*	Output baud rate (0 means close connection).  *)
  mutable c_ibaud : int;	(*	Input baud rate.  *)
  mutable c_csize : int;	(*	Number of bits per character (5-8).  *)
  mutable c_cstopb : int;	(*	Number of stop bits (1-2).  *)
  mutable c_cread : bool;	(*	Reception is enabled.  *)
  mutable c_parenb : bool;	(*	Enable parity generation and detection.  *)
  mutable c_parodd : bool;	(*	Specify odd parity instead of even.  *)
  mutable c_hupcl : bool;	(*	Hang up on last close.  *)
  mutable c_clocal : bool;	(*	Ignore modem status lines.  *)
  mutable c_isig : bool;	(*	Generate signal on INTR, QUIT, SUSP.  *)
  mutable c_icanon : bool;	(*	Enable canonical processing (line buffering and editing) *)
  mutable c_noflsh : bool;	(*	Disable flush after INTR, QUIT, SUSP.  *)
  mutable c_echo : bool;	(*	Echo input characters.  *)
  mutable c_echoe : bool;	(*	Echo ERASE (to erase previous character).  *)
  mutable c_echok : bool;	(*	Echo KILL (to erase the current line).  *)
  mutable c_echonl : bool;	(*	Echo NL even if c_echo is not set.  *)
  mutable c_vintr : char;	(*	Interrupt character (usually ctrl-C).  *)
  mutable c_vquit : char;	(*	Quit character (usually ctrl-\).  *)
  mutable c_verase : char;	(*	Erase character (usually DEL or ctrl-H).  *)
  mutable c_vkill : char;	(*	Kill line character (usually ctrl-U).  *)
  mutable c_veof : char;	(*	End-of-file character (usually ctrl-D).  *)
  mutable c_veol : char;	(*	Alternate end-of-line char. (usually none).  *)
  mutable c_vmin : int;	(*	Minimum number of characters to read before the read request is satisfied.  *)
  mutable c_vtime : int;	(*	Maximum read wait (in 0.1s units).  *)
  mutable c_vstart : char;	(*	Start character (usually ctrl-Q).  *)
  mutable c_vstop : char;	(*	Stop character (usually ctrl-S).  *)
}

(** Externals *)
class type _node_fs_stat = object
  method dev : int
  method ino: int
  method mode: int
  method nlink: int
  method uid: int
  method gid: int
  method rdev: int
  method size: int
  method blksize: int
  method blocks: int
  method atime: < now: unit -> float [@bs.meth] > Js.t
  method mtime: < now: unit -> float [@bs.meth] > Js.t
  method ctime: < now: unit -> float [@bs.meth] > Js.t
  method birthtime: < now: unit -> float > Js.t
  method isDirectory : unit -> bool
end [@bs]

type file_descr = int

external node_fs_openSync: string -> string Js.Array.t -> file_perm -> file_descr = "openSync"  [@@bs.module "fs"]
external node_fs_statSync : string -> _node_fs_stat Js.t = "statSync"  [@@bs.module "fs"]
external node_fs_lstatSync : string -> _node_fs_stat Js.t = "lstatSync"  [@@bs.module "fs"]
external node_fs_fstatSync : file_descr -> _node_fs_stat Js.t = "fstatSync"  [@@bs.module "fs"]
external node_fs_chmodSync : string -> file_perm -> unit = "chmodSync"  [@@bs.module "fs"]
external node_fs_mkdirSync : string -> file_perm -> unit = "mkdirSync"  [@@bs.module "fs"]
external node_fs_rmdirSync : string -> unit = "rmdirSync"  [@@bs.module "fs"]
external node_fs_unlinkSync : string -> unit = "unlinkSync"  [@@bs.module "fs"]
external node_fs_symlinkSync: string -> string -> string -> unit = "symlinkSync"  [@@bs.module "fs"]
external node_fs_readlinkSync: string -> string = "readlinkSync"  [@@bs.module "fs"]
external node_fs_closeSync: file_descr -> unit = "closeSync"  [@@bs.module "fs"]

let _NODE_O_RDONLY = "O_RDONLY"
let _NODE_O_WRONLY = "O_WRONLY"
let _NODE_O_RDWR = "O_RDWR"
let _NODE_O_CREAT = "O_CREAT"
let _NODE_O_EXCL = "O_EXCL"
let _NODE_O_NOCTTY = "O_NOCTTY"
let _NODE_O_TRUNC = "O_TRUNC"
let _NODE_O_APPEND = "O_APPEND"
let _NODE_O_DIRECTORY = "O_DIRECTORY"
let _NODE_O_NOATIME = "O_NOATIME"
let _NODE_O_NOFOLLOW = "O_NOFOLLOW"
let _NODE_O_SYNC = "O_SYNC"
let _NODE_O_SYMLINK = "O_SYMLINK"
let _NODE_O_DIRECT = "O_DIRECT"
let _NODE_O_NONBLOCK = "O_NONBLOCK"

let unix_open_flag_to_node_open_flag (flag: open_flag) =
  match flag with
  | O_RDONLY -> _NODE_O_RDONLY
  | O_WRONLY -> _NODE_O_WRONLY
  | O_RDWR -> _NODE_O_RDWR
  | O_NONBLOCK -> _NODE_O_NONBLOCK
  | O_APPEND -> _NODE_O_APPEND
  | O_CREAT -> _NODE_O_CREAT
  | O_TRUNC -> _NODE_O_TRUNC
  | O_EXCL -> _NODE_O_EXCL
  | O_NOCTTY -> _NODE_O_NOCTTY
  | O_DSYNC -> _NODE_O_SYNC
  | O_SYNC -> _NODE_O_SYNC
  | O_RSYNC -> _NODE_O_SYNC
  | O_SHARE_DELETE -> ""
  | O_CLOEXEC -> ""

external date_now : unit -> float = "date_now" [@@bs.val "Date.now"]
external node_process_cwd : unit -> string = "chdir" [@@bs.module "process"]
external node_process_chdir : string -> unit = "chdir" [@@bs.module "process"]
external node_process_getuid: unit -> int = "getuid" [@@bs.module "process"]
external node_process_getgroups: unit -> int array = "getgroups" [@@bs.module "process"]
external process_env_USER : string = "" [@@bs.val "process.env.USER"]
external process_pid : int = "" [@@bs.val "process.pid"]

let node_stats_to_unix_stats stat =
  {
    st_dev = stat##dev;
    st_ino = stat##ino;
    st_kind = if stat##isDirectory () then S_DIR else S_REG;
    st_perm = stat##mode;
    st_nlink = stat##nlink;
    st_uid = stat##uid;
    st_gid = stat##gid;
    st_rdev = stat##rdev;
    st_size = stat##size;
    st_atime = stat##atime##now ();
    st_mtime = stat##mtime##now ();
    st_ctime = stat##ctime##now ();
  }

let error_message _error = "unix error"

let pipe () = (3, 4)

let openfile filename flags mode =
  let node_flags = Array.of_list (List.map unix_open_flag_to_node_open_flag flags) in
  node_fs_openSync filename node_flags mode

let mkdir dirname mode =
  node_fs_mkdirSync dirname mode

let rmdir dirname =
  node_fs_rmdirSync dirname

let unlink filename =
  node_fs_unlinkSync filename

let getuid () =
  node_process_getuid ()

let symlink ?to_dir source dest =
  node_fs_symlinkSync source dest (match to_dir with
      | None -> if SysNode.is_directory source then "dir" else "file"
      | Some is_dir -> if is_dir then "dir" else "file")

let getgroups() =
  node_process_getgroups ()

let stat filename =
  let stat = node_fs_statSync filename in
  node_stats_to_unix_stats stat

let close fd =
  node_fs_closeSync fd

let lstat filename =
  let stat = node_fs_lstatSync filename in
  node_stats_to_unix_stats stat

let fstat fd =
  let stat = node_fs_fstatSync fd in
  node_stats_to_unix_stats stat

let getcwd () = node_process_cwd ()

let chmod filename mode =
  node_fs_chmodSync filename mode

let environment () = Array.make 0 ""

let read _file_descr _bytes _ofs _len  = 0

let readlink filename = node_fs_readlinkSync filename

let tcgetattr _file_descr = {
  c_ignbrk = true;	(*	Ignore the break condition.  *)
  c_brkint = true;	(*	Signal interrupt on break condition.  *)
  c_ignpar = true;	(*	Ignore characters with parity errors.  *)
  c_parmrk = true;	(*	Mark parity errors.  *)
  c_inpck = true;	(*	Enable parity check on input.  *)
  c_istrip = true;	(*	Strip 8th bit on input characters.  *)
  c_inlcr = true;	(*	Map NL to CR on input.  *)
  c_igncr = true;	(*	Ignore CR on input.  *)
  c_icrnl = true;	(*	Map CR to NL on input.  *)
  c_ixon = true;	(*	Recognize XON/XOFF characters on input.  *)
  c_ixoff = true;	(*	Emit XON/XOFF chars to control input flow.  *)
  c_opost = true;	(*	Enable output processing.  *)
  c_obaud = 0;	(*	Output baud rate (0 means close connection).  *)
  c_ibaud = 112000;	(*	Input baud rate.  *)
  c_csize = 8;	(*	Number of bits per character (5-8).  *)
  c_cstopb = 2;	(*	Number of stop bits (1-2).  *)
  c_cread = true;	(*	Reception is enabled.  *)
  c_parenb = true;	(*	Enable parity generation and detection.  *)
  c_parodd = true;	(*	Specify odd parity instead of even.  *)
  c_hupcl = true;	(*	Hang up on last close.  *)
  c_clocal = true;	(*	Ignore modem status lines.  *)
  c_isig = true;	(*	Generate signal on INTR, QUIT, SUSP.  *)
  c_icanon = true;	(*	Enable canonical processing (line buffering and editing) *)
  c_noflsh = true;	(*	Disable flush after INTR, QUIT, SUSP.  *)
  c_echo = true;	(*	Echo input characters.  *)
  c_echoe = true;	(*	Echo ERASE (to erase previous character).  *)
  c_echok = true;	(*	Echo KILL (to erase the current line).  *)
  c_echonl = true;	(*	Echo NL even if c_echo is not set.  *)
  c_vintr = 'c';	(*	Interrupt character (usually ctrl-C).  *)
  c_vquit = 'c';	(*	Quit character (usually ctrl-\).  *)
  c_verase = 'c';	(*	Erase character (usually DEL or ctrl-H).  *)
  c_vkill = 'c';	(*	Kill line character (usually ctrl-U).  *)
  c_veof = 'c';	(*	End-of-file character (usually ctrl-D).  *)
  c_veol = 'c';	(*	Alternate end-of-line char. (usually none).  *)
  c_vmin = 0;	(*	Minimum number of characters to read before the read request is satisfied.  *)
  c_vtime = 0;	(*	Maximum read wait (in 0.1s units).  *)
  c_vstart = 'c';	(*	Start character (usually ctrl-Q).  *)
  c_vstop = 'c';	(*	Stop character (usually ctrl-S).  *)
}

type setattr_when =
  |	TCSANOW
  |	TCSADRAIN
  |	TCSAFLUSH

let tcsetattr _file_descr _setattr_when _terminal_io = ()

type flush_queue =
  |	TCIFLUSH
  |	TCOFLUSH
  |	TCIOFLUSH

let tcflush _file_descr _flush_queue = ()

let gettimeofday () = date_now ()
let gmtime _f = {
  tm_sec = 0;
  tm_min = 0;
  tm_hour = 0;	(*	Hours 0..23 *)
  tm_mday = 1;	(*	Day of month 1..31 *)
  tm_mon = 0;	(*	Month of year 0..11 *)
  tm_year = 1900;	(*	Year - 1900 *)
  tm_wday = 0;	(*	Day of week (Sunday is 0) *)
  tm_yday = 0;	(*	Day of year 0..365 *)
  tm_isdst = true;	(*	Daylight time savings in effect *)
}

let chdir = node_process_chdir

let descr_of_in_channel _in_channel = 1
let descr_of_out_channel _out_channel = 1

type lock_command =
  |	F_ULOCK	(*	Unlock a region *)
  |	F_LOCK	(*	Lock a region for writing, and block if already locked *)
  |	F_TLOCK	(*	Lock a region for writing, or fail if already locked *)
  |	F_TEST	(*	Test a region for other process locks *)
  |	F_RLOCK	(*	Lock a region for reading, and block if already locked *)
  |	F_TRLOCK	(*	Lock a region for reading, or fail if already locked *)

let lockf _file_descr _lock_command _size = ()

let open_process_in _command = Pervasives.stdin
let close_process_in _in_channel = 0

let isatty _channel = true

let getlogin () = process_env_USER

let getpid () = process_pid

let stdout = 1
let stdin = 2
let stderr = 3

type inet_addr = string

let inet_addr_of_string addr = (addr: inet_addr)

let create_process_env _prog _args _env _new_stdin _new_stdout _new_stderr = 1

let kill _pid _sig = ()
let alarm _sec = ()
let waitpid _flags _pid = (0, WEXITED 0)
let wait () = (0, WEXITED 0)

type error =
  |	E2BIG	(* Argument list too long *)
  |	EACCES	(*	Permission denied *)
  |	EAGAIN	(*	Resource temporarily unavailable; try again *)
  |	EBADF	(*	Bad file descriptor *)
  |	EBUSY	(*	Resource unavailable *)
  |	ECHILD	(*	No child process *)
  |	EDEADLK	(*	Resource deadlock would occur *)
  |	EDOM	(*	Domain error for math functions, etc.  *)
  |	EEXIST	(*	File exists *)
  |	EFAULT	(*	Bad address *)
  |	EFBIG	(*	File too large *)
  |	EINTR	(*	Function interrupted by signal *)
  |	EINVAL	(*	Invalid argument *)
  |	EIO	(*	Hardware I/O error *)
  |	EISDIR	(*	Is a directory *)
  |	EMFILE	(*	Too many open files by the process *)
  |	EMLINK	(*	Too many links *)
  |	ENAMETOOLONG	(*	Filename too long *)
  |	ENFILE	(*	Too many open files in the system *)
  |	ENODEV	(*	No such device *)
  |	ENOENT	(*	No such file or directory *)
  |	ENOEXEC	(*	Not an executable file *)
  |	ENOLCK	(*	No locks available *)
  |	ENOMEM	(*	Not enough memory *)
  |	ENOSPC	(*	No space left on device *)
  |	ENOSYS	(*	Function not supported *)
  |	ENOTDIR	(*	Not a directory *)
  |	ENOTEMPTY	(*	Directory not empty *)
  |	ENOTTY	(*	Inappropriate I/O control operation *)
  |	ENXIO	(*	No such device or address *)
  |	EPERM	(*	Operation not permitted *)
  |	EPIPE	(*	Broken pipe *)
  |	ERANGE	(*	Result too large *)
  |	EROFS	(*	Read-only file system *)
  |	ESPIPE	(*	Invalid seek e.g. on a pipe *)
  |	ESRCH	(*	No such process *)
  |	EXDEV	(*	Invalid link *)
  |	EWOULDBLOCK	(*	Operation would block *)
  |	EINPROGRESS	(*	Operation now in progress *)
  |	EALREADY	(*	Operation already in progress *)
  |	ENOTSOCK	(*	Socket operation on non-socket *)
  |	EDESTADDRREQ	(*	Destination address required *)
  |	EMSGSIZE	(*	Message too long *)
  |	EPROTOTYPE	(*	Protocol wrong type for socket *)
  |	ENOPROTOOPT	(*	Protocol not available *)
  |	EPROTONOSUPPORT	(*	Protocol not supported *)
  |	ESOCKTNOSUPPORT	(*	Socket type not supported *)
  |	EOPNOTSUPP	(*	Operation not supported on socket *)
  |	EPFNOSUPPORT	(*	Protocol family not supported *)
  |	EAFNOSUPPORT	(*	Address family not supported by protocol family *)
  |	EADDRINUSE	(*	Address already in use *)
  |	EADDRNOTAVAIL	(*	Can't assign requested address *)
  |	ENETDOWN	(*	Network is down *)
  |	ENETUNREACH	(*	Network is unreachable *)
  |	ENETRESET	(*	Network dropped connection on reset *)
  |	ECONNABORTED	(*	Software caused connection abort *)
  |	ECONNRESET	(*	Connection reset by peer *)
  |	ENOBUFS	(*	No buffer space available *)
  |	EISCONN	(*	Socket is already connected *)
  |	ENOTCONN	(*	Socket is not connected *)
  |	ESHUTDOWN	(*	Can't send after socket shutdown *)
  |	ETOOMANYREFS	(*	Too many references: can't splice *)
  |	ETIMEDOUT	(*	Connection timed out *)
  |	ECONNREFUSED	(*	Connection refused *)
  |	EHOSTDOWN	(*	Host is down *)
  |	EHOSTUNREACH	(*	No route to host *)
  |	ELOOP	(*	Too many levels of symbolic links *)
  |	EOVERFLOW	(*	File size or position not representable *)
  |	EUNKNOWNERR of int	(*	Unknown error The type of error codes. Errors defined in the POSIX standard and additional errors from UNIX98 and BSD. All other errors are mapped to EUNKNOWNERR.  *)
exception Unix_error of error * string * string
