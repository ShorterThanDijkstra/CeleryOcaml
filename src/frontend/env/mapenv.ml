open Value

let init_mapenv () :(string, denval) Hashtbl.t = Hashtbl.create 16

let extend_env (var:string) (value:denval) (env:(string, denval) Hashtbl.t) = Hashtbl.add env var value; env

let apply_env (var:string) (env:(string, denval) Hashtbl.t) = Hashtbl.find env var


