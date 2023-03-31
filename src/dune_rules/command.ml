open Import

module Args0 = struct
  type without_targets = [ `Others ]

  type any =
    [ `Others
    | `Targets
    ]

  type expand = dir:Path.t -> string list Action_builder.t

  (* Debugging tip: if you changed this file and Dune got broken in a weird way
     it's probably because of the [Fail] constructor. *)
  type _ t =
    | A : string -> _ t
    | As : string list -> _ t
    | S : 'a t list -> 'a t
    | Concat : string * 'a t list -> 'a t
    | Dep : Path.t -> _ t
    | Deps : (Path.t list * string list) -> _ t
    | Target : Path.Build.t -> [> `Targets ] t
    | Path : Path.t -> _ t
    | Paths : Path.t list -> _ t
    | Hidden_deps : Dep.Set.t -> _ t
    | Hidden_targets : Path.Build.t list -> [> `Targets ] t
    | Dyn : without_targets t Action_builder.t -> _ t
    | Fail : Action_builder.fail -> _ t
    | Expand : expand -> _ t

  let dyn args = Dyn (Action_builder.map args ~f:(fun x -> As x))

  let empty = S []

  let rec as_any : without_targets t -> any t = function
    | A _ as x -> (x :> any t)
    | As _ as x -> (x :> any t)
    | S l -> S (List.map l ~f:as_any)
    | Concat (sep, l) -> Concat (sep, List.map l ~f:as_any)
    | Dep _ as x -> (x :> any t)
    | Deps _ as x -> (x :> any t)
    | Path _ as x -> (x :> any t)
    | Paths _ as x -> (x :> any t)
    | Hidden_deps _ as x -> (x :> any t)
    | Dyn _ as x -> (x :> any t)
    | Fail _ as x -> (x :> any t)
    | Expand _ as x -> (x :> any t)
end

open Args0

let rec expand :
    type a.
       ?module_name_built:string
    -> ?deps:(string * string list Action_builder.t) list Action_builder.t
    -> ?from:string
    -> dir:Path.t
    -> a t
    -> string list Action_builder.With_targets.t =
 fun ?(module_name_built = "Not_defined")
     ?(deps = Action_builder.return [ ("", Action_builder.return [ "" ]) ])
     ?(from = "unknown321") ~dir t ->
  let _s =
    match t with
    | A s -> "A   " ^ s
    | As s -> " As  " ^ String.concat ~sep:"," s
    | S _ -> " S  "
    | Concat _ -> "Concat"
    | Dep p -> " Dep  " ^ Path.to_string p
    | Deps _ -> "  Deps "
    | Path _ -> " Path  "
    | Paths _ -> "Paths"
    | Hidden_deps _ -> "Hidden_deps"
    | Dyn _ -> "Dyn"
    | Fail _ -> "Fail"
    | Expand _ -> "Expand"
    | Target _ -> "Target"
    | Hidden_targets _ -> "Hidden_targets"
  in
 (*  if not (String.equal module_name_built "Not_defined") then
    Dune_util.Log.info
      [ Pp.textf "Expand for module %s is %s\n" module_name_built _s ]; *)
  (* if Char.equal (String.get from 0) '3' then
     Dune_util.Log.info [ Pp.textf "buildcmfrom %s %s\n" from s ]; *)
  match t with
  | A s -> Action_builder.With_targets.return [ s ]
  | As l -> Action_builder.With_targets.return l
  | Dep fn ->
    Action_builder.with_no_targets
      (Action_builder.map
         (Action_builder.path ~from:(from ^ "Dep fn expand") fn)
         ~f:(fun () -> [ Path.reach fn ~from:dir ]))
  | Path fn -> Action_builder.With_targets.return [ Path.reach fn ~from:dir ]
  | Deps (fns, m_name_list) ->
    Action_builder.with_no_targets
      (Action_builder.map
         (Action_builder.bind deps ~f:(fun external_deps_map ->
              (if List.length fns == List.length m_name_list then
               let external_module_deps =
                 List.map external_deps_map ~f:(fun (a, b) ->
                     (a, Action_builder.map b ~f:(fun deps -> deps)))
               in
               let _ext_dep_map = String.Map.of_list_exn external_module_deps in
               ());
              Action_builder.paths ~external_deps:[] ~from:(from ^ "->expand")
                fns))
         ~f:(fun () -> List.map fns ~f:(Path.reach ~from:dir)))
  | Paths fns ->
    Action_builder.With_targets.return (List.map fns ~f:(Path.reach ~from:dir))
  | S ts ->
    Action_builder.With_targets.map
      (Action_builder.With_targets.all
         (List.map ts
            ~f:
              (expand ~module_name_built ~deps
                 ~from:
                   (Printf.sprintf "  %s iter S ~>  %s module_built %s\n" from
                      (List.length ts |> Int.to_string)
                      module_name_built)
                 ~dir)))
      ~f:List.concat
  | Concat (sep, ts) ->
    Action_builder.With_targets.map
      (expand ~module_name_built ~deps ~from ~dir (S ts)) ~f:(fun x ->
        [ String.concat ~sep x ])
  | Target fn ->
    Action_builder.with_file_targets ~file_targets:[ fn ]
      (Action_builder.return [ Path.reach (Path.build fn) ~from:dir ])
  | Dyn dyn ->
    Action_builder.with_no_targets
      (Action_builder.bind dyn ~f:(fun t ->
           expand_no_targets ~module_name_built ~deps ~from ~dir t))
  | Fail f -> Action_builder.with_no_targets (Action_builder.fail f)
  | Hidden_deps deps ->
    Dune_util.Log.info
      [ Pp.textf "Hidden_deps for %s %s\n" module_name_built
          (Dep.Set.to_dyn deps |> Dyn.to_string)
      ];
    (* if not (String.equal module_name_built "Not_defined") then
       Dune_util.Log.info [ Pp.textf "HIDDEN DEPS NOT DEF" ]; *)
    Action_builder.with_no_targets
      (Action_builder.map
         (Action_builder.deps
            ~from:
              (from
              ^ Printf.sprintf "->expand|Hiddendeps in  Dep.set is %s \n - "
                  (Dep.Set.to_dyn deps |> Dyn.to_string))
            deps)
         ~f:(fun () -> []))
  | Hidden_targets fns ->
    Action_builder.with_file_targets ~file_targets:fns
      (Action_builder.return [])
  | Expand f ->
    Dune_util.Log.info [ Pp.textf "expand from %s" from ];
    Action_builder.with_no_targets (f ~dir)

and expand_no_targets ?(module_name_built = "Not_defined")
    ?(deps = Action_builder.return [ ("", Action_builder.return [ "" ]) ])
    ?(from = "unknown123") ~dir (t : without_targets t) =
  let { Action_builder.With_targets.build; targets } =
    expand ~module_name_built ~deps ~from ~dir t
  in
  assert (Targets.is_empty targets);
  build

let dep_prog = function
  | Ok p -> Action_builder.path ~from:"dep_prog" p
  | Error _ -> Action_builder.return ()

let run ?(module_name_built = "Not_defined")
    ?(deps = Action_builder.return [ ("", Action_builder.return [ "" ]) ])
    ?(from = "unknown") ~dir ?sandbox ?stdout_to prog args =
  (*   let deps = Action_builder.return [ "" ] in
 *)
  Action_builder.With_targets.add ~file_targets:(Option.to_list stdout_to)
    (let open Action_builder.With_targets.O in
    let+ () = Action_builder.with_no_targets (dep_prog prog)
    and+ args =
      expand ~module_name_built ~deps ~from:(from ^ "Command.run") ~dir (S args)
    in
    let action = Action.run prog args in
    let action =
      match stdout_to with
      | None -> action
      | Some path -> Action.with_stdout_to path action
    in
    Action.Full.make ?sandbox (Action.chdir dir action))

let run' ~dir prog args =
  let open Action_builder.O in
  let+ () = dep_prog prog
  and+ args = expand_no_targets ~from:"run'" ~dir (S args) in
  Action.Full.make (Action.chdir dir (Action.run prog args))

let quote_args =
  let rec loop quote = function
    | [] -> []
    | arg :: args -> quote :: arg :: loop quote args
  in
  fun quote args -> As (loop quote args)

let fail e = Fail { fail = (fun _ -> raise e) }

module Args = struct
  include Args0

  let memo ?(from = "unknown") t =
    let memo =
      Action_builder.create_memo "Command.Args.memo"
        ~input:(module Path)
        (fun dir -> expand_no_targets ~from:(from ^ "->memo") ~dir t)
    in
    Expand (fun ~dir -> Action_builder.exec_memo memo dir)
end

module Ml_kind = struct
  let flag t = Ml_kind.choose ~impl:(Args.A "-impl") ~intf:(A "-intf") t

  let ppx_driver_flag t =
    Ml_kind.choose ~impl:(Args.A "--impl") ~intf:(A "--intf") t
end
