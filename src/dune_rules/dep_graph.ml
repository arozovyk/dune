open Import
open Action_builder.O

type t =
  { dir : Path.Build.t
  ; per_module :
      (Module.t list Action_builder.t * Ocamldep.Modules_data.odep_out)
      Module_name.Unique.Map.t
  }

let make ~dir ~per_module = { dir; per_module }

let deps_of t (m : Module.t) =
  match Module_name.Unique.Map.find t.per_module (Module.obj_name m) with
  | Some x -> x
  | None ->
    Code_error.raise "Ocamldep.Dep_graph.deps_of"
      [ ("dir", Path.Build.to_dyn t.dir)
      ; ( "modules"
        , Dyn.(list Module_name.Unique.to_dyn)
            (Module_name.Unique.Map.keys t.per_module) )
      ; ("m", Module.to_dyn m)
      ]

module Top_closure = Top_closure.Make (Module_name.Unique.Set) (Action_builder)

let top_closed t (modules : Module.t list) =
  let+ res =
    Top_closure.top_closure modules ~key:Module.obj_name ~deps:(fun m ->
        Module_name.Unique.Map.find_exn t.per_module (Module.obj_name m) |> fst)
  in
  match res with
  | Ok modules -> modules
  | Error cycle ->
    User_error.raise
      [ Pp.textf "dependency cycle between modules in %s:"
          (Path.Build.to_string t.dir)
      ; Pp.chain cycle ~f:(fun m ->
            Pp.verbatim (Module_name.to_string (Module.name m)))
      ]

let top_closed_implementations t modules =
  let top_c =
    top_closed t (List.filter modules ~f:(Module.has ~ml_kind:Impl))
  in

  Action_builder.map
    ~f:(fun mod_l ->
      let obj_map =
        List.map modules ~f:(fun x -> (x, x))
        |> Module.Obj_map.of_list_reduce ~f:(fun x y ->
               match Module.kind x with
               | Impl_vmodule -> x
               | _ -> y)
      in
      List.filter_map
        ~f:(fun m ->
          match Module.kind m with
          | Virtual -> Some (Module.Obj_map.find_exn obj_map m)
          | Intf_only -> None
          | _ -> Some m)
        mod_l)
    top_c
  |> Action_builder.memoize "top sorted implementations"

let dummy (m : Module.t) =
  let odep_dummy =
    Ocamldep.odep_dummy
      (Module.source ~ml_kind:Impl m |> Option.value_exn)
      "dep_graph_dummy"
  in
  { dir = Path.Build.root
  ; per_module =
      Module_name.Unique.Map.singleton (Module.obj_name m)
        (Action_builder.return [], odep_dummy)
  }

module Ml_kind = struct
  type nonrec t = t Ml_kind.Dict.t

  let dummy m = Ml_kind.Dict.make_both (dummy m)
end
