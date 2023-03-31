open Import

module Includes = struct
  type t = Command.Args.without_targets Command.Args.t Lib_mode.Cm_kind.Map.t

  let make ?dep_graphs ?(from = "unknown") () ~project ~opaque ~requires ~md
      ~mdeps =
    let patched = false in
    let res =
      let _ = mdeps in
      let open Lib_mode.Cm_kind.Map in
      let _mname = Module.name md |> Module_name.to_string in
      let open Resolve.Memo.O in
      let iflags libs mode = Lib_flags.L.include_flags ~project libs mode in
      let make_includes_args ~mode groups =
        Command.Args.memo
          ~from:
            (from ^ "->complilation_context.includes.make.make_includes_args")
          (Resolve.Memo.args
             (let r =
                match dep_graphs with
                | Some (dep_graphs : Dep_graph.t Ml_kind.Dict.t) ->
                  let dep_graph = Ml_kind.Dict.get dep_graphs Ml_kind.Impl in

                  let module_deps = Dep_graph.deps_of dep_graph md in

                  let a = Action_builder.run module_deps Action_builder.Eager in
                  let rrr = Resolve.Memo.lift_memo a in
                  rrr
                | None -> Resolve.Memo.return ([], Dep.Map.empty)
              in

              let* libs = requires in
              let+ abra, _ = r in
              let external_dep_names =
                List.filter_map ~f:Module_dep.filter_external abra
                |> List.map ~f:Module_dep.External_name.to_string
              in
              let external_dep_names =
                if List.is_empty external_dep_names then [ "" ]
                else external_dep_names
              in
              let filtered =
                List.fold_map external_dep_names ~init:libs ~f:(fun b a ->
                    let filtered = Lib_flags.L.filter_by_name b a in
                    (filtered, a))
              in
              let l1 = fst filtered in
              (* Dune_util.Log.info
                 [ Pp.textf
                     "Making context for module %s\n\
                     \ libs are %s\n\
                     \ libs atref are %s\n\
                      abrakadabra deps is %s\n\
                     \ " mname
                     (Lib_flags.L.to_string_list libs |> String.concat ~sep:",")
                     (Lib_flags.L.to_string_list l1 |> String.concat ~sep:",")
                     (external_dep_names |> String.concat ~sep:",")
                 ]; *)
              (* Dune_util.Log.info
                 [ Pp.textf "Includes.make cmi_includes libs are %s \n "
                     (Lib_flags.L.to_string_list libs |> String.concat ~sep:",")
                 ]; *)
              let lib_list = if patched then l1 else libs in
              Command.Args.S
                [ iflags lib_list mode
                ; Hidden_deps
                    (Lib_file_deps.deps ~from:"make_includes_args " lib_list
                       ~groups)
                ]))
      in
      let cmi_includes = make_includes_args ~mode:(Ocaml Byte) [ Ocaml Cmi ] in
      let cmx_includes =
        Command.Args.memo
          ~from:(from ^ "->complilation_context.includes.make")
          (Resolve.Memo.args
             (let r =
                match dep_graphs with
                | Some (dep_graphs : Dep_graph.t Ml_kind.Dict.t) ->
                  let dep_graph = Ml_kind.Dict.get dep_graphs Ml_kind.Impl in

                  let module_deps = Dep_graph.deps_of dep_graph md in

                  let a = Action_builder.run module_deps Action_builder.Eager in
                  let rrr = Resolve.Memo.lift_memo a in
                  rrr
                | None -> Resolve.Memo.return ([], Dep.Map.empty)
              in
              let* libs = requires in
              let+ abra, _ = r in
              let external_dep_names =
                List.filter_map ~f:Module_dep.filter_external abra
                |> List.map ~f:Module_dep.External_name.to_string
              in
              let external_dep_names =
                if List.is_empty external_dep_names then [ "" ]
                else external_dep_names
              in
              let filtered =
                List.fold_map external_dep_names ~init:libs ~f:(fun b a ->
                    let filtered = Lib_flags.L.filter_by_name b a in
                    (filtered, a))
              in
              let l1 = fst filtered in
              (* Dune_util.Log.info
                 [ Pp.textf
                     "Making context for module %s\n\
                     \ libs are %s\n\
                     \ libs atref are %s\n\
                      abrakadabra deps is %s\n\
                     \ " mname
                     (Lib_flags.L.to_string_list libs |> String.concat ~sep:",")
                     (Lib_flags.L.to_string_list l1 |> String.concat ~sep:",")
                     (external_dep_names |> String.concat ~sep:",")
                 ]; *)
              (* Dune_util.Log.info
                 [ Pp.textf "Includes.make cmx_includes libs are %s \n "
                     (Lib_flags.L.to_string_list libs |> String.concat ~sep:",")
                 ]; *)
              let lib_list = if patched then l1 else libs in

              Command.Args.S
                [ iflags lib_list (Ocaml Native)
                ; Hidden_deps
                    (if opaque then
                     List.map lib_list ~f:(fun lib ->
                         ( lib
                         , if Lib.is_local lib then
                             [ Lib_file_deps.Group.Ocaml Cmi ]
                           else [ Ocaml Cmi; Ocaml Cmx ] ))
                     |> Lib_file_deps.deps_with_exts
                    else
                      Lib_file_deps.deps ~from:"cmx_includes " lib_list
                        ~groups:[ Lib_file_deps.Group.Ocaml Cmi; Ocaml Cmx ])
                ]))
      in
      let melange_cmi_includes =
        make_includes_args ~mode:Melange [ Melange Cmi ]
      in
      let melange_cmj_includes =
        make_includes_args ~mode:Melange [ Melange Cmi; Melange Cmj ]
      in
      { ocaml = { cmi = cmi_includes; cmo = cmi_includes; cmx = cmx_includes }
      ; melange = { cmi = melange_cmi_includes; cmj = melange_cmj_includes }
      }
    in
    res

  let empty = Lib_mode.Cm_kind.Map.make_all Command.Args.empty
end

type opaque =
  | Explicit of bool
  | Inherit_from_settings

let eval_opaque (context : Context.t) = function
  | Explicit b -> b
  | Inherit_from_settings ->
    Profile.is_dev context.profile
    && Ocaml.Version.supports_opaque_for_mli context.version

type modules =
  { modules : Modules.t
  ; dep_graphs : Dep_graph.t Ml_kind.Dict.t
  }

let singleton_modules m =
  { modules = Modules.singleton m; dep_graphs = Dep_graph.Ml_kind.dummy m }

type t =
  { super_context : Super_context.t
  ; scope : Scope.t
  ; expander : Expander.t
  ; obj_dir : Path.Build.t Obj_dir.t
  ; modules : modules
  ; flags : Ocaml_flags.t
  ; requires_compile : Lib.t list Resolve.Memo.t
  ; requires_link : Lib.t list Resolve.t Memo.Lazy.t
  ; includes :
      md:Module.t -> mdeps:Module_dep.t list Action_builder.t -> Includes.t
  ; preprocessing : Pp_spec.t
  ; opaque : bool
  ; stdlib : Ocaml_stdlib.t option
  ; js_of_ocaml : Js_of_ocaml.In_context.t option
  ; sandbox : Sandbox_config.t
  ; package : Package.t option
  ; vimpl : Vimpl.t option
  ; public_lib_name : Lib_name.t option
  ; modes : Lib_mode.Map.Set.t
  ; bin_annot : bool
  ; ocamldep_modules_data : Ocamldep.Modules_data.t
  ; loc : Loc.t option
  }

let loc t = t.loc

let super_context t = t.super_context

let scope t = t.scope

let expander t = t.expander

let dir t = Obj_dir.dir t.obj_dir

let obj_dir t = t.obj_dir

let modules t = t.modules.modules

let flags t = t.flags

let requires_compile t = t.requires_compile

let requires_link t = Memo.Lazy.force t.requires_link

let includes t = t.includes

let preprocessing t = t.preprocessing

let opaque t = t.opaque

let stdlib t = t.stdlib

let js_of_ocaml t = t.js_of_ocaml

let sandbox t = t.sandbox

let set_sandbox t sandbox = { t with sandbox }

let package t = t.package

let public_lib_name t = t.public_lib_name

let vimpl t = t.vimpl

let modes t = t.modes

let bin_annot t = t.bin_annot

let context t = Super_context.context t.super_context

let ocamldep_modules_data t = t.ocamldep_modules_data

let dep_graphs t = t.modules.dep_graphs

let create ?(from = "unkn") ~super_context ~scope ~expander ~obj_dir ~modules
    ~flags ~requires_compile ~requires_link ?(preprocessing = Pp_spec.dummy)
    ~opaque ?stdlib ~js_of_ocaml ~package ?public_lib_name ?vimpl ?modes
    ?bin_annot ?loc () =
  let open Memo.O in
  let project = Scope.project scope in
  let requires_compile =
    if Dune_project.implicit_transitive_deps project then
      Memo.Lazy.force requires_link
    else requires_compile
  in
  let sandbox =
    (* With sandboxing, there are a few build errors in ocaml platform 1162238ae
       like: File "ocaml_modules/ocamlgraph/src/pack.ml", line 1: Error: The
       implementation ocaml_modules/ocamlgraph/src/pack.ml does not match the
       interface
       ocaml_modules/ocamlgraph/src/.graph.objs/byte/graph__Pack.cmi: *)
    Sandbox_config.no_sandboxing
  in
  let modes =
    let default =
      { Lib_mode.Map.ocaml =
          Mode.Dict.make_both (Some Dune_file.Mode_conf.Kind.Inherited)
      ; melange = None
      }
    in
    Option.value ~default modes |> Lib_mode.Map.map ~f:Option.is_some
  in
  let opaque = eval_opaque (Super_context.context super_context) opaque in
  let ocamldep_modules_data : Ocamldep.Modules_data.t =
    { dir = Obj_dir.dir obj_dir
    ; sandbox = Sandbox_config.no_special_requirements
    ; obj_dir
    ; sctx = super_context
    ; vimpl
    ; modules
    ; stdlib
    }
  in

  let+ dep_graphs = Dep_rules.rules ocamldep_modules_data
  and+ bin_annot =
    match bin_annot with
    | Some b -> Memo.return b
    | None -> Super_context.bin_annot super_context ~dir:(Obj_dir.dir obj_dir)
  in
  let dep_graphs = dep_graphs in
  let includes =
    Includes.make ~dep_graphs
      ~from:(from ^ "->compliation_context_create")
      ~project ~opaque ~requires:requires_compile ()
  in
  (*   Dune_util.Log.info [ Pp.textf "create from %s" from ];
 *)
  { super_context
  ; scope
  ; expander
  ; obj_dir
  ; modules = { modules; dep_graphs }
  ; flags
  ; requires_compile
  ; requires_link
  ; includes
  ; preprocessing
  ; opaque
  ; stdlib
  ; js_of_ocaml
  ; sandbox
  ; package
  ; vimpl
  ; public_lib_name
  ; modes
  ; bin_annot
  ; ocamldep_modules_data
  ; loc
  }

let for_alias_module t alias_module =
  let flags =
    let project = Scope.project t.scope in
    let dune_version = Dune_project.dune_version project in
    let profile = (Super_context.context t.super_context).profile in
    Ocaml_flags.default ~dune_version ~profile
  in
  let sandbox =
    let ctx = Super_context.context t.super_context in
    (* If the compiler reads the cmi for module alias even with [-w -49
       -no-alias-deps], we must sandbox the build of the alias module since the
       modules it references are built after. *)
    if Ocaml.Version.always_reads_alias_cmi ctx.version then
      Sandbox_config.needs_sandboxing
    else Sandbox_config.no_special_requirements
  in
  let (modules, includes)
        : modules
          * (   md:Module.t
             -> mdeps:Module_dep.t list Action_builder.t
             -> Includes.t) =
    match Modules.is_stdlib_alias t.modules.modules alias_module with
    | false ->
      (singleton_modules alias_module, fun ~md:_ ~mdeps:_ -> Includes.empty)
    | true ->
      (* The stdlib alias module is different from the alias modules usually
         produced by Dune: it contains code and depends on a few other
         [CamlinnternalXXX] modules from the stdlib, so we need the full set of
         modules to compile it. *)
      (t.modules, t.includes)
  in
  { t with
    flags =
      Ocaml_flags.append_common flags
        [ "-w"; "-49"; "-nopervasives"; "-nostdlib" ]
  ; includes
  ; stdlib = None
  ; sandbox
  ; modules
  }

let for_root_module t root_module =
  let flags =
    let project = Scope.project t.scope in
    let dune_version = Dune_project.dune_version project in
    let profile = (Super_context.context t.super_context).profile in
    Ocaml_flags.default ~profile ~dune_version
  in
  { t with
    flags =
      Ocaml_flags.append_common flags
        [ "-w"; "-49"; "-nopervasives"; "-nostdlib" ]
  ; stdlib = None
  ; modules = singleton_modules root_module
  }

let for_module_generated_at_link_time cctx ~requires ~module_ =
  let opaque =
    (* Cmi's of link time generated modules are compiled with -opaque, hence
       their implementation must also be compiled with -opaque *)
    let ctx = Super_context.context cctx.super_context in
    Ocaml.Version.supports_opaque_for_mli ctx.version
  in
  let modules = singleton_modules module_ in
  let includes =
    Includes.make ~from:"for_module_generated_at_link_time"
      ~project:(Scope.project cctx.scope) ~opaque ~requires ()
  in
  { cctx with
    opaque
  ; flags = Ocaml_flags.empty
  ; requires_link = Memo.lazy_ (fun () -> requires)
  ; requires_compile = requires
  ; includes
  ; modules
  }

let for_wrapped_compat t =
  { t with includes = (fun ~md:_ ~mdeps:_ -> Includes.empty); stdlib = None }

let for_plugin_executable t ~embed_in_plugin_libraries =
  let libs = Scope.libs t.scope in
  let requires_link =
    Memo.lazy_ (fun () ->
        Resolve.Memo.List.map ~f:(Lib.DB.resolve libs) embed_in_plugin_libraries)
  in
  { t with requires_link }

let without_bin_annot t = { t with bin_annot = false }

let entry_module_names sctx t =
  match Lib_info.entry_modules (Lib.info t) with
  | External d -> Resolve.Memo.of_result d
  | Local ->
    let open Memo.O in
    let+ modules = Dir_contents.modules_of_lib sctx t in
    let modules = Option.value_exn modules in
    Resolve.return (Modules.entry_modules modules |> List.map ~f:Module.name)

let root_module_entries t =
  let open Action_builder.O in
  let* requires = Resolve.Memo.read t.requires_compile in
  let* l =
    Action_builder.List.map requires ~f:(fun lib ->
        Action_builder.of_memo (entry_module_names t.super_context lib)
        >>= Resolve.read)
  in
  Action_builder.return (List.concat l)

let set_obj_dir t obj_dir = { t with obj_dir }

let set_modes t ~modes = { t with modes }
