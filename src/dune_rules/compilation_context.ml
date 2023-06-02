open Import

module Includes = struct
  type t = Command.Args.without_targets Command.Args.t Lib_mode.Cm_kind.Map.t

  let _filter_updated (libs : Lib.t list) module_deps emns md =
    let open Resolve.Memo.O in
    let* (module_deps, flags), _ = module_deps in
    let rec flag_open_present entry_lib_name l =
      match l with
      | flag :: entry_name :: t ->
        if
          String.equal flag "-open"
          && String.is_prefix ~prefix:entry_lib_name entry_name
        then true
        else flag_open_present entry_lib_name (entry_name :: t)
      | _ -> false
    in
    let dep_names =
      List.map module_deps ~f:(fun mdep ->
          let open Module_dep in
          match mdep with
          (* Lib shadowing by a local module obliges
             us to also check if a lib is a local module *)
          | Local m -> Module.name m |> Module_name.to_string
          | External mname -> External_name.to_string mname)
    in
    let exists_in_odeps lib_name =
      List.exists dep_names ~f:(fun odep ->
          (*  Dune_util.Log.info [ Pp.textf "Comparing %s %s \n" lib_name odep ]; *)
          String.equal lib_name odep || String.is_prefix ~prefix:odep lib_name)
    in
    (* if
         (* FIXME: menhir mocks (i.e melange-compiler-libs.0.0.1-414) ? we skip for now  *)
         String.is_suffix (Module.name md |> Module_name.to_string) ~suffix:"__mock"
       then libs
       else *)
    let entry_names_map libs =
      let inrt =
        List.map libs ~f:(fun lib ->
            let local_lib = Lib.Local.of_lib lib in
            if Option.is_none local_lib then Resolve.Memo.return ([], lib)
            else
              let em =
                emns (Option.value_exn local_lib) |> Resolve.Memo.lift_memo
              in
              Resolve.Memo.map em ~f:(fun emns_l ->
                  (List.map emns_l ~f:Module.name, lib)))
        |> Resolve.Memo.all
      in
      inrt
    in

    let map e =
      let init = Module_name.Map.empty in
      List.fold_left e ~init ~f:(fun init (ml, lib) ->
          List.fold_left ml ~init ~f:(fun acc m_name ->
              match Module_name.Map.add acc m_name lib with
              | Ok map -> map
              | Error _ -> acc))
    in

    let* enl = entry_names_map libs in
    if List.is_empty dep_names then Resolve.Memo.return libs
    else
      let r2 =
        List.filter_map enl ~f:(fun (entry_names, lib) ->
            let entries_empty = List.is_empty entry_names in
            let emnstr =
              List.map entry_names ~f:(fun m -> Module_name.to_string m)
            in
            if
              List.exists emnstr ~f:(fun emn ->
                  String.equal emn "Fmt"
                  && String.equal
                       (Module.name md |> Module_name.to_string)
                       "OLS")
            then
              Some
                (let closure = Lib.closure [ lib ] ~linking:true in
                 Resolve.Memo.map closure ~f:(fun _cl ->
                     (* Dune_util.Log.info
                        [ Pp.textf
                            "Debugging FMT OLS %s \n\
                             having entries: (%s)\n\
                             for module %s\n\
                             Odep {%s}\n\
                             Flags [%s]\n\
                            \ having closure [%s]\n"
                            (Lib.name lib |> Lib_name.to_string)
                            (String.concat emnstr ~sep:",")
                            (Module.name md |> Module_name.to_string)
                            (String.concat dep_names ~sep:",")
                            (String.concat flags ~sep:",")
                            (List.map cl ~f:(fun lib ->
                                 Lib.name lib |> Lib_name.to_string)
                            |> String.concat ~sep:",")
                        ]; *)
                     Some lib))
            else
              let melange_mode =
                Lib_mode.Map.get
                  (Lib.info lib |> Lib_info.modes)
                  Lib_mode.Melange
              in
              let implements =
                Option.is_some (Lib_info.implements (Lib.info lib))
              in
              let local = Lib.Local.of_lib lib |> Option.is_none in

              let virtual_ =
                Option.is_some (Lib_info.virtual_ (Lib.info lib))
              in
              if
                implements || virtual_ || local || melange_mode || entries_empty
              then Some (Resolve.Memo.return (Some lib))
              else if
                List.exists emnstr ~f:(fun emn ->
                    let is_melange_wrapper =
                      String.equal "Melange_wrapper" emn
                    in
                    let is_unwrapped = flag_open_present emn flags in
                    is_melange_wrapper || is_unwrapped || exists_in_odeps emn)
              then Some (Resolve.Memo.return (Some lib))
              else
                let enlmap = map enl in
                (* Closure of each odep -> is []*)
                Some
                  (let odep_libs =
                     List.filter_map dep_names ~f:(fun odep_module ->
                         let odep_mod_name =
                           Module_name.of_string odep_module
                         in
                         match Module_name.Map.find enlmap odep_mod_name with
                         | Some lib ->
                           let closure = Lib.closure [ lib ] ~linking:true in
                           Some closure
                         | None -> None)
                     |> Resolve.Memo.all
                     |> Resolve.Memo.map ~f:List.concat
                   in
                   let entry_odep_closure_map =
                     Resolve.Memo.bind odep_libs ~f:(fun e -> entry_names_map e)
                   in
                   Resolve.Memo.map entry_odep_closure_map
                     ~f:(fun enl (*odep lib closure and its entry names*) ->
                       if
                         List.exists enl ~f:(fun (mn_list_odep, _) ->
                             (* Dune_util.Log.info
                                [ Pp.textf
                                    "For module %s\n\
                                     Closure list odep [%s]\n\
                                     Entries of filtered lib [%s]"
                                    (Module.name md |> Module_name.to_string)
                                    (List.map mn_list_odep
                                       ~f:Module_name.to_string
                                    |> String.concat ~sep:",")
                                    (String.concat emnstr ~sep:",")
                                ]; *)
                             List.exists mn_list_odep ~f:(fun mn_odep ->
                                 List.exists emnstr ~f:(fun init_lib_mn ->
                                     String.equal
                                       (Module_name.to_string mn_odep)
                                       init_lib_mn)))
                       then Some lib
                       else
                         (* Dune_util.Log.info
                            [ Pp.textf
                                "Debugging remove  %s \n\
                                 having entries: (%s)\n\
                                 for module %s\n\
                                 Odep {%s}\n\n\
                                \ "
                                (Lib.name lib |> Lib_name.to_string)
                                (String.concat emnstr ~sep:",")
                                (Module.name md |> Module_name.to_string)
                                (String.concat dep_names ~sep:",")
                            ]; *)
                         None))
            (* let closure = Lib.closure [ lib ] ~linking:true in
               Some
                 (Resolve.Memo.bind closure ~f:(fun c ->
                      let emn_map = entry_names_map c in
                      Resolve.Memo.bind emn_map ~f:(fun entry_names ->
                          if
                            List.exists
                              (List.filter entry_names ~f:(fun (e, _) ->
                                   List.is_non_empty e))
                              ~f:(fun (e, _) ->
                                let emnstr =
                                  List.map e ~f:(fun m ->
                                      Module_name.to_string m)
                                in

                                List.exists emnstr
                                  ~f:(fun entry_name_closure ->
                                    Dune_util.Log.info
                                      [ Pp.textf
                                          "Checkout existence of %s \n\
                                           for module %s\n\
                                          \ Odep {%s}\n"
                                          entry_name_closure
                                          (Module.name md
                                         |> Module_name.to_string)
                                          (String.concat dep_names ~sep:",")
                                      ];
                                    exists_in_odeps entry_name_closure))
                          then Resolve.Memo.return (Some lib)
                          else (
                            Dune_util.Log.info
                              [ Pp.textf
                                  "Debugging remove  %s \n\
                                   having entries: (%s)\n\
                                   for module %s\n\
                                   Odep {%s}\n\n\
                                  \                                        \
                                   having closure [%s]\n"
                                  (Lib.name lib |> Lib_name.to_string)
                                  (String.concat emnstr ~sep:",")
                                  (Module.name md |> Module_name.to_string)
                                  (String.concat dep_names ~sep:",")
                                  (List.map c ~f:(fun lib ->
                                       Lib.name lib |> Lib_name.to_string)
                                  |> String.concat ~sep:",")
                              ];
                            Resolve.Memo.return None))) *)
            (* Dune_util.Log.info
                 [ Pp.textf
                     "Removing_upd %s \n\
                      having entries: (%s)\n\
                      for module %s\n\
                      Odep {%s}\n\
                      Flags [%s]\n\
                      having re_exports [%s]\n"
                     (Lib.name lib |> Lib_name.to_string)
                     (String.concat emnstr ~sep:",")
                     (Module.name md |> Module_name.to_string)
                     (String.concat dep_names ~sep:",")
                     (String.concat flags ~sep:",")
                     (List.map r ~f:(fun lib ->
                          Lib.name lib |> Lib_name.to_string)
                     |> String.concat ~sep:",")
                 ];
               None *))
      in
      let r3 = Resolve.Memo.all r2 |> Resolve.Memo.map ~f:List.filter_opt in
      (* let _r =
           List.filter_map entry_names_map ~f:(fun (lib, entry_names, r) ->
               let entries_empty = List.is_empty entry_names in
               let emnstr =
                 List.map entry_names ~f:(fun m ->
                     Module.name m |> Module_name.to_string)
               in
               if
                 List.exists emnstr ~f:(fun emn ->
                     String.equal emn "Fmt"
                     && String.equal
                          (Module.name md |> Module_name.to_string)
                          "OLS")
               then
                 Dune_util.Log.info
                   [ Pp.textf
                       "Debugging FMT OLS %s \n\
                        having entries: (%s)\n\
                        for module %s\n\
                        Odep {%s}\n\
                        Flags [%s]\n\
                        having re_exports [%s]\n"
                       (Lib.name lib |> Lib_name.to_string)
                       (String.concat emnstr ~sep:",")
                       (Module.name md |> Module_name.to_string)
                       (String.concat dep_names ~sep:",")
                       (String.concat flags ~sep:",")
                       (List.map r ~f:(fun lib ->
                            Lib.name lib |> Lib_name.to_string)
                       |> String.concat ~sep:",")
                   ];
               let melange_mode =
                 Lib_mode.Map.get (Lib.info lib |> Lib_info.modes) Lib_mode.Melange
               in
               let implements =
                 Option.is_some (Lib_info.implements (Lib.info lib))
               in
               let local = Lib.Local.of_lib lib |> Option.is_none in
               let virtual_ = Option.is_some (Lib_info.virtual_ (Lib.info lib)) in
               if implements || virtual_ || local || melange_mode || entries_empty
               then Some lib
               else if
                 List.exists emnstr ~f:(fun emn ->
                     let is_melange_wrapper = String.equal "Melange_wrapper" emn in
                     let is_unwrapped = flag_open_present emn flags in
                     is_melange_wrapper || is_unwrapped || exists_in_odeps emn)
               then Some lib
               else (
                 Dune_util.Log.info
                   [ Pp.textf
                       "Removing_upd %s \n\
                        having entries: (%s)\n\
                        for module %s\n\
                        Odep {%s}\n\
                        Flags [%s]\n\
                        having re_exports [%s]\n"
                       (Lib.name lib |> Lib_name.to_string)
                       (String.concat emnstr ~sep:",")
                       (Module.name md |> Module_name.to_string)
                       (String.concat dep_names ~sep:",")
                       (String.concat flags ~sep:",")
                       (List.map r ~f:(fun lib ->
                            Lib.name lib |> Lib_name.to_string)
                       |> String.concat ~sep:",")
                   ];
                 None))
         in *)
      r3

  let _filter_with_odeps libs deps md lib_top_module_map
      lib_to_entry_modules_map =
    let open Resolve.Memo.O in
    let* (module_deps, flags), _ = deps in
    let* lib_to_entry_modules_map = lib_to_entry_modules_map in
    let lib_to_entry_modules_map =
      Lib.Map.of_list lib_to_entry_modules_map
      |> Result.value ~default:Lib.Map.empty
    in
    let+ lib_top_module_map = lib_top_module_map in
    let lib_top_module_map =
      List.concat lib_top_module_map
      |> Module_name.Map.of_list
      |> Result.value ~default:Module_name.Map.empty
    in
    if
      List.is_empty module_deps
      || Module_name.Map.is_empty lib_top_module_map
      || Lib.Map.is_empty lib_to_entry_modules_map
    then libs
    else
      let rec flag_open_present entry_lib_name l =
        match l with
        | flag :: entry_name :: t ->
          if
            String.equal flag "-open"
            && String.is_prefix ~prefix:entry_lib_name entry_name
          then true
          else flag_open_present entry_lib_name (entry_name :: t)
        | _ -> false
      in
      let dep_names =
        List.map module_deps ~f:(fun mdep ->
            let open Module_dep in
            match mdep with
            (* Lib shadowing by a local module obliges
               us to also check if a lib is a local module *)
            | Local m -> Module.name m |> Module_name.to_string
            | External mname -> External_name.to_string mname)
      in
      let exists_in_odeps lib_name =
        List.exists dep_names ~f:(fun odep ->
            (*  Dune_util.Log.info [ Pp.textf "Comparing %s %s \n" lib_name odep ]; *)
            String.equal lib_name odep || String.is_prefix ~prefix:odep lib_name)
      in
      if
        (* FIXME: menhir mocks (i.e melange-compiler-libs.0.0.1-414) ? we skip for now  *)
        String.is_suffix
          (Module.name md |> Module_name.to_string)
          ~suffix:"__mock"
      then libs
      else
        List.filter libs ~f:(fun lib ->
            let melange_mode =
              Lib_mode.Map.get (Lib.info lib |> Lib_info.modes) Lib_mode.Melange
            in
            let implements =
              Option.is_some (Lib_info.implements (Lib.info lib))
            in
            let local = Lib.Local.of_lib lib |> Option.is_none in
            (* Not filtering vlib implementations, vlibs, and melange mode *)
            let virtual_ = Option.is_some (Lib_info.virtual_ (Lib.info lib)) in
            if implements || virtual_ || local || melange_mode then true
            else
              let entry_module_names =
                (match Lib.Map.find lib_to_entry_modules_map lib with
                | Some modules -> modules
                | None -> [])
                |> List.map ~f:(fun m -> Module.name m)
              in
              if List.is_non_empty entry_module_names then
                List.exists entry_module_names ~f:(fun entry_module_name ->
                    (* FIXME: ocamldep doesn't see Melange_wrapper for files that
                        have been `copy_files` *)
                    if
                      String.equal "Melange_wrapper"
                        (Module_name.to_string entry_module_name)
                    then true
                    else if
                      not
                        (flag_open_present
                           (Module_name.to_string entry_module_name)
                           flags)
                    then
                      let top_c_modules =
                        match
                          Module_name.Map.find lib_top_module_map
                            entry_module_name
                        with
                        | Some modules -> modules
                        | None -> []
                      in
                      let keep =
                        (* First, check if one of the top closed modules matches any of [ocamldep] outputs *)
                        List.exists top_c_modules ~f:(fun top_c_mod ->
                            exists_in_odeps
                              (Module.name top_c_mod |> Module_name.to_string))
                        (* Secondly, for each [ocamldep] outut [X], see if current [entry_module_name] is in closure of [X]  *)
                        || List.exists dep_names ~f:(fun odep_output ->
                               let odep_module_name =
                                 Module_name.of_string odep_output
                               in
                               let top_c_modules =
                                 match
                                   Module_name.Map.find lib_top_module_map
                                     odep_module_name
                                 with
                                 | Some modules -> modules
                                 | None -> []
                               in
                               List.exists top_c_modules ~f:(fun top_c_mod ->
                                   Module_name.equal entry_module_name
                                     (Module.name top_c_mod)))
                      in
                      if not keep then (
                        Dune_util.Log.info
                          [ Pp.textf
                              "Removing %s aka %s for module %s \n\n\
                               ~Odep_list: %s\n\n\
                               ~Top_c_modules: %s\n\
                               ~Flags : %s\n\n\n\
                              \                                                          \
                               \n\n\
                              \                              \\n\n\
                              \                         \n\
                              \                         •\n\
                               ------------------"
                              (Lib.name lib |> Lib_name.to_string)
                              (Module_name.to_string entry_module_name)
                              (Module.name md |> Module_name.to_string)
                              (String.concat dep_names ~sep:" , ")
                              (List.map top_c_modules ~f:(fun m ->
                                   Module.name m |> Module_name.to_string)
                              |> String.concat ~sep:", ")
                              (String.concat flags ~sep:",")
                          ];
                        keep)
                      else (
                        Dune_util.Log.info
                          [ Pp.textf
                              "Keeping %s aka %s for module %s \n\n\
                               ~Odep_list: %s\n\n\
                               ~Top_c_modules: %s\n\
                               ~Flags : %s\n\n\n\
                              \                                                          \
                               \n\n\
                              \                              \\n\n\
                              \                         \n\
                              \                         •\n\
                               ------------------"
                              (Lib.name lib |> Lib_name.to_string)
                              (Module_name.to_string entry_module_name)
                              (Module.name md |> Module_name.to_string)
                              (String.concat dep_names ~sep:" , ")
                              (List.map top_c_modules ~f:(fun m ->
                                   Module.name m |> Module_name.to_string)
                              |> String.concat ~sep:", ")
                              (String.concat flags ~sep:",")
                          ];
                        keep)
                    else true)
              else true)

  let filter_ocamldep link_requires module_deps entry_names_closure md =
    let open Resolve.Memo.O in
    let* (module_deps, flags), _ = module_deps in
    let flag_open_present entry_lib_name =
      let rec help l =
        match l with
        | flag :: entry_name :: t ->
          if
            String.equal flag "-open"
            && String.is_prefix ~prefix:entry_lib_name entry_name
          then true
          else help (entry_name :: t)
        | _ -> false
      in
      help flags
    in
    let dep_names =
      List.map module_deps ~f:(fun mdep ->
          let open Module_dep in
          match mdep with
          (* Lib shadowing by a local module obliges
             us to also check if a lib is a local module *)
          | Local m -> Module.name m |> Module_name.to_string
          | External mname -> External_name.to_string mname)
    in
    ignore dep_names;
    ignore flag_open_present;
    ignore link_requires;
    ignore entry_names_closure;
    let not_filtrable lib =
      let melange_mode =
        Lib_mode.Map.get (Lib.info lib |> Lib_info.modes) Lib_mode.Melange
      in
      let implements = Option.is_some (Lib_info.implements (Lib.info lib)) in
      let local = Lib.Local.of_lib lib |> Option.is_none in

      let virtual_ = Option.is_some (Lib_info.virtual_ (Lib.info lib)) in
      melange_mode || implements || local || virtual_
    in
    let+ requires =
      Resolve.Memo.bind link_requires ~f:(fun lcs ->
          Resolve.Memo.List.map lcs ~f:(fun (lib, closure) ->
              let local_lib = Lib.Local.of_lib lib in
              if Option.is_none local_lib || not_filtrable lib then
                Resolve.Memo.return (Some (lib, closure))
              else
                let* (em : Module.t list) =
                  entry_names_closure (Option.value_exn local_lib)
                  |> Resolve.Memo.lift_memo
                in
                Dune_util.Log.info
                  [ Pp.textf "Entry names of %s %s"
                      (Lib.name lib |> Lib_name.to_string)
                      (List.map em ~f:(fun l ->
                           Module.name l |> Module_name.to_string)
                      |> String.concat ~sep:",")
                  ];
                let+ closure_names =
                  Resolve.Memo.List.fold_left closure ~init:[]
                    ~f:(fun acc libc ->
                      if not_filtrable libc then Resolve.Memo.return acc
                      else
                        let local_lib = Lib.Local.of_lib libc in
                        if Option.is_none local_lib then Resolve.Memo.return acc
                        else
                          let+ (em : Module.t list) =
                            entry_names_closure (Option.value_exn local_lib)
                            |> Resolve.Memo.lift_memo
                          in
                          Dune_util.Log.info
                            [ Pp.textf
                                "(Module:%s)Closure of (%s) gave lib (%s) \
                                 having entry names: (%s)\n"
                                (Module.name md |> Module_name.to_string)
                                (Lib.name lib |> Lib_name.to_string)
                                (Lib.name libc |> Lib_name.to_string)
                                (List.map em ~f:(fun l ->
                                     Module.name l |> Module_name.to_string)
                                |> String.concat ~sep:",")
                            ];
                          List.append acc em)
                in

                if List.is_empty em || List.is_empty closure_names then
                  Some (lib, closure)
                else
                  let module_names = List.append em closure_names in
                  if
                    (* Dune_util.Log.info
                       [ Pp.textf
                           "\n\
                            Gona see for lib %s if dep_names (%s) exits in{ %s}"
                           (Lib.name lib |> Lib_name.to_string)
                           (String.concat ~sep:"," dep_names)
                           (List.map module_names ~f:(fun l ->
                                Module.name l |> Module_name.to_string)
                           |> String.concat ~sep:",")
                       ]; *)
                    let ocamldep_output_exists_in_module_names =
                      List.exists dep_names ~f:(fun ocamldep_out ->
                          let ocamldep_out_mn =
                            Module_name.of_string ocamldep_out
                          in

                          List.exists module_names ~f:(fun a ->
                              let a = Module.name a in
                              flag_open_present (Module_name.to_string a)
                              || Module_name.equal a ocamldep_out_mn))
                    in

                    ocamldep_output_exists_in_module_names
                  then Some (lib, closure)
                  else (
                    Dune_util.Log.info
                      [ Pp.textf
                          "Removing lib %s for module %s\n\
                           Entry modules of lib : {%s} closure names of it \
                           [%s] ocamldeps names (%s)\n\n"
                          (Lib.name lib |> Lib_name.to_string)
                          (Module.name md |> Module_name.to_string)
                          (List.map em ~f:(fun l ->
                               Module.name l |> Module_name.to_string)
                          |> String.concat ~sep:",")
                          (List.map closure_names ~f:(fun l ->
                               Module.name l |> Module_name.to_string)
                          |> String.concat ~sep:";")
                          (String.concat dep_names ~sep:",")
                      ];
                    None)))
    in

    (* List.iter requires ~f:(fun ropt ->
        match ropt with
        | Some (lib, clos) ->
          Dune_util.Log.info
            [ Pp.textf "Some (lib,clos) = {%s} [%s]"
                (Lib.name lib |> Lib_name.to_string)
                (List.map clos ~f:(fun lib ->
                     Lib.name lib |> Lib_name.to_string)
                |> String.concat ~sep:",")
            ]
        | None -> ()); *)
    let requires = List.filter_opt requires in
    (*     let+ requires = Lib.uniq_linking_closure link_requires in
 *)
    let result =
      List.fold_left requires ~init:Lib.Set.empty ~f:(fun set (lib, closure) ->
          let set = Lib.Set.add set lib in
          let set =
            List.fold_left closure ~init:set ~f:(fun set lib ->
                Lib.Set.add set lib)
          in
          set)
    in
    Lib.Set.to_list result

  let make ?(lib_top_module_map = Resolve.Memo.return [])
      ?(lib_to_entry_modules_map = Resolve.Memo.return [])
      ?(direct_requires = Resolve.Memo.return []) ~link_requires
      ~compile_requires ?(entry_names_closure = fun _ -> Memo.return []) ()
      ~project ~opaque ~md ~dep_graphs ~flags =
    ignore direct_requires;
    ignore link_requires;
    ignore entry_names_closure;
    let flags =
      Action_builder.map2
        (Action_builder.map2
           (Ocaml_flags.get flags (Lib_mode.Ocaml Byte))
           (Ocaml_flags.get flags (Lib_mode.Ocaml Native))
           ~f:List.append)
        (Ocaml_flags.get flags Lib_mode.Melange)
        ~f:List.append
    in

    let open Lib_mode.Cm_kind.Map in
    let open Resolve.Memo.O in
    let iflags libs mode = Lib_flags.L.include_flags ~project libs mode in
    let deps =
      let dep_graph_impl = Ml_kind.Dict.get dep_graphs Ml_kind.Impl in
      let dep_graph_intf = Ml_kind.Dict.get dep_graphs Ml_kind.Intf in
      let module_deps_impl = Dep_graph.deps_of dep_graph_impl md in
      let module_deps_intf = Dep_graph.deps_of dep_graph_intf md in
      let cmb_itf_impl =
        Action_builder.map2 module_deps_impl module_deps_intf
          ~f:(fun inft impl -> List.append inft impl)
      in

      let cmb_flags =
        Action_builder.map2 cmb_itf_impl flags ~f:(fun mods map -> (mods, map))
      in
      Action_builder.run cmb_flags Action_builder.Eager
      |> Resolve.Memo.lift_memo
    in
    let flags =
      let dep_graph_impl = Ml_kind.Dict.get dep_graphs Ml_kind.Impl in
      let dep_graph_intf = Ml_kind.Dict.get dep_graphs Ml_kind.Intf in
      let module_deps_impl = Dep_graph.deps_of dep_graph_impl md in
      let module_deps_intf = Dep_graph.deps_of dep_graph_intf md in
      let cmb_itf_impl =
        Action_builder.map2 module_deps_impl module_deps_intf
          ~f:(fun inft impl -> List.append inft impl)
      in

      let cmb_flags =
        Action_builder.map2 cmb_itf_impl flags ~f:(fun mods map -> (mods, map))
      in
      Action_builder.run cmb_flags Action_builder.Eager
      |> Resolve.Memo.lift_memo
    in
    ignore flags;
    let requires =
      if Dune_project.implicit_transitive_deps project then
        filter_ocamldep link_requires flags entry_names_closure md
      else compile_requires
    in

    ignore deps;
    ignore lib_to_entry_modules_map;
    ignore lib_top_module_map;
    let make_includes_args ~mode groups =
      Command.Args.memo
        (Resolve.Memo.args
           (let+ libs = requires in
            (* let+ libs =
                 filter_with_odeps libs deps md lib_top_module_map
                   lib_to_entry_modules_map
               in *)
            Command.Args.S
              [ iflags libs mode
              ; Hidden_deps (Lib_file_deps.deps libs ~groups)
              ]))
    in
    let cmi_includes = make_includes_args ~mode:(Ocaml Byte) [ Ocaml Cmi ] in
    let cmx_includes =
      Command.Args.memo
        (Resolve.Memo.args
           (let+ libs = requires in
            (* let+ libs =
                 filter_with_odeps libs deps md lib_top_module_map
                   lib_to_entry_modules_map
               in *)
            Command.Args.S
              [ iflags libs (Ocaml Native)
              ; Hidden_deps
                  (if opaque then
                   List.map libs ~f:(fun lib ->
                       ( lib
                       , if Lib.is_local lib then
                           [ Lib_file_deps.Group.Ocaml Cmi ]
                         else [ Ocaml Cmi; Ocaml Cmx ] ))
                   |> Lib_file_deps.deps_with_exts
                  else
                    Lib_file_deps.deps libs
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

  let empty = Lib_mode.Cm_kind.Map.make_all Command.Args.empty
end

type opaque =
  | Explicit of bool
  | Inherit_from_settings

let eval_opaque (context : Context.t) = function
  | Explicit b -> b
  | Inherit_from_settings ->
    Profile.is_dev context.profile
    && Ocaml.Version.supports_opaque_for_mli context.ocaml.version

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
  ; requires_link : (Lib.t * Lib.t list) list Resolve.t Memo.Lazy.t
  ; includes : md:Module.t -> Includes.t
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

let requires_link t =
  Memo.Lazy.force t.requires_link |> Lib.uniq_linking_closure

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

let create ~super_context ~scope ~expander ~obj_dir ~modules ~flags
    ~(requires_compile : Lib.t list Resolve.Memo.t)
    ~(requires_link : (Lib.t * Lib.t list) list Resolve.t Memo.Lazy.t)
    ?(preprocessing = Pp_spec.dummy) ~opaque ?stdlib ~js_of_ocaml ~package
    ?public_lib_name ?vimpl ?modes ?bin_annot ?loc
    ?(lib_top_module_map = Resolve.Memo.return [])
    ?(lib_to_entry_modules_map = Resolve.Memo.return [])
    ?(entry_names_closure = fun _ -> Memo.return []) () =
  let open Memo.O in
  let project = Scope.project scope in

  let requires_compile =
    if Dune_project.implicit_transitive_deps project then
      Memo.Lazy.force requires_link
      |> Resolve.Memo.map ~f:(fun l ->
             List.map l ~f:(fun (_, a) -> a) |> List.concat)
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

  let includes =
    Includes.make ~project ~opaque ~dep_graphs
      ~link_requires:(Memo.Lazy.force requires_link)
      ~compile_requires:requires_compile ~lib_top_module_map
      ~lib_to_entry_modules_map ~direct_requires:requires_compile ~flags
      ~entry_names_closure ()
  in
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
  let keep_flags = Modules.is_stdlib_alias (modules t) alias_module in
  let flags =
    if keep_flags then
      (* in the case of stdlib, these flags can be written by the user *)
      t.flags
    else
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
    if Ocaml.Version.always_reads_alias_cmi ctx.ocaml.version then
      Sandbox_config.needs_sandboxing
    else Sandbox_config.no_special_requirements
  in
  let (modules, includes) : modules * (md:Module.t -> Includes.t) =
    match Modules.is_stdlib_alias t.modules.modules alias_module with
    | false -> (singleton_modules alias_module, fun ~md:_ -> Includes.empty)
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
  let flags =
    let project = Scope.project cctx.scope in
    let dune_version = Dune_project.dune_version project in
    let profile = (Super_context.context cctx.super_context).profile in
    Ocaml_flags.default ~profile ~dune_version
  in
  let opaque =
    (* Cmi's of link time generated modules are compiled with -opaque, hence
       their implementation must also be compiled with -opaque *)
    let ctx = Super_context.context cctx.super_context in
    Ocaml.Version.supports_opaque_for_mli ctx.ocaml.version
  in
  let modules = singleton_modules module_ in
  let dummy =
    Dep_graph.make ~dir:(Path.Build.of_string "")
      ~per_module:Module_name.Unique.Map.empty
  in
  let dep_graphs = Ml_kind.Dict.make ~intf:dummy ~impl:dummy in
  let requires_link =
    Resolve.Memo.map requires ~f:(List.map ~f:(fun r -> (r, [])))
  in
  let includes =
    Includes.make ~dep_graphs ~project:(Scope.project cctx.scope) ~opaque
      ~link_requires:requires_link ~compile_requires:requires ~flags ()
  in
  { cctx with
    opaque
  ; flags = Ocaml_flags.empty
  ; requires_link = Memo.lazy_ (fun () -> requires_link)
  ; requires_compile = requires
  ; includes
  ; modules
  }

let for_wrapped_compat t =
  { t with includes = (fun ~md:_ -> Includes.empty); stdlib = None }

let for_plugin_executable t ~embed_in_plugin_libraries =
  let libs = Scope.libs t.scope in
  let requires_link =
    Memo.lazy_ (fun () ->
        Resolve.Memo.List.map
          ~f:(fun l ->
            Lib.DB.resolve libs l |> Resolve.Memo.map ~f:(fun l -> (l, [])))
          embed_in_plugin_libraries)
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
