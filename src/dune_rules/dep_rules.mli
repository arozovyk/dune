(** Get dependencies for a set of modules using either ocamldep or ocamlobjinfo *)

open Import

val for_module :
     Ocamldep.Modules_data.t
  -> Module.t
  -> bool
  -> Module_dep.t list Action_builder.t Ml_kind.Dict.t Memo.t

val immediate_deps_of :
     Module.t
  -> Modules.t
  -> Path.Build.t Obj_dir.t
  -> Ml_kind.t
  -> Module_dep.t list Action_builder.t

val rules :
     Ocamldep.Modules_data.t
  -> implicit_transitive_deps:bool
  -> Dep_graph.t Ml_kind.Dict.t Memo.t
