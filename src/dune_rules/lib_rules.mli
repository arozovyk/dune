open Import
open Dune_file

val foreign_rules :
     Foreign.Library.t
  -> sctx:Super_context.t
  -> expander:Expander.t
  -> dir:Path.Build.t
  -> dir_contents:Dir_contents.t
  -> unit Memo.t

val compile_info :
     ?dep_graphs:Dep_graph.t Ml_kind.Dict.t
  -> ?modules:Modules.t
  -> Library.t
  -> Scope.t
  -> (Lib.t * Lib.Compile.t) Memo.t

val rules :
     ?lib_to_entry_modules_map:(Lib.t * Module.t list) list Resolve.Memo.t
  -> ?lib_top_module_map:
       (Module_name.t * Module.t list) list list Resolve.Memo.t
  -> Library.t
  -> sctx:Super_context.t
  -> dir_contents:Dir_contents.t
  -> dir:Path.Build.t
  -> expander:Expander.t
  -> scope:Scope.t
  -> (Compilation_context.t * Merlin.t) Memo.t
