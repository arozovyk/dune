open Import

module Predicate_with_id = struct
  open Stdune
  open Dune_sexp

  type 'a t =
    { id : Dyn.t Lazy.t
    ; f : 'a Predicate.t
    }

  let compare x y = Dyn.compare (Lazy.force x.id) (Lazy.force y.id)

  let equal x y = compare x y = Ordering.Eq

  let hash t = Dyn.hash (Lazy.force t.id)

  let to_dyn t =
    let open Dyn in
    Record [ ("id", Lazy.force t.id) ]

  let encode _ = Encoder.string "predicate <opaque>"

  let create ~id ~f = { id; f = Predicate.create f }

  let true_ = { id = lazy (String "true_"); f = Predicate.true_ }

  let false_ = { id = lazy (String "false_"); f = Predicate.false_ }

  let test t e = Predicate.test t.f e
end

type t =
  { dir : Path.t
  ; predicate : Filename.t Predicate_with_id.t
  ; only_generated_files : bool
  ; name : string
  }

let dir t = t.dir

let predicate t = t.predicate

let only_generated_files t = t.only_generated_files

let compare { dir; predicate; only_generated_files; name } t =
  let open Ordering.O in
  let= () = Path.compare dir t.dir in
  let= () = Predicate_with_id.compare predicate t.predicate in
  let= () = String.compare name t.name in
  Bool.compare only_generated_files t.only_generated_files

let create ~dir ?(only_generated_files = false) predicate name =
  { dir; predicate; only_generated_files; name }

let name t = t.name

let of_glob ~dir glob name =
  let id = lazy (Glob.to_dyn glob) in
  create ~dir (Predicate_with_id.create ~id ~f:(Glob.test glob)) name

let to_dyn { dir; predicate; only_generated_files; name } =
  Dyn.Record
    [ ("dir", Path.to_dyn dir)
    ; ("predicate", Predicate_with_id.to_dyn predicate)
    ; ("only_generated_files", Bool only_generated_files)
    ; ("name", String name)
    ]

let encode { dir; predicate; only_generated_files; name } =
  let open Dune_lang.Encoder in
  record
    [ ("dir", Dpath.encode dir)
    ; ("predicate", Predicate_with_id.encode predicate)
    ; ("only_generated_files", bool only_generated_files)
    ; ("name", string name)
    ]

let equal x y = compare x y = Eq

let hash { dir; predicate; only_generated_files; name } =
  Tuple.T3.hash Path.hash Predicate_with_id.hash
    (Tuple.T2.hash Bool.hash String.hash)
    (dir, predicate, (only_generated_files, name))

let test t path = Predicate_with_id.test t.predicate (Path.basename path)
