module External_name = struct
  type t = string

  let of_string n = n

  let to_string t = t
end

type t =
  | Local of Module.t
  | External of External_name.t

let to_local m = Local m

let to_external m_name = External m_name

let is_local t =
  match t with
  | Local _ -> true
  | _ -> false

let is_external t =
  match t with
  | External _ -> true
  | _ -> false

let filter_local t =
  match t with
  | Local m -> Some m
  | _ -> None

let filter_external t =
  match t with
  | External m_name -> Some m_name
  | _ -> None

let compare a b =
  match (a, b) with
  | External a_name, External b_name -> Stdune.String.compare a_name b_name
  | Local a, Local b -> Module_name.compare (Module.name a) (Module.name b)
  | Local _, External _ -> Ordering.Lt
  | External _, Local _ -> Ordering.Gt
