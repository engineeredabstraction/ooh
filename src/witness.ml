
type ('v00, 'v01, 'v10, 'v11, 'v) t =
  | V00 : ('v00, _, _, _, 'v00) t
  | V01 : (_, 'v01, _, _, 'v01) t
  | V10 : (_, _, 'v10, _, 'v10) t
  | V11 : (_, _, _, 'v11, 'v11) t

(* TODO: Check if this compiles to the identity *)
let as_int
    (type v00 v01 v10 v11 v)
    (witness : (v00, v01, v10, v11, v) t)
  : int =
  match witness with
  | V00 -> 0
  | V01 -> 1
  | V10 -> 2
  | V11 -> 3
