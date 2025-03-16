
type ('vs, 'v) t =
  | V00 : (< v00 : 'v00; ..>, 'v00) t
  | V01 : (< v01 : 'v01; ..>, 'v01) t
  | V10 : (< v10 : 'v10; ..>, 'v10) t
  | V11 : (< v11 : 'v11; ..>, 'v11) t

(* TODO: Check if this compiles to the identity *)
let as_int
    (type vs v)
    (witness : (vs, v) t)
  : int =
  match witness with
  | V00 -> 0b00
  | V01 -> 0b01
  | V10 -> 0b10
  | V11 -> 0b11
