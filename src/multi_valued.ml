
module Raw : sig
  type ('vs, 'v) t [@@immediate]

  val kind : ('vs, 'v) t -> ('vs, 'v) Witness.t

  val create
    :  'v Encoded.t
    -> ('vs, 'v) Witness.t
    -> ('vs, 'v) t

  val get : ('vs, 'v) t -> 'v
end = struct
  type ('vs, 'v) t = int

  let kind
      (type vs v)
      (t : (vs, v) t)
    : (vs, v) Witness.t
    =
    Obj.magic (t land 0b11)

  let create
      (type vs v)
      (encoded : v Encoded.t)
      (witness : (vs, v) Witness.t)
    : (vs, v) t
    =
    (Witness.as_int witness) lor (Encoded.Raw.to_int encoded)

  let get 
      (type vs v)
      (t : (vs, v) t)
    : v
    =
    Obj.magic (t land (lnot 0b11))
end

type 'vs t =
  | T 
    : ('vs, 'v) Raw.t
      -> 'vs t
[@@unboxed] [@@immediate]

let create
    (type vs v)
    (encoded : v Encoded.t)
    (witness : (vs, v) Witness.t)
  : vs t
  =
  T (Raw.create encoded witness)

module Option0 = struct
  type nonrec 'value t = < v00 : 'value; v01 : unit > t

  module Optional_syntax = struct
    module Optional_syntax = struct
      let is_none (type value) (T t : value t) : bool =
        match Raw.kind t with
        | V00 -> true
        | V01 -> false

      let unsafe_value (type value) (T t : value t) : value =
        Raw.get ((Obj.magic t) : (< v00 : value; .. >, value) Raw.t)
    end
  end
end
