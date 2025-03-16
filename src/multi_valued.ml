
module Raw : sig
  type ('v00, 'v01, 'v10, 'v11, 'v) t [@@immediate]

  val kind : ('v00, 'v01, 'v10, 'v11, 'v) t -> ('v00, 'v01, 'v10, 'v11, 'v) Witness.t

  val create
    :  'v Encoded.t
    -> ('v00, 'v01, 'v10, 'v11, 'v) Witness.t
    -> ('v00, 'v01, 'v10, 'v11, 'v) t

  val get : ('v00, 'v01, 'v10, 'v11, 'v) t -> 'v
end = struct
  type ('v00, 'v01, 'v10, 'v11, 'v) t = int

  let kind
      (type v00 v01 v10 v11 v)
      (t : (v00, v01, v10, v11, v) t)
    : (v00, v01, v10, v11, v) Witness.t
    =
    Obj.magic (t land 0b11)

  let create
      (type v00 v01 v10 v11 v)
      (encoded : v Encoded.t)
      (witness : (v00, v01, v10, v11, v) Witness.t)
    : (v00, v01, v10, v11, v) t
    =
    (Witness.as_int witness) lor (Encoded.Raw.to_int encoded)

  let get 
      (type v00 v01 v10 v11 v)
      (t : (v00, v01, v10, v11, v) t)
    : v
    =
    Obj.magic (t land (lnot 0b11))
end

type ('v00, 'v01, 'v10, 'v11) t =
  | T 
    : ('v00, 'v01, 'v10, 'v11, 'v) Raw.t
      -> ('v00, 'v01, 'v10, 'v11) t
[@@unboxed] [@@immediate]

let create
    (type v00 v01 v10 v11 v)
    (encoded : v Encoded.t)
    (witness : (v00, v01, v10, v11, v) Witness.t)
  : (v00, v01, v10, v11) t
  =
  T (Raw.create encoded witness)

