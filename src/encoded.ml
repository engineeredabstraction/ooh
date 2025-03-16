
type 'v t = int [@@immediate]

module Int61_high_bits = struct
  type value = int
  
  let unchecked_encode (x : value) : value t = x

  let encode_exn (x : value) : value t =
    if x land 3 <> 0 then raise (Invalid_argument "x land 3 <> 0") else x

  let decode (x : value t) : value = x
end

module Raw = struct
  let to_int (x : _ t) : int = x
end
