
type 'v t [@@immediate]

module Int61_high_bits : sig
  type value = int

  val unchecked_encode : value -> value t
  val encode_exn : value -> value t
  val decode : value t -> value
end

module Raw : sig
  val to_int : _ t -> int
end
