
module Encoded_pointer : sig
  type t [@@immediate]

  val offset_words : t -> int -> t

  val deref : t -> Obj.t
end = struct
  type t = int

  let offset_words t i =
    t + (i * ((Sys.word_size / 8) / 2))

  external deref : t -> Obj.t = "%int_as_pointer"
end

module Pool_section : sig
  type t

  val create : block_wosize:int -> block_count:int -> t

  val block_wosize : t -> int
  val block_count : t -> int

  val entry : t -> int -> Encoded_pointer.t
end = struct

  external pool_alloc : block_wosize:int -> block_count:int -> Encoded_pointer.t = "caml_pool_alloc"
  external pool_free : Encoded_pointer.t -> unit = "caml_pool_free"

  type t =
    { pointer : Encoded_pointer.t
    ; block_wosize : int
    ; block_count : int
    }

  let create ~block_wosize ~block_count =
    let pointer = pool_alloc ~block_wosize ~block_count in
    let result =  { pointer; block_wosize; block_count } in
    Gc.finalise
      (fun t ->
         pool_free t.pointer
      )
      result;
    result

  let block_wosize t = t.block_wosize
  let block_count t = t.block_count

  let entry t i = 
    Encoded_pointer.offset_words t.pointer (i * (t.block_wosize + 1) + 1)
end
