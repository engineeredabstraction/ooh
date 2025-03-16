(* SPDX-FileCopyrightText: Copyright (C) 2025 Stefan Muenzel
 * SPDX-License-Identifier: MPL-2.0
 *)

module Encoded_pointer : sig
  type t [@@immediate]

  val offset_words : t -> int -> t
  val deref : t -> Obj.t
  val as_int : t -> int

  module Optional : sig
    type t' := t
    type t [@@immediate]

    val none : t
    val some : t' -> t
    module Optional_syntax : sig
      module Optional_syntax : sig
        val is_none : t -> bool
        val unsafe_value : t -> t'
      end
    end
  end

  val get_int_field : t -> int -> int
  val set_int_field : t -> int -> int -> unit

  val get_ptr_field : t -> int -> Optional.t
  val set_ptr_field : t -> int -> Optional.t -> unit

  module Optional_syntax = Optional.Optional_syntax

  module Unsafe : sig
    val of_int : int -> t
  end
end = struct
  type t = int

  let offset_words t i =
    t + (i * ((Sys.word_size / 8) / 2))

  external deref : t -> Obj.t = "%int_as_pointer"

  let as_int t = t

  module Optional = struct
    type t = int

    let none = 0
    let some t = t

    module Optional_syntax = struct
      module Optional_syntax = struct
        let is_none t = t = 0
        let unsafe_value t = t
      end
    end
  end

  let get_int_field t i =
    let ar : int array = Obj.obj (deref t) in
    Array.unsafe_get ar i

  let set_int_field t i v =
    let ar : int array = Obj.obj (deref t) in
    Array.unsafe_set ar i v

  let get_ptr_field = get_int_field
  let set_ptr_field = set_int_field

  module Optional_syntax = Optional.Optional_syntax

  module Unsafe = struct
    let of_int i = i
  end
end

module Pool_section : sig
  type t

  val create : block_wosize:int -> block_count:int -> t

  val block_wosize : t -> int
  val block_count : t -> int

  val entry : t -> int -> Encoded_pointer.t
end = struct

  external pool_alloc : block_wosize:int -> block_count:int -> Encoded_pointer.t = "ooh_pool_alloc"
  external pool_free : Encoded_pointer.t -> unit = "ooh_pool_free"

  type t =
    { pointer : Encoded_pointer.t
    ; block_wosize : int
    ; block_count : int
    }

  let create ~block_wosize ~block_count =
    assert (block_wosize >= 1);
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

type t =
  { mutable section_list : Pool_section.t list
  ; mutable free_list : Encoded_pointer.Optional.t
  ; mutable total_blocks : int
  ; block_wosize : int
  ; block_count : int
  }

let block_wosize t = t.block_wosize

let init_section section block_count =
  for i = 1 to block_count - 1 do
    let entry = Pool_section.entry section i in
    let prev_entry = Pool_section.entry section (i - 1) in
    Encoded_pointer.set_int_field prev_entry 0 (Encoded_pointer.as_int entry)
  done

let create
    ~element_wosize
    ?(initial_size = 64)
    ()
  =
  let block_wosize = element_wosize in
  let block_count = initial_size in
  let section = Pool_section.create ~block_wosize ~block_count in
  let section_list = [ section ] in
  let free_list = Encoded_pointer.Optional.some (Pool_section.entry section 0) in
  init_section section block_count;
  { section_list; free_list; block_wosize; block_count; total_blocks = block_count }

let alloc t =
  let entry =
    match%optional.Encoded_pointer t.free_list with
    | None ->
      let new_section =
        Pool_section.create ~block_wosize:t.block_wosize ~block_count:t.block_count
      in
      init_section new_section t.block_count;
      t.section_list <- new_section :: t.section_list;
      let ptr = Pool_section.entry new_section 0 in
      t.free_list <- Encoded_pointer.Optional.some ptr;
      t.total_blocks <- t.total_blocks + t.block_count;
      ptr
    | Some ptr -> ptr
  in
  let new_free_list = Encoded_pointer.get_ptr_field entry 0 in
  t.free_list <- new_free_list;
  entry

let free t entry =
  Encoded_pointer.set_ptr_field entry 0 t.free_list;
  t.free_list <- Encoded_pointer.Optional.some entry
