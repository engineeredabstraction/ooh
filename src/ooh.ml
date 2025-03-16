(* SPDX-FileCopyrightText: Copyright (C) 2025 Stefan Muenzel
 * SPDX-License-Identifier: MPL-2.0
 *)

module Nothing = Nothing

module Witness = Witness

module Container = Container

module Ext = Ext

module Encoded = struct
  type 'v t = 'v Encoded.t

  module type Encoder = sig
    type value

    val encode_exn : value -> value t
    val unchecked_encode : value -> value t
    val decode : value t -> value
  end

  module Int61 = struct
    type value = int

    let encode_exn (x : value) : value t =
      (* TODO: Check bounds *)
      Encoded.Int61_high_bits.encode_exn (x lsl 2)

    let unchecked_encode (x : value) : value t =
      Encoded.Int61_high_bits.unchecked_encode (x lsl 2)

    let decode (x : value t) : value =
      (Encoded.Int61_high_bits.decode x) asr 2
  end

  module Unit = struct
    type value = unit

    let encode_exn (() : value) : value t =
      Encoded.Int61_high_bits.unchecked_encode 0
      |> Encoded.Private.unsafe_create

    let unchecked_encode (() : value) : value t =
      encode_exn ()

    let decode (_x : value t) : value = ()

    let unit = encode_exn ()
  end

  module Ext = struct

    let encode (type container value) (e : (container, value) Ext.t) : (container, value) Ext.t t =
      Ext.Unsafe.as_int e
      |> Encoded.Int61_high_bits.unchecked_encode
      |> Encoded.Private.unsafe_create 

    let decode (type container value) (e : (container, value) Ext.t t) : (container, value) Ext.t =
      Encoded.Int61_high_bits.decode (Encoded.Private.unsafe_create e)
      |> Ext.Unsafe.of_int
  end

  module Raw = Encoded.Raw
end

module Multi_valued = Multi_valued

module Pool = struct
  module Raw = Pool

  type 'container t = ('container, Raw.t) Container.t

  let element_wosize element_size =
    match element_size with
    | `Words w -> w
    | `Typerep (Typerep_lib.Std.Typerep.T tr) ->
      match Type_properties.ext_size tr with
      | None -> failwith "No size for type"
      | Some size -> size

  let create ~element_size ?initial_size () =
    let element_wosize = element_wosize element_size in
    Pool.create ~element_wosize ?initial_size ()
    |> Container.Private.create
    |> Container.Packed.T

  let permanent = ref []

  let create_permanent ~element_size ?initial_size () =
    let element_wosize = element_wosize element_size in
    let v = Pool.create ~element_wosize ?initial_size () in
    permanent := v :: !permanent;
    Container.Private.create v

  let free container v =
    Raw.free (Container.get container)
      (Raw.Encoded_pointer.Unsafe.of_int (Ext.Unsafe.as_int v))

  module Allocator = struct
    type ('container, 'value) t =
      { container : ('container, Raw.t) Container.t
      ; template : 'value
      }

    let create container typerep ~template =
      match Type_properties.ext_size typerep with
      | None -> None
      | Some ext_size ->
        if Raw.block_wosize (Container.get container) < ext_size
        then None
        else begin
          { container
          ; template
          } |> Some
        end

    let create_exn container typerep ~template =
      match create container typerep ~template with
      | None -> failwith "Pool.Allocator.create_exn"
      | Some v -> v

    let alloc_unitialized { container; template = _} =
      Raw.alloc (Container.get container)
      |> Raw.Encoded_pointer.as_int
      |> Ext.Unsafe.of_int

    let alloc _ = assert false

    let alloc_with_tag _ = assert false
  end

end
