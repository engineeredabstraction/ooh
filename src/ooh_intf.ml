(* SPDX-FileCopyrightText: Copyright (C) 2025 Stefan Muenzel
 * SPDX-License-Identifier: MPL-2.0
 *)

module type Container = sig
  type ('container, 'cty) t

  val permanent : (unit, unit) t

  val get : (_, 'cty) t -> 'cty

  module Packed : sig
    type ('container, 'cty) t' := ('container, 'cty) t

    type 'cty t = | T : ('container, 'cty) t' -> 'cty t
  end
end

module type Ext = sig
  module Container : Container

  type ('container, 'value) t [@@immediate] [@@deriving typerep]

  val get : ('container, _) Container.t -> ('container, 'value) t -> 'value

  module Prefix : sig
    val ( !> ) : (unit, 'value) t -> 'value
    val ( #> ) : ('container, _) Container.t -> ('container, 'value) t -> 'value
  end

  module Unsafe : sig
    val get : ('container, 'value) t -> 'value
    val as_int : ('container, 'value) t -> int
    val of_int : int -> ('container, 'value) t
  end

  module Permanent : sig
    val get : (unit, 'value) t -> 'value
  end

  module Obj : sig
    val set_tag : ('container, 'value) t -> int -> unit
  end
end

module type S = sig

  module Container : Container

  module Ext : Ext with module Container := Container

  module Nothing : sig
    type t = |
  end

  module Witness : sig
    type ('vs, 'v) t =
      | V00 : (< v00 : 'v00; ..>, 'v00) t
      | V01 : (< v01 : 'v01; ..>, 'v01) t
      | V10 : (< v10 : 'v10; ..>, 'v10) t
      | V11 : (< v11 : 'v11; ..>, 'v11) t
  end

  module rec Encoded : sig
    type 'v t [@@immediate]

    module type Encoder = sig
      type value

      val encode_exn : value -> value t
      val unchecked_encode : value -> value t
      val decode : value t -> value
    end

    module Int61 : Encoder with type value = int

    module Unit : sig
      include Encoder with type value = unit
      val unit : unit t
    end

    module Ext : sig
      val encode : (('container, 'value) Ext.t as 'v) -> 'v t
      val decode : (('container, 'value) Ext.t as 'v) t -> 'v
    end

    module Raw : sig
      val to_int : 'v t -> int
    end
  end

  and Multi_valued : sig
    module Raw : sig
      type ('vs, 'v) t [@@immediate]

      val kind : ('vs, 'v) t -> ('vs, 'v) Witness.t
    end

    type 'vs t =
      | T 
        : ('vs, 'v) Raw.t
          -> 'vs t
    [@@unboxed] [@@immediate]

    val create
      :  'v Encoded.t
      -> ('vs, 'v) Witness.t
      -> 'vs t

    module Option0 : sig
      type 'value t = < v00 : 'value; v01 : unit > Multi_valued.t

      module Optional_syntax : sig
        module Optional_syntax : sig
          val is_none : _ t -> bool
          val unsafe_value : 'value t -> 'value
        end
      end
    end
  end

  and Pool : sig
    module Raw : sig
      type t
    end

    type 'container t = ('container, Raw.t) Container.t

    type 'ret create 
    := element_size:[`Words of int | `Typerep of Typerep_lib.Std.Typerep.packed]
      -> ?initial_size:int
      -> unit
      -> 'ret

    val create : Raw.t Container.Packed.t create
    val create_permanent : unit t create

    module Allocator : sig
      type 'container t' := 'container t
      type ('container, 'value) t

      val create
        :  'container t'
        -> 'value Typerep_lib.Std.Typerep.t
        -> template:'value
        -> ('container, 'value) t option

      val create_exn 
        :  'container t'
        -> 'value Typerep_lib.Std.Typerep.t
        -> template:'value
        -> ('container, 'value) t

      val alloc_unitialized : ('container, 'value) t -> ('container, 'value) Ext.t
      val alloc : ('container, 'value) t -> ('container, 'value) Ext.t
    end

    val free : 'container t -> ('container, 'value) Ext.t -> unit
  end

end
