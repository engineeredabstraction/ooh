(* SPDX-FileCopyrightText: Copyright (C) 2025 Stefan Muenzel
 * SPDX-License-Identifier: MPL-2.0
 *)

type ('container, 'value) t = int

module Unsafe = struct
  external get : ('container, 'value) t -> 'value = "%int_as_pointer"  
end

let get
    (type container value container_raw)
    (_container :(container, container_raw) Container.t)
    (t : (container, value) t)
  : value =
  Unsafe.get t

module Prefix = struct
  let ( !> ) t = get Container.permanent t
  let ( #>) = get
end

module Permanent = struct
  let get t = get Container.permanent t
end

module Obj = struct
  let set_tag _t _tag = assert false
end
