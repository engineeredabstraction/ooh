

type ('container, 'cty) t = 'cty

let permanent = ()

let get (t : _ t) = t

module Packed = struct
  type ('container, 'cty) t' = ('container, 'cty) t

  type 'cty t = | T : ('container, 'cty) t' -> 'cty t
end

module Private = struct
  let create t = t
end
