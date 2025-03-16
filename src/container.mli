
include Ooh_intf.Container

module Private : sig
  val create : 'cty -> (_, 'cty) t
end
