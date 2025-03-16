(* SPDX-FileCopyrightText: Copyright (C) 2025 Stefan Muenzel
 * SPDX-License-Identifier: MPL-2.0
 *)

include Ooh_intf.Container

module Private : sig
  val create : 'cty -> (_, 'cty) t
end
