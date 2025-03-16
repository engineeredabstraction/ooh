open Typerep_lib.Std
open! Ooh.Ext.Prefix

type t0 =
  { mutable i0 : int
  ; mutable i1 : int
  } [@@deriving typerep, fields]

let%expect_test "" =
  let T pool =
    Ooh.Pool.create
      ~element_size:(`Typerep (Typerep.T [%typerep_of: t0]))
      ()
  in
  let allocator =
    Ooh.Pool.Allocator.create_exn
      pool
      ~template:{i0 = 0; i1 = 0}
      [%typerep_of: t0]
  in
  let v_ptr = Ooh.Pool.Allocator.alloc_unitialized allocator in
  Fields_of_t0.Direct.set_all_mutable_fields
    pool#>v_ptr
    ~i0:7
    ~i1:8;
  ()

