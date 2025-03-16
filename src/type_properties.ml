(* SPDX-FileCopyrightText: Copyright (C) 2025 Stefan Muenzel
 * SPDX-License-Identifier: MPL-2.0
 *)

open Typerep_lib.Std

let is_word (type v) (t : v Typerep.t) : bool =
  match Typerep.head t with
  | Int -> true
  | Char -> true
  | Bool -> true
  | Unit -> true
  | Variant v ->
    Typerep.Variant.fold
      v
      ~init:true
      ~f:(fun acc (Tag tag) ->
          if Typerep.Tag.arity tag = 0 then acc else false
        )
  | _ -> false

let ext_size (type v) (t : v Typerep.t) : int option =
  match Typerep.head t with
  | Array _ -> None
  | Int -> None
  | Int32 -> None
  | Int64 -> None
  | Nativeint -> None
  | Float -> None
  | Char -> None
  | String -> None
  | Option _ -> None
  | List _ -> None
  | Bytes -> None
  | Bool -> None
  | Unit -> None
  | Lazy _ -> None
  | Ref f when is_word f -> Some 1
  | Ref _ -> None
  | Function _ -> None
  | Tuple _ -> None
  | Variant _ -> None
  | Named _ -> None

  | Record r -> 
    if Typerep.Record.has_double_array_tag r
    then Some (Typerep.Record.length r)
    else begin
      Typerep.Record.fold
        r
        ~init:(Some 0)
        ~f:(fun size (Field field) ->
            match size with
            | None -> None
            | Some size ->
              if not (Typerep.Field.is_mutable field)
              then None
              else if is_word (Typerep.Field.traverse field)
              then Some (size + 1)
              else None
          )
    end


