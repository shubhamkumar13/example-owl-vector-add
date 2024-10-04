[@@@warning "-32-34-16-27"]

open Owl
open Rresult.R

let ( let* )  a b =  a >>= b

type ('a, 'b) result = ('a, 'b) Rresult.R.t

module Arr = Dense.Ndarray.Generic

let mk ~row_major_dim_list ~arr_type ?elt ~dim =
  (* enforce explicit check to make sure dimensions are right *)
  let f fn = 
    if List.length row_major_dim_list == dim then
      ok fn
    else
      Error "The number of elements in the size list is not equal to the dimensions that is expected to generate the Ndarray"
  in
  match elt with
  | Some elt -> f @@ Arr.create arr_type (Array.of_list row_major_dim_list) elt 
  | None -> f @@ Arr.empty arr_type (Array.of_list row_major_dim_list)

let arr = 
  let row_major_dim_list = [2; 4] in
  let arr_type = Bigarray.Genarray.kind @@ Mat.vector_zeros 2 in
  let elt = 0. in
  let dim = 2 in
  mk ~row_major_dim_list ~arr_type ~elt ~dim

let update_col ~(coords:(('a * (int * int)) list)) arr:('a, 'b) Arr.t =
  let b = Arr.init Bigarray.Float64 [|2; 1|] (fun _ -> 1.) in 
  let f a = 
    Arr.pp_dsnda Format.std_formatter a;
    Arr.pp_dsnda Format.std_formatter b;
    Arr.add a b in
  Mat.map_by_col 0 f arr
  

  

let _ = 
  let* arr = arr in 
  update_col ~coords:[(1., (0, 0))] arr |> fun arr -> 
  ok @@ Arr.pp_dsnda Format.std_formatter arr