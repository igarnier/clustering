open Owl
open Clustering

module Elt =
struct

  type t = Vec.vec

  let dist v1 v2 =
    let v1x = Vec.get v1 0 and v1y = Vec.get v1 1
    and v2x = Vec.get v2 0 and v2y = Vec.get v2 1 in
    sqrt ((v1x -. v2x) ** 2. +. (v1y -. v2y) ** 2.)

  let mean vec_array =
    let sum = Array.fold_left Vec.add (Vec.zeros 2) vec_array in
    Vec.mul_scalar sum (1. /. (float (Array.length vec_array)))
  
end

let mean0 =
  Vec.of_array [| 1.0; ~-. 0.5 |]

let mean1 =
  Vec.of_array [| 2.5;  2.0 |]

let mean2 =
  Vec.of_array [| ~-. 1.5;  0.0 |]

let dataset0 =
  Array.init 60 (fun _ -> Vec.add mean0 (Vec.gaussian 2))

let dataset1 =
  Array.init 60 (fun _ -> Vec.add mean1 (Vec.gaussian 2))

let dataset2 =
  Array.init 60 (fun _ -> Vec.add mean2 (Vec.gaussian 2))

let dataset = Array.concat [dataset0; dataset1; dataset2]

let matrix_of_dataset dataset =
  Mat.concatenate dataset

let plot =
  let h  = Plot.create "dataset.png" in
  Plot.set_background_color h 255 255 255;

  let ds = matrix_of_dataset dataset0 in
  let c1 = Mat.col ds 0 in
  let c2 = Mat.col ds 1 in
  Plot.(scatter ~h ~spec:[ RGB (255,0,0) ] c1 c2);

  let ds = matrix_of_dataset dataset1 in
  let c1 = Mat.col ds 0 in
  let c2 = Mat.col ds 1 in
  Plot.(scatter ~h ~spec:[ RGB (0,255,0) ] c1 c2);

  let ds = matrix_of_dataset dataset2 in
  let c1 = Mat.col ds 0 in
  let c2 = Mat.col ds 1 in
  Plot.(scatter ~h ~spec:[ RGB (0,0,255) ] c1 c2);

  Plot.output h

let classes = 3

(* Test k-medoids *)
let _ =
  let module K = K_medoids.Make(Elt) in
  let [| cl0; cl1; cl2 |] =
    let result =
      K.k_medoids
        ~precompute:false
        ~elements:dataset
        ~k:classes
        ~algorithm:K.PAM
        ~init:K.KmedoidsPP
        ~threshold:0.1
    in
    if Array.length result != classes then
      failwith "bug found"
    else
      [| result.(0); result.(1); result.(2) |]
  in
  let h  = Plot.create "result_kmedoids.png" in
  Plot.set_background_color h 255 255 255;
  let ds = matrix_of_dataset cl0 in
  let c1 = Mat.col ds 0 in
  let c2 = Mat.col ds 1 in
  Plot.(scatter ~h ~spec:[ RGB (255,0,0) ] c1 c2);
  let ds = matrix_of_dataset cl1 in
  let c1 = Mat.col ds 0 in
  let c2 = Mat.col ds 1 in
  Plot.(scatter ~h ~spec:[ RGB (0,255,0) ] c1 c2);
  let ds = matrix_of_dataset cl2 in
  let c1 = Mat.col ds 0 in
  let c2 = Mat.col ds 1 in
  Plot.(scatter ~h ~spec:[ RGB (0,0,255) ] c1 c2);
  Plot.output h

(* Test k-medoids *)
let _ =
  let module K = K_means.Make(Elt) in
  let [| cl0; cl1; cl2 |] =
    let result =
      K.k_means
        ~elements:dataset
        ~k:classes
        ~init:K.KmeansPP
        ~threshold:0.1
    in
    if Array.length result != classes then
      failwith "bug found"
    else
      [| result.(0); result.(1); result.(2) |]
  in
  let h  = Plot.create "result_kmeans.png" in
  Plot.set_background_color h 255 255 255;
  let ds = matrix_of_dataset cl0 in
  let c1 = Mat.col ds 0 in
  let c2 = Mat.col ds 1 in
  Plot.(scatter ~h ~spec:[ RGB (255,0,0) ] c1 c2);
  let ds = matrix_of_dataset cl1 in
  let c1 = Mat.col ds 0 in
  let c2 = Mat.col ds 1 in
  Plot.(scatter ~h ~spec:[ RGB (0,255,0) ] c1 c2);
  let ds = matrix_of_dataset cl2 in
  let c1 = Mat.col ds 0 in
  let c2 = Mat.col ds 1 in
  Plot.(scatter ~h ~spec:[ RGB (0,0,255) ] c1 c2);
  Plot.output h

