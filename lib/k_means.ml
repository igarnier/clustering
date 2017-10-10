open Batteries

(* We require elements to have the structure of a
   metric space and to support the computing of geometric means. *)
module type Element =
sig

  type t

  (* This should be a proper distance function (symmetric, zero on the diagonal,
     verifying the triangular inequality). *)
  val dist : t -> t -> float

  val mean : t array -> t
  
end

module Make(E : Element) =
struct

  type elt = E.t

  type init =
    | Forgy           (* Selects k elements at random (without replacement) *)
    | RandomPartition (* Select a random partition *)
    | KmeansPP        (* K-means++ *)

  exception KmeansError of string

  let max : float -> float -> float =
    fun x y ->
      if x > y then x
      else y
  
  let closest centroids elt =
    let m = ref 0  in
    let d = ref max_float in
    for i = 0 to Array.length centroids - 1 do
      let dist = E.dist elt centroids.(i) in
      if dist < !d then
        (d := dist; m := i)
    done;
    !m, !d
  
  let compute_classes centroids elements =
    let classes : elt list array =
      Array.create (Array.length centroids) []
    in
    Array.iter (fun elt ->
        let k, _ = closest centroids elt in
        classes.(k) <- elt :: classes.(k)
      ) elements;
    let classes = Array.filter (function [] -> false | _ -> true) classes in
    Array.map Array.of_list classes
                
  let compute_centroids classes =
    Array.map E.mean classes

  let rec iterate centroids elements threshold =
    let classes    = compute_classes centroids elements in
    let centroids' = compute_centroids classes in
    let dist =
      Array.mapi (fun i c -> E.dist c centroids'.(i)) centroids'
      |> Array.fold_left (+.) 0.
    in
    if dist < threshold then
      classes
    else
      iterate centroids' (Array.concat (Array.to_list classes)) threshold

  let forgy_init k elements =
    Array.of_enum (Random.multi_choice k (Array.enum elements))

  let random_partition_init k elements =
    let classes = Array.create k [] in
    Array.iter (fun elt ->
        let i = Random.int k in
        classes.(i) <- elt :: classes.(i)
      ) elements;
    let classes = Array.map Array.of_list classes in
    compute_centroids classes

  let pick_uniformly arr =
    let c = Array.length arr in
    if c = 0 then
      raise (KmeansError "pick_uniformly: empty array - bug found, please report")
    else
      arr.(Random.int c)

  (* Note that *)
  let pick_proportional arr =
    let total = Array.fsum arr in
    let r = Random.float total in
    let rec loop i acc =
      if acc <= arr.(i) then
        i 
      else
        loop (i+1) (acc -. arr.(i))
    in
    loop 0 r

  let rec kmeanspp_iter k centroids elements =
    if k = 0 then centroids
    else
      let dists = Array.map (fun e -> let _, d = closest centroids e in d *. d) elements in
      let i     = pick_proportional dists in
      let centroids = Array.concat [ centroids; [| elements.(i) |] ] in
      kmeanspp_iter (k-1) centroids elements
      
  let kmeanspp_init k elements =
    if k < 1 then
      raise (KmeansError "kmeanspp_init: k < 1, error")
    else
      let elt = pick_uniformly elements in
      kmeanspp_iter (k-1) [| elt |] elements
      
  let k_means ~k ~init ~elements ~threshold =
    let centroids = 
     match init with
      | Forgy ->
        forgy_init k elements
      | RandomPartition ->
        random_partition_init k elements
      | KmeansPP ->
        kmeanspp_init k elements
    in
    iterate centroids elements threshold
  
end
