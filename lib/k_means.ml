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
  
  let closest elements centroids elti =
    let m = ref 0  in
    let d = ref max_float in
    for i = 0 to Array.length centroids - 1 do
      let dist = E.dist elements.(elti) centroids.(i) in
      if dist < !d then
        (d := dist; m := i)
    done;
    !m, !d
  
  let compute_classes centroids elements =
    let classes : int list array =
      Array.create (Array.length centroids) []
    in
    Array.iteri (fun elti _ ->
        let k, _ = closest elements centroids elti in
        classes.(k) <- elti :: classes.(k)
      ) elements;
    let classes = Array.filter (function [] -> false | _ -> true) classes in
    Array.map Array.of_list classes
                
  let compute_centroids (elements : elt array) (classes : int array array) =
    Array.map (fun arr -> E.mean (Array.map (fun i -> elements.(i)) arr)) classes

  let rec iterate (centroids : E.t array) (elements : E.t array) threshold =
    let classes    = compute_classes centroids elements in
    let centroids' = compute_centroids elements classes in
    let dist =
      Array.mapi (fun i c -> E.dist c centroids'.(i)) centroids'
      |> Array.fsum
    in
    if dist < threshold then
      classes
    else
      iterate centroids' elements threshold

  let forgy_init k elements =
    Array.of_enum (Random.multi_choice k (Array.enum elements))

  let random_partition_init k elements =
    let classes = Array.create k [] in
    Array.iteri (fun elti _ ->
        let i = Random.int k in
        classes.(i) <- elti :: classes.(i)
      ) elements;
    let classes = Array.map Array.of_list classes in
    compute_centroids elements classes

  let pick_uniformly arr =
    let c = Array.length arr in
    if c = 0 then
      raise (KmeansError "pick_uniformly: empty array - bug found, please report")
    else
      arr.(Random.int c)

  (* Note that the distance to a point to itself is 0, so the probability for a centroid
     to pick itself is also zero. *)
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
      let dists = Array.mapi (fun elti _ -> let _, d = closest elements centroids elti in d *. d) elements in
      let i     = pick_proportional dists in
      let centroids = Array.concat [ centroids; [| elements.(i) |] ] in
      kmeanspp_iter (k-1) centroids elements
      
  let kmeanspp_init k elements =
    if k < 1 then
      raise (KmeansError "kmeanspp_init: k < 1, error")
    else
      let elt = pick_uniformly elements in
      kmeanspp_iter (k-1) [| elt |] elements

  let k_means_internal ~k ~init ~elements ~threshold =
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
      
  let k_means ~k ~init ~elements ~threshold =
    let classes = k_means_internal ~k ~init ~elements ~threshold in
    Array.map (Array.map (fun i -> elements.(i))) classes

  
  (* let multi_start ~k ~init ~elements ~threshold ~nstarts = *)
  (*   let result = Array.init nstarts *)
  (*       (fun _ -> *)
  (*          k_means_internal ~k ~init ~elements ~threshold *)
  (*       ) *)

    
  
end
