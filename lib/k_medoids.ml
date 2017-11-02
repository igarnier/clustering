open Batteries

(* K-medoids only requires that elements are endowed with the structure
   of a metric space. *)
module type Element =
sig

  type t

  (* This should be a proper distance function (symmetric, zero on the diagonal,
     verifying the triangular inequality). *)
  val dist : t -> t -> float
  
end

module Make(E : Element) =
struct

  type elt = E.t

  type init =
    | Forgy      (* Selects k elements at random (without replacement) *)
    | KmedoidsPP (* K-means++ *)


  type algorithm =
    | PAM              (* Partition Around Medoids - the classical greedy algorithm. Costly. *)
    | VoronoiIteration (* Another heuristic, less costly but perhaps less reliable. *)
        

  type fref = {
    mutable contents : float
  }

  exception KmedoidsError of string
  
  let fref x = { contents = x }
  
  let closest dist elt medoids =
    let m = ref 0  in
    let d = fref max_float in
    for i = 0 to Array.length medoids - 1 do
      let dist = dist elt medoids.(i) in
      if dist < d.contents then
        (d.contents <- dist;
         m := i)
    done;
    !m, d.contents

  let cost_ dist elements medoids =
    Array.fold_lefti (fun acc i elt ->
        let _, dist_to_closest = closest dist elt medoids in
        acc +. dist_to_closest
      ) 0.0 elements

  let pam_step dist elements medoids =
    for mi = 0 to Array.length medoids - 1 do
      let current_cost = fref (cost_ dist elements medoids) in
      let m = medoids.(mi) in
      for ei = 0 to Array.length elements - 1 do
        let e = elements.(ei) in
        medoids.(mi) <- e;
        let new_cost = cost_ dist elements medoids in
        if new_cost >= current_cost.contents then
          medoids.(mi) <- m
        else
          current_cost.contents <- new_cost
      done
    done
  
  let produce_clusters dist elements medoids =
    let buckets = Array.make (Array.length medoids) [] in
    Array.iter (fun elt ->
        let closest_idx, _ = closest dist elt medoids in
        buckets.(closest_idx) <- elt :: buckets.(closest_idx)
      ) elements;
    Array.map Array.of_list buckets

  let compute_medoid_of_class dist cls =
    let centralities =
      Array.map (fun elt ->
          let dists = Array.map (dist elt) cls in
          let centrality = Array.fsum dists in
          (elt, centrality)
        ) cls
    in
    Array.sort (fun (_, c) (_, c') -> Float.compare c c') centralities;
    let (elt, cost) = centralities.(0) in
    elt, cost

  let compute_medoids dist classes =
    let result  = Array.map (compute_medoid_of_class dist) classes in
    let medoids = Array.map fst result in
    let costs   = Array.fsum (Array.map snd result) in
    (medoids, costs)
    
  let voronoi_iteration_step dist elements medoids =
    let classes = produce_clusters dist elements medoids in
    Array.modifyi (fun k _ ->
        fst (compute_medoid_of_class dist classes.(k))
      ) medoids

  let iterate dist elements medoids threshold step =
    let exception Break in
    let current_cost = fref (cost_ dist elements medoids) in
    try
      while true do
        step dist elements medoids;
        let new_cost = cost_ dist elements medoids in
        assert (new_cost <= current_cost.contents);
        if current_cost.contents -. new_cost < threshold then
          raise Break
        else
          current_cost.contents <- new_cost
      done
    with Break -> ()

  let pick_uniformly arr =
    let c = Array.length arr in
    if c = 0 then
      raise (KmedoidsError "pick_uniformly: empty array - bug found, please report")
    else
      arr.(Random.int c)

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

  let cost ~classes =
    snd (compute_medoids E.dist classes)

  let forgy_init k elements =
    Array.of_enum (Random.multi_choice k (Array.enum elements))
  
  let rec kmedoidspp_iter dist k medoids elements =
    if k = 0 then medoids
    else
      let dists = Array.map (fun elt ->
          let _, d = closest dist elt medoids in
          d
        ) elements in
      let i       = pick_proportional dists in
      let medoids = Array.concat [ medoids; [| elements.(i) |] ] in
      kmedoidspp_iter dist (k-1) medoids elements
      
  let kmedoidspp_init dist k elements =
    if k < 1 then
      raise (KmedoidsError "kmedoidspp_init: k < 1, error")
    else
      let elt = pick_uniformly elements in
      kmedoidspp_iter dist (k-1) [| elt |] elements

  let k_medoids ~precompute ~elements =
    if precompute then
      let len = Array.length elements in
      let mat = Array.init len (fun i ->
          Array.init len (fun j -> E.dist elements.(i) elements.(j))
        )
      in
      let dist i j = mat.(i).(j) in
      fun ~k ~init ~algorithm ~threshold ->
        let elements_indices = Array.init len (fun i -> i) in
        let medoids =
          match init with
          | KmedoidsPP ->
            kmedoidspp_init dist k elements_indices
          | Forgy ->
            forgy_init k elements_indices
        in
        (match algorithm with
         | PAM ->
           iterate dist elements_indices medoids threshold pam_step
         | VoronoiIteration ->
           iterate dist elements_indices medoids threshold voronoi_iteration_step);
        let clusters = produce_clusters dist elements_indices medoids in
        Array.map (Array.map (fun i -> elements.(i))) clusters        
    else
      fun ~k ~init ~algorithm ~threshold ->
        let medoids =
          match init with
          | KmedoidsPP ->
            kmedoidspp_init E.dist k elements
          | Forgy ->
            forgy_init k elements
        in
        (match algorithm with
         | PAM ->
           iterate E.dist elements medoids threshold pam_step
         | VoronoiIteration ->
           iterate E.dist elements medoids threshold voronoi_iteration_step);
        produce_clusters E.dist elements medoids
  
end
