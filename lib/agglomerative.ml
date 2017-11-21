open Batteries

module type Element =
sig

  type t

  val dist : t -> t -> float

end

module type ElementSet =
sig

  type t
  type elt

  val singleton : elt -> t

  val dist : t -> t -> float

  val join : t -> t -> t
    
end

module Make = functor (E : Element) (S : ElementSet with type elt = E.t) ->
struct

  module VP = Vp_tree.Make(S)
  
  type tree =
    | Leaf of S.t
    | Node of S.t * tree * tree

  type cluster = { set : S.t; closest : S.t; dist : float; tree : tree }

  (* let dist c1 c2 = S.dist c1.set c2.set *)

  (* let join c1 c2 = *)
  (*   let set = S.join c1.set c2.set in *)
  (*   { *)
  (*     set; tree = Node(set, cluster1.tree, cluster2.tree) *)
  (*   } *)

  let sort clusters =
    List.sort (fun { dist = d1 } { dist = d2 } -> Float.compare d1 d2) clusters

  let take key l =
    let rec take key l acc =
      match l with
      | [] -> failwith "take: key not found"
      | c :: tl ->
        if S.dist c.set key = 0.0 then
          (c, List.rev_append acc tl)
        else
          take key tl (c :: acc)
    in
    take key l []

  let update_closest set clusters =
    let rec update_closest set clusters closest_opt acc =
      match clusters with
      | [] -> acc, closest_opt
      | c :: tl ->
        let d = S.dist set c.set in
        let c =
          if d < c.dist then
            { c with dist = d; closest = set }
          else
            c
        in
        match closest_opt with
        | None ->
          update_closest set tl (Some (c.set, d)) (c :: acc)
        | Some (_, closest_dist) when d < closest_dist ->
          update_closest set tl (Some (c.set, d)) (c :: acc)
        | _ ->
          update_closest set tl closest_opt (c :: acc)
    in
    match update_closest set clusters None [] with
    | (clusters, None) -> failwith "empty clusters"
    | (clusters, Some (closest, closest_dist)) -> (clusters, closest, closest_dist)

  (* Invariant: when calling [iterate clusters], clusters is sorted according to dist. *)
  let rec iterate clusters =
    match clusters with
    | []         -> failwith "empty clusters list"
    | [cluster]  -> Leaf cluster.set
    | [cl1; cl2] ->
      let s = S.join cl1.set cl2.set in
      Node(s, cl1.tree, cl2.tree)
    | _ ->
      match clusters with
      | [] -> failwith "bug"
      | head :: tail ->
        let set  = S.join head.set head.closest in
        let closest_cluster, tail = take head.closest tail in
        let tail, closest, dist   = update_closest set tail in
        let c = {
          set; closest; dist; tree = Node(set, head.tree, closest_cluster.tree)
        } in
        iterate (sort (c :: tail))

  let cluster elements =
    let elements = List.map S.singleton elements in
    let vptree   = VP.create elements in
    let clusters = List.map (fun set ->
        let dist, closest = VP.nearest_neighbor set vptree in
        { set; closest; dist; tree = Leaf set }
      ) elements
    in
    iterate clusters
 
  let truncate tree depth =
    let rec truncate tree depth queue acc =
      match tree with
      | Leaf set ->
        (if depth > 0 then
           invalid_arg "truncate: tree too short"
         else
           let acc = set :: acc in
           match queue with
           | [] -> acc
           | (next, d) :: tl ->
             truncate next d tl acc)
      | Node(set, l, r) ->
        if depth = 0 then
           let acc = set :: acc in
           match queue with
           | [] -> acc
           | (next, d) :: tl ->
             truncate next d tl acc
        else
          truncate l (depth-1) ((r,depth-1) :: queue) acc
    in
    truncate tree depth [] []


end
