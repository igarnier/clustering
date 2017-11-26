(** K-medoids functor. *)

module type Element =
sig

  (** [t] is the type of elements to be clustered. *)
  type t   

  (** [dist] should be a distance function: symmetric, zero and the diagonal and verifying
     the triangular inequality.  *)
  val dist : t -> t -> float
  
end

module Make : functor (E : Element) ->
sig

  (** Initial choice of medoids.
      This implementation provides several initialization algorithms, 
      the standard one being Kmeans++ (KmeansPP) *)
  type init =

    (** [Forgy] selects k elements at random (without replacement) as initial centroids. *)
    | Forgy

    (** [KmedoidsPP] selects initial medoids iteratively with probabilities proportional
        to their distance to the previously selected centroids. This intuitively
        allows to spread them well. *)
    | KmedoidsPP

  (** Algorithm used to perform partitioning. *)
  type algorithm =

    (** [PAM] stands for Partition Around Medoids - the classical greedy algorithm. Costly. *)
    | PAM

    (** Another heuristic, proceeding similarly to Lloyd's algorithm for Kmeans. Less costly
        (but still more than Kmeans) but perhaps less precise. *)
    | VoronoiIteration


  (** Exception thrown by [k_medoids] in case something goes awry.*)
  exception KmedoidsError of string  


  (* In contrast with [k_means], k_medoids never has to produce new elements,
     and the distance function is only evaluated on the given set of elements.
     It might be worthwhile to precompute the distance on those elements,
     especially if the distance function is costly and if one wants to compute a clustering for several [k]
     or try different algorithms with the same dataset. This is done by partially evaluating [k_medoids] and
     setting precompute to true, as in:

     let cluster_function = k_medoids ~precompute:true ~elements in
     let res1 = cluster_function ~k:10 ~init:KmedoidsPP ~algorithm:PAM ~threshold:0.1 in
     let res2 = cluster_function ~k:15 ~init:KmedoidsPP ~algorithm:VoronoiIteration ~threshold:0.1 in
     ...
  *)
  
  (** [k_means] performs the clustering using to the provided initialization method.
      When the centroids collectively move less than [threshold], the algorithm terminates. *)
  val k_medoids : precompute:bool -> elements:E.t array -> k:int -> init:init -> algorithm:algorithm -> threshold:float -> E.t array array

  (** [cost] returns the sum over all classes of the sum of distances from
      the medoid of the class to all elements of the class. *)
  val cost : classes:E.t array array -> float

end