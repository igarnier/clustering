
(** K-means functor. *)

module type Element =
sig

  (** [t] is the type of elements to be clustered. *)
  type t   

  (** In principle, [dist] should be a distance function: symmetric, zero and the diagonal and verifying
     the triangular inequality. If these assumptions are violated, the algorithm will
     still terminate with a "clustering" though. *)
  val dist : t -> t -> float

  (** Elements of type [t] should support taking arithmetic means. The function
      [mean] provides this. *)
  val mean : t array -> t
  
end

module Make : functor (E : Element) ->
sig

  (** K-means is rather sensitive to the initial choice of centroids.
      This implementation provides several initialization algorithms, 
      the standard one being Kmeans++ (KmeansPP) *)
  type init =

    (** [Forgy] selects k elements at random (without replacement) as initial centroids. *)
    | Forgy

    (** Assigns each point to a random cluster, and computes the corresponding centroid.
        Note that these centroids do not necessarily belong to the dataset, which might
        cause robustness issues. *)  
    | RandomPartition

    (** [KmeansPP] selects initial centroids iteratively with probabilities proportional
        to their squared distance to the previously selected centroids. This intuitively
        allows to spread them well. *)
    | KmeansPP


  (** [k_means] performs the clustering using to the provided initialization method.
      When the centroids collectively move less than [threshold], the algorithm terminates.
  *)
  val k_means : k:int -> init:init -> elements:E.t array -> threshold:float -> E.t array array

end
