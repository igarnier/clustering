
(** K-means functor. *)

module type Element =
sig

  (** [t] is the type of elements to be clustered. *)
  type t   

  (** In principle, [dist] should be a distance function: symmetric, zero on the diagonal and verifying
     the triangular inequality. If these assumptions are violated, the algorithm will
     still terminate with a "clustering" though. *)
  val dist : t -> t -> float

  (** Elements of type [t] should support taking arithmetic means. The function
      [mean] provides this. *)
  val mean : t array -> t
  
end

(** K-means is rather sensitive to the initial choice of centroids.
    This implementation provides several initialization algorithms, 
    the standard one being Kmeans++ (KmeansPP) *)
type init =
  [
  (** [Forgy] selects k elements at random (without replacement) as initial centroids. *)
  | `Forgy

  (** Assigns each point to a random cluster, and computes the corresponding centroid.
      Note that these centroids do not necessarily belong to the dataset, which might
      cause robustness issues. *)  
  | `RandomPartition

  (** [KmeansPP] selects initial centroids iteratively with probabilities proportional
      to their squared distance to the previously selected centroids. This intuitively
      allows to spread them well. *)
  | `KmeansPP ]

(** Termination of the algorithm can be either specified as:
    1) an /exact/ number of iterations [Num_iter],
    2) a [Threshold] giving the biggest delta-[cost] decrease under which we stop iterating, or
    3) as the minimum of the above to, i.e. stop iterating when the [cost]-decrease is
       under [threshold] or when we reach [max_iter]. *)
type termination =
  [
  | `Num_iter  of int
  | `Threshold of float
  | `Min       of constraints ]
and constraints =  { max_iter : int; threshold : float }


(** Exception thrown by [k_means] in case something goes awry.*)
exception KmeansError of string  

module Make : functor (E : Element) ->
sig

  (** [k_means] performs the clustering using to the provided initialization method. *)
  val k_means : k:int -> init:init -> elements:E.t array -> termination:termination -> E.t array array

  (** [cost] returns the sum over all classes of the sum of squared distances from
      the mean of the class to all elements of the class. This quantity will decrease 
      monotonically as [k] increases, and the usual method to determine [k] is to select 
      the one where this quantity's decrease has its first inflection.  *)
  val cost : classes:E.t array array -> float

end
