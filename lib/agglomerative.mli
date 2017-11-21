(** Agglomerative clustering functor. *)

module type Element =
sig

  (** [t] is the type of elements to be clustered. *)
  type t


  (** [dist] should be a distance function: symmetric, zero and the diagonal and verifying
     the triangular inequality.  *)
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

(* [Make] takes as first argument a module [E : Element] of elements 
   admitting the structure of a metric space together with that of a 
   total order. The second argument of [Make] is a module endowing
   /sets/ of elements with the structure of a metric space (for
   instance, the Hausdorff distances over [E], but other choices
   are possible).*)
module Make : functor (E : Element) (S : ElementSet with type elt = E.t) ->
sig

  type tree =
    | Leaf of S.t
    | Node of S.t * tree * tree

  val cluster : E.t list -> tree

  val truncate : tree -> int -> S.t list
end
