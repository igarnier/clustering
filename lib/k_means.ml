open Batteries

(* We require elements to have the structure of a
   metric space, to support the computing of geometric means 
   and to be printable. *)
module type Element =
sig

  type t

  val print : t -> string

  (* This should be a proper distance function (symmetric, zero on the diagonal,
     verifying the triangular inequality). *)
  val dist : t -> t -> float

  val mean : t array -> t
  
end

module Make(E : Element) =
struct

  type elt = E.t

  type init =
    | Forgy (* Selects k elements at random (without replacement) *)
    | RandomPartition (* Select a random partition *)

  let max : float -> float -> float =
    fun x y ->
      if x > y then x
      else y
  
  let closest centroids elt =
    let m = ref 0  in
    let d = ref 0. in
    for i = 0 to Array.length centroids - 1 do
      let dist = E.dist elt centroids.(i) in
      if dist < !d then
        (d := dist; m := i)
    done;
    !m
  
  let compute_classes centroids elements =
    let classes : elt list array =
      Array.create (Array.length centroids) []
    in
    Array.iter (fun elt ->
        let k = closest centroids elt in
        classes.(k) <- elt :: classes.(k)
      ) elements;
    Array.map Array.of_list classes

  let compute_centroids classes =
    Array.map E.mean classes

  let rec iterate centroids elements =
    let classes    = compute_classes centroids elements in
    let centroids' = compute_centroids classes in
    let dist =
      Array.mapi (fun i c -> E.dist c centroids'.(i)) centroids
      |> Array.fold_left (+.) 0.
    in
    if dist < 0.1 then
      classes
    else
      iterate centroids' (Array.concat (Array.to_list classes))

  let k_means k init elements =
    let centroids =
      match init with
      | Forgy ->
        Array.of_enum (Random.multi_choice k (Array.enum elements))
      | RandomPartition ->
        (let classes = Array.create k [] in
         Array.iter (fun elt ->
             let i = Random.int k in
             classes.(i) <- elt :: classes.(i)
           ) elements;
         let classes = Array.map Array.of_list classes in
         compute_centroids classes)
    in
    iterate centroids elements
  
end
