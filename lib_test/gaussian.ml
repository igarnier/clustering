open Batteries
open Owl
open Clustering
open Vplot

module Elt =
struct

  type t = Vlayout.Pt.t

  let dist v1 v2 =
    Vlayout.Pt.(norm (v2 - v1))

  let mean vec_array =
    let sum = Array.fold_left Vlayout.Pt.plus Vlayout.Pt.zero vec_array in
    Vlayout.Pt.scale sum (1. /. (float (Array.length vec_array)))
  
end

let vec2pt vec = Vlayout.Pt.pt (Vec.get vec 0) (Vec.get vec 1)

let mean0 =
  Vec.of_array [| 2.0; ~-. 1.5 |]

let mean1 =
  Vec.of_array [| 3.5;  3.0 |]

let mean2 =
  Vec.of_array [| ~-. 2.5;  0.0 |]

let dataset0 =
  Array.init 60 (fun _ -> Vec.add mean0 (Vec.gaussian 2))
  |> Array.map vec2pt

let dataset1 =
  Array.init 60 (fun _ -> Vec.add mean1 (Vec.gaussian 2))
  |> Array.map vec2pt

let dataset2 =
  Array.init 60 (fun _ -> Vec.add mean2 (Vec.gaussian 2))
  |> Array.map vec2pt

let plot_scatter ds1 ds2 ds3 deco =
  let data1 = Scatter.({ data = ds1; plot_type = Scatter { shape = Circle { radius = 0.025 }; color = Vlayout.Style.red } }) in
  let data2 = Scatter.({ data = ds2; plot_type = Scatter { shape = Cross { length = 0.05 }; color = Vlayout.Style.green } }) in
  let data3 = Scatter.({ data = ds3; plot_type = Scatter { shape = Square { length = 0.05 }; color = Vlayout.Style.blue } }) in
  Plot.Plot {
    vp = Viewport.AutoY { xsize = Units.mm 200. };
    plot = Scatter { data = [data1; data2; data3] @ deco; options = [] }
  }
  
let dataset = Array.concat [dataset0; dataset1; dataset2]

let _ = Plot.plot_pdf "dataset.pdf" (plot_scatter dataset0 dataset1 dataset2 [])

let classes = 3

(* Test k-medoids avec PAM *)
let _ =
  let module K = K_medoids.Make(Elt) in
  let [| cl0; cl1; cl2 |] =
    let result =
      K.k_medoids
        ~precompute:false
        ~elements:dataset
        ~k:classes
        ~algorithm:`PAM
        ~init:`KmedoidsPP
        ~termination:(`Threshold 0.1)
    in
    if Array.length result != classes then
      failwith "bug found"
    else
      [| result.(0); result.(1); result.(2) |]
  in
  Plot.plot_pdf "result_kmedoids_pam.pdf" (plot_scatter cl0 cl1 cl2 [])


(* Test k-medoids avec VoronoiIteration *)
let _ =
  let module K = K_medoids.Make(Elt) in
  let [| cl0; cl1; cl2 |] =
    let result =
      K.k_medoids
        ~precompute:false
        ~elements:dataset
        ~k:classes
        ~algorithm:`VoronoiIteration
        ~init:`KmedoidsPP
        ~termination:(`Threshold 0.1)
    in
    if Array.length result != classes then
      failwith "bug found"
    else
      [| result.(0); result.(1); result.(2) |]
  in
  Plot.plot_pdf "result_kmedoids_voronoi.pdf" (plot_scatter cl0 cl1 cl2 [])

(* Test k-medoids *)
let _ =
  let module K = K_means.Make(Elt) in
  let [| cl0; cl1; cl2 |] =
    let result =
      K.k_means
        ~elements:dataset
        ~k:classes
        ~init:`KmeansPP
        ~termination:(`Threshold 0.1)
    in
    if Array.length result != classes then
      failwith "bug found"
    else
      [| result.(0); result.(1); result.(2) |]
  in
  Plot.plot_pdf "result_kmeans.pdf" (plot_scatter cl0 cl1 cl2 [])

let print_cluster points =
  let module C = Vplot.Cmds in
  let clr   = Vlayout.Style.black in
  (* let len = Array.length points in *)
  (* if len < 10 then *)
  (*   C.style ~style:Vlayout.Style.(make ~stroke:(solid_stroke ~clr:black) ~width:None ~dash:None ~fill:None) *)
  (*     ~subcommands:[] *)
  (* else *)
  let box = C.Bbox.of_points_arr points in
  let box = C.box ~mins:(C.Bbox.sw box) ~maxs:(C.Bbox.ne box) in
  let sty = Vlayout.Style.(make ~stroke:(solid_stroke ~clr) ~width:None ~dash:None ~fill:None) in
  C.style ~style:sty ~subcommands:[box]

(* Test agglomerative avec Hausdorff *)
module H =
struct
  include Gromov.Hausdorff.ArrayBased(Elt)
  type elt = Elt.t

  let singleton x = [|x|]
                    
  (* We deal with partitions, so no need to remove duplicates. *)
  let join a b = Array.concat [a;b]
end


module Agglo = Agglomerative.Make(Elt)(H)

let rec display (cluster : Agglo.cluster) =
  match cluster.Agglo.tree with
  | Agglo.Node(c, c') ->
    `Node(cluster.set, [display c; display c'])
  | Agglo.Leaf ->
    `Node(cluster.set, [])

let _ =
  let tree = Agglo.cluster (Array.to_list dataset) in
  let clusters = Agglo.all_clusters tree in
  let clusters = List.sort (fun (x, _) (y, _) ->  
      Int.compare (Array.length x) (Array.length y)
    )  clusters
  in
  let clusters = List.rev clusters in
  let clusters = List.take 4 clusters in
  let clusters = Agglo.truncate tree 2 in
  let boxes = Scatter.{ data = [||]; plot_type = Decoration (List.map print_cluster clusters) } in
  Plot.plot_pdf "agglomerative.pdf" (plot_scatter dataset0 dataset1 dataset2 [boxes]);
  Plot.plot_pdf "dendrogram.pdf"
    (Plot.Cmd
       [Trees.plot
          ~options:[`Deltax (Units.mm 0.0); `Deltay (Units.mm 20.0); `Scalex 0.4; `Scaley 0.9; `Halign `Bottom; ]
          ~viewport:(Viewport.AutoX { ysize = Units.mm 300.0 })
          ~data:
            { tree = display tree;
              tree_type =
                Dendrogram { lbl = (fun set -> 
                    let len  = Array.length set in
                    let flen = float len in
                    let text = [Cmds.text ~pos:{ pos = Vlayout.Pt.zero; relpos = Cmds.North } ~width:10.0 ~height:5.0 ~text:(string_of_int len)] in
                    text, flen)
                  }
            }
       ]
    )
