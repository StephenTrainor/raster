open Core

let select_region ~w ~h image =
  let x_bound = Image.width image - w in
  let y_bound = Image.height image - h in
  Image.slice_random_in_bounds image ~x_bound ~y_bound ~w ~h
;;

let mse region1 region2 ~w ~h =
  Image.foldi region1 ~init:0.0 ~f:(fun ~x ~y acc (r1, g1, b1) ->
    let r2, g2, b2 = Image.get region2 ~x ~y in
    let subtotal =
      Int.pow (r1 - r2) 2 + Int.pow (g1 - g2) 2 + Int.pow (b1 - b2) 2
    in
    acc +. float_of_int subtotal)
  /. (float_of_int w *. float_of_int h)
;;

let select_other_region ~w ~h ~coordinate_list image region_to_compare =
  let image_with_smallest_mse =
    List.map coordinate_list ~f:(fun (x, y) ->
      let small_image = Image.slice_in_bounds_from_w_h ~x ~y ~w ~h image in
      let mse = mse ~w ~h small_image region_to_compare in
      small_image, (x, y), mse)
    |> List.min_elt ~compare:(Comparable.lift Float.compare ~f:trd3)
  in
  match image_with_smallest_mse with
  | None -> failwith "Couldn't find an image to compare with"
  | Some (image, (x, y), _) -> image, x, y
;;

let swap_pixels ~x1 ~y1 ~x2 ~y2 image region1 region2 =
  (* .iteri, rename vars *)
  Image.foldi region1 ~init:() ~f:(fun ~x ~y () pixel1 ->
    let pixel2 = Image.get region2 ~x ~y in
    Image.set image ~x:(x1 + x) ~y:(y1 + y) pixel2;
    Image.set image ~x:(x2 + x) ~y:(y2 + y) pixel1)
;;

let transform image ~w ~h ~moves =
  let grid_w = Image.width image / w in
  let grid_h = Image.height image / h in
  let coordinate_list =
    List.init (grid_h * grid_w) ~f:(fun i ->
      let x = i % grid_w * w in
      let y = i / grid_w * h in
      x, y)
  in
  let rec transform' moves_left =
    if moves_left = 0
    then ()
    else (
      let region1, x1, y1 = select_region ~w ~h image in
      let region2, x2, y2 =
        select_other_region image ~w ~h ~coordinate_list region1
      in
      swap_pixels image ~x1 ~y1 ~x2 ~y2 region1 region2;
      transform' (moves_left - 1))
  in
  transform' moves
;;

let command =
  Command.basic
    ~summary:"Mosaic"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      and width =
        flag "width" (required Command.Param.int) ~doc:"width of regions"
      and height =
        flag "height" (required Command.Param.int) ~doc:"height of regions"
      and moves =
        flag "moves" (required Command.Param.int) ~doc:"# of moves"
      in
      fun () ->
        let image = Image.load_ppm ~filename in
        transform image ~w:width ~h:height ~moves;
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_mosaic.ppm")]
;;
