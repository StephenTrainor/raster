open Core

(* let ordinal_to_float ordinal max_val =
   float_of_int ordinal /. float_of_int max_val
   ;;

   let solarization_function x = 1. -. (4. *. x) +. (4. *. x *. x)

   let new_rgb rgb max_val =
   int_of_float
   ((ordinal_to_float rgb max_val |> solarization_function)
   *. float_of_int max_val)
   ;; *)

let invert (r, g, b) max_val =
  let threshold = int_of_float (Float.round (float_of_int max_val *. 0.4)) in
  let new_r = if r > threshold then max_val - r else r
  and new_g = if g > threshold then max_val - g else g
  and new_b = if b > threshold then max_val - b else b in
  new_r, new_g, new_b
;;

let transform image =
  let max_val = Image.max_val image in
  Image.mapi image ~f:(fun ~x:_ ~y:_ pixel ->
    (* let new_brightness = new_rgb r max_val in *)
    (* new_brightness, new_brightness, new_brightness) *)
    invert pixel max_val)
;;

let command =
  Command.basic
    ~summary:"Solarize an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      in
      fun () ->
        let image = Image.load_ppm ~filename |> transform in
        Image.save_ppm
          image
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_solarize.ppm")]
;;
