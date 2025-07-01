open Core

let ordinal_to_float ordinal max_val =
  float_of_int ordinal /. float_of_int max_val
;;

let solarization_function x = 1. -. (4. *. x) +. (4. *. x *. x)

let new_rgb rgb max_val =
  int_of_float
    ((ordinal_to_float rgb max_val |> solarization_function)
     *. float_of_int max_val)
;;

let transform image =
  let max_val = Image.max_val image in
  Image.mapi image ~f:(fun ~x:_ ~y:_ (r, _, _) ->
    let new_brightness = new_rgb r max_val in
    new_brightness, new_brightness, new_brightness)
;;

(* let%expect_test "solarize_transform" =
   Image.image_comparison_report expected_image transformed_image;
   [%expect {| Identical images |}]
   ;; *)

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
