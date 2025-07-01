open Core

(* You need to modify this function to blur the input image
   based on the provided radius instead of ignoring it. *)
let transform image ~radius =
  let reference_copy = Image.copy image in
  Image.mapi image ~f:(fun ~x ~y (_ : Pixel.t) ->
    Image.slice_in_bounds reference_copy ~x ~y ~radius |> Image.mean_pixel)
;;

let%expect_test "blur_transform" =
  let expected_image =
    Image.load_ppm ~filename:"../images/reference-beach_portrait_blur.ppm"
  in
  let transformed_image =
    Image.load_ppm ~filename:"../images/beach_portrait.ppm"
    |> transform ~radius:3
  in
  Image.image_comparison_report expected_image transformed_image;
  [%expect {| Identical images |}]
;;

let command =
  Command.basic
    ~summary:"Blur an image"
    [%map_open.Command
      let filename =
        flag
          "filename"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the PPM image file"
      and radius =
        flag
          "radius"
          (required Command.Param.int)
          ~doc:"N the radius to use when blurring (higher = more blurred)"
      in
      fun () ->
        let image = Image.load_ppm ~filename in
        let image' = transform image ~radius in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_blur.ppm")]
;;
