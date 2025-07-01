open Core

let in_bounds ~x ~y image =
  if x < 0 || x >= Image.width image || y < 0 || y >= Image.height image
  then false
  else true
;;

let add_error_to_neighbors ~x ~y ~error image : unit =
  if in_bounds ~x:(x + 1) ~y image
  then Image.add ~x:(x + 1) ~y ~value:(7.0 *. error /. 16.0) image;
  if in_bounds ~x:(x - 1) ~y:(y + 1) image
  then Image.add ~x:(x - 1) ~y:(y + 1) ~value:(3.0 *. error /. 16.0) image;
  if in_bounds ~x ~y:(y + 1) image
  then Image.add ~x ~y:(y + 1) ~value:(5.0 *. error /. 16.0) image;
  if in_bounds ~x:(x + 1) ~y:(y + 1) image
  then Image.add ~x:(x + 1) ~y:(y + 1) ~value:(error /. 16.0) image
;;

let transform image =
  let grayscale = Grayscale.transform image in
  let max_val = Image.max_val grayscale in
  let half_max_val = max_val / 2 in
  Image.mapi grayscale ~f:(fun ~x ~y (r, _, _) ->
    let new_rgb_value = if r > half_max_val then max_val else 0 in
    let error = float_of_int (r - new_rgb_value) in
    add_error_to_neighbors ~x ~y ~error grayscale;
    new_rgb_value, new_rgb_value, new_rgb_value)
;;

let%expect_test "dither_transform" =
  let expected_image =
    Image.load_ppm ~filename:"../images/reference-beach_portrait_dither.ppm"
  in
  let transformed_image =
    Image.load_ppm ~filename:"../images/beach_portrait.ppm" |> transform
  in
  Image.image_comparison_report expected_image transformed_image;
  [%expect {| Identical images |}]
;;

let command =
  Command.basic
    ~summary:"Dither an image"
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
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_dither.ppm")]
;;
