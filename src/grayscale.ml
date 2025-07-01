open Core

(* You need to change the implementation of this function so that it does something
   to the image instead of just leaving it untouched. *)
let transform image = Image.map ~f:Pixel.average image

let%expect_test "grayscale_transform" =
  let image_expected =
    Image.load_ppm ~filename:"../images/reference-beach_portrait_gray.ppm"
  in
  let image_made =
    Image.load_ppm ~filename:"../images/beach_portrait.ppm" |> transform
  in
  Image.image_comparison_report image_expected image_made;
  [%expect {| Identical images |}]
;;

let command =
  Command.basic
    ~summary:"Convert an image to grayscale"
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
            (String.chop_suffix_exn filename ~suffix:".ppm" ^ "_gray.ppm")]
;;
