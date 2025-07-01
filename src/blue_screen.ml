open Core

(* You need to change the implementation of this function so that it
   replaces the "blue" pixels of the foreground image with pixels from
   the corresponding position in the background image instead of
   just ignoring the background image and returning the foreground image.
*)
let transform ~foreground ~background =
  Image.mapi foreground ~f:(fun ~x ~y (r, g, b) ->
    if b > r + g then Image.get ~x ~y background else r, g, b)
;;

(* let dist =
      ((10496 - r) * (10496 - r))
      + ((12288 - g) * (12288 - g))
      + ((29184 - b) * (29184 - b))
      |> float_of_int
      |> Float.sqrt
      |> Float.round
      |> int_of_float
    in
    if dist < 25000 *)
(* then
      (* print_endline (string_of_int dist); *)
      Image.get ~x ~y background
    else r, g, b) *)

(* 19532 21331 39835 *)

let%expect_test "blue_screen_transform" =
  let background = Image.load_ppm ~filename:"../images/meadow.ppm" in
  let foreground = Image.load_ppm ~filename:"../images/oz_bluescreen.ppm" in
  let expected_image =
    Image.load_ppm ~filename:"../images/reference-oz_bluescreen_vfx.ppm"
  in
  let transformed_image = transform ~foreground ~background in
  Image.image_comparison_report expected_image transformed_image;
  [%expect {| Identical images |}]
;;

let command =
  Command.basic
    ~summary:
      "Replace the 'blue' pixels of an image with those from another image"
    [%map_open.Command
      let foreground_file =
        flag
          "foreground"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the foreground PPM image file"
      and background_file =
        flag
          "background"
          (required Command.Param.string)
          ~doc:"IMAGE_FILE the background PPM image file"
      in
      fun () ->
        let foreground = Image.load_ppm ~filename:foreground_file in
        let background = Image.load_ppm ~filename:background_file in
        let image' = transform ~foreground ~background in
        Image.save_ppm
          image'
          ~filename:
            (String.chop_suffix_exn foreground_file ~suffix:".ppm"
             ^ "_vfx.ppm")]
;;
