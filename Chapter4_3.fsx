#load "Load.fsx"

module C = Controls
open C.Operators
open Piglets

let caroussel =
    [
        "http://www.nasa.gov/images/content/324230main_image_1320_946-710.jpg"
        "http://www.nasa.gov/centers/jpl/images/content/230983main_exoplanet-browse.jpg"
        "http://www.nasa.gov/centers/jpl/images/content/215981main_exoplanet-20080227-browse.jpg"
        "http://www.nasa.gov/images/content/502973main_exoplanet20101201-43_946-710.jpg"
        "http://www.nasa.gov/images/content/669051main_pia15808-43_946-710.jpg"
        "http://www.nasa.gov/images/content/672454main_47s_kepler_4_full.jpg"
    ]
    |> Seq.map (fun x -> System.Windows.Media.Imaging.BitmapImage(System.Uri x))
    |> Seq.repeat

Window.create 8e1 5e1 8e2 6e2 (fun _ ->
    Yield None
    |> Render (fun clicked ->
        let image =
            clicked
            |> Reader.zipWithSeq caroussel
            |> Reader.map snd
            |> Controls.image

        C.controlPanel [
            Controls.button "Change" clicked
            |>! C.withMargins 3 0 3 3
        ] -- image
    )
)
|>! C.withBackground Colors.Black
|> Window.show