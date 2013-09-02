#load "Load.fsx"

module C = Controls
open C.Operators
open FSharpx

let window _ =
    let canvas = Canvas.create []
    let swap =
        let caroussel =
            ref [
                @"http://planetquest.jpl.nasa.gov/system/news_items/images/39/medium/PIA15257.jpg"
                @"http://www.jpl.nasa.gov/images/kepler/20111205/kepler20111205-640.jpg"
                @"http://www.jpl.nasa.gov/images/kepler/20130717/pia17250-640.jpg"
                @"http://www.nasa.gov/images/content/502973main_exoplanet20101201-43_946-710.jpg"
            ]
        fun () ->
            let source = System.Windows.Media.Imaging.BitmapImage(uriSource = System.Uri caroussel.Value.Head )
            canvas |> Controls.withImage source
            caroussel := caroussel.Value.Tail @ [caroussel.Value.Head]

    swap()
    
    C.controlPanel [
        Button.create "Change"
        |>! C.withMargins 3 0 3 3
        |>! Button.onClick swap
    ] -- canvas

window
|> Window.create -1e3 5e1 8e2 4e2
|> Window.show