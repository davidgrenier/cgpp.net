#load "Load.fsx"

module C = Controls
open C.Operators

Window.create -1e3 5e1 8e2 8e2 (fun _ ->
    C.controlPanel [
        Slider.create 2e1 2.0
        |>! C.withWidth 1e2
    ] -- Canvas.create []
)
|> Window.show