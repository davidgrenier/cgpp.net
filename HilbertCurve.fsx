#load "Load.fsx"

module C = Controls
open C.Operators
open FSharpx

Window.create -1e3 5e1 8e2 8e2 (fun _ ->
    C.dockPanel [
        C.controlPanel [
            Slider.create 8.0 1.0
            |>! Slider.withToolTip Slider.BottomRight
            |>! C.withWidth 1e2
            |> C.withLabel "Depth"

            Slider.create 25.0 1.0
            |>! Slider.withToolTip Slider.BottomRight
            |>! C.withWidth 1e2
            |> C.withLabel "Smoothing"

            Button.create "Smooth"
            |>! Button.onClick (fun () -> ())
        ]

        Canvas.create []
    ]
)
|> Window.show

let rot n x y rx ry =
    match ry, rx with
    | 0, 1 -> n - 1 - y, n - 1 - x
    | 0, _ -> y, x
    | _ -> x, y

let point n d =
    let rec point t x y = function
        | s when s < n ->
            let rx = 1 &&& t / 2
            let ry = 1 &&& (t ^^^ rx)
            let x, y = rot s x y rx ry
            point (t / 4) (x + s * rx) (y + s * ry) (s * 2)
        | _ -> x, y
    point d 0 0 1

let points n = Seq.init (n * n) (point n)

points 2
points 3
|> Seq.toArray
points 9
|> Seq.toArray
            
let pointCount =
    seq {
        for x = 1 to 6 do
            let n = 1 <<< x
            yield n, points n |> Seq.length
    }
    |> Seq.toArray