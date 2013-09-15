#load "Load.fsx"

open Transform.Operators
module C = Controls
open C.Operators
open FSharpx

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

let points level =
    let n = 1 <<< level
    Seq.init (n * n) (point n)

Window.create 8e1 5e1 8e2 68e1 (fun _ ->
    let margin = fromInch 0.25<inch>

    let curve =
        Polygon.createOpen []
        |>! Shapes.withStroke Brushes.Black

    let updatePoints f =
        curve.Points <-
            curve
            |> Polygon.points
            |> Seq.map Points.coords
            |> f
            |> Shapes.pointCollection

    let smoothing =
        Slider.create 0.10 0.01 0.25
        |>! Slider.withToolTip Slider.BottomRight
        |>! C.withWidth 8e1
        |>! Slider.initTo 0.15

    let depth =
        Slider.create 1.0 1.0 7.0
        |>! Slider.withToolTip Slider.BottomRight
        |>! Slider.withTick Slider.BottomRight
        |>! Slider.snaps
        |>! C.withWidth 8e1

    let subdivide =
        let (!) a b = (1.0 - smoothing.Value) * a + smoothing.Value * b
        let smooth (x1, y1) (x2, y2) = !x1 x2, !y1 y2

        fun () ->
            updatePoints (fun points ->
                seq {
                    yield Seq.head points
                    for p1, p2 in points |> Seq.skip 1 |> Seq.zip points do
                        yield smooth p1 p2
                        yield smooth p2 p1
                    yield Seq.last points
                }
            )

    let generateCurve depth (c: Canvas.T) =
        updatePoints (fun _ ->
            let factor = min c.ActualHeight c.ActualWidth
            let m = (1 <<< depth) - 1 |> float
            let factor = (factor - 2.0 * margin) / m
            points depth
            |> Seq.map (fun (x, y) -> float x * factor, float y * factor)
        )

    let canvas =
        Canvas.create [
            curve
        ]
        -+ Transform.flipY
        |>! Elements.onLoad (fun c ->
            generateCurve 1 c

            c -+ Transform.translate margin (c.ActualHeight - margin)
            |> ignore
        )

    C.dockPanel [
        C.controlPanel [
            depth
            |>! Slider.onChange (fun _ depth ->
                generateCurve (int depth) canvas
            )
            |> C.withLabel "Depth"

            smoothing
            |> C.withLabel "Smoothing"

            Button.create "Smooth"
            |>! Button.onClick subdivide
        ]
        
        canvas
    ]
)
|> Window.show