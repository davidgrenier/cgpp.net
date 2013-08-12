#load "WPF.fsx"
open ExtCore.Collections
open FSharpx
open WPF

module C = Controls
open C.Operators
open Transform.Operators

let defaultSpeed = Timespan.secs 14

let candidates() =
    let dot offx offy =
        Shapes.dot 1.5 Brushes.Yellow
        -+ Transform.translate offx offy

    let f = 37.0

    let crossLine a b =
        Shapes.line a b
        |>! Shapes.withStroke (Brushes.fromCode 0x222222)
        |>! Shapes.withMiterLimit

    Canvas.create [
        crossLine (5.0, 0.0) (-5.0, 0.0)
        crossLine (0.0, 5.0) (0.0, -5.0)

        dot f f
        dot -f f
        dot 0.0 (-f * sqrt 2.0)
    ]
    -+ Animations.fullRevo -36e1 0.0 defaultSpeed

let canvas candidates =
    Canvas.create []
    -+ Transform.scale (fromMili 1.0<mm>) (fromMili 1.0<mm>)
    -+ Transform.flipY
    -+ Animations.fullRevoOver defaultSpeed
    |>! Elements.onLoad (fun c ->
        let b = fromMili (sqrt 2.0 * 1.0<_>)
        let w, h = c.ActualWidth / b, c.ActualWidth / b

        [
            let f = 18.0
            for x in [f..f..w] @ [0.0 .. -f .. -w] do
            for y in [f..f..h] @ [0.0 .. -f .. -h] do
                if (x, y) <> (0.0, 0.0) then
                    yield x, y
        ]
        |> List.collect (fun (x, y) ->
            let line a b = Shapes.line a b |>! Shapes.withStroke Brushes.Navy |>! Shapes.withThickness 0.6
            let f = 3.5
            [
                line (x - f, y) (x + f, y)
                line (x, y - f) (x, y + f)
            ]
        )
        |> flip C.addChildren c
    )
    |>! Elements.onLoad (fun c ->
        c -+ Transform.translate (c.ActualWidth / 2.0) (c.ActualHeight / 2.0)        
        -- candidates
        |> ignore
    )

let slider max ticksAt =
    Slider.create max ticksAt
    |>! C.withWidth 1e2
    |>! Slider.snaps
    |>! Slider.withToolTip Slider.BottomRight
    |>! Slider.withTick Slider.BottomRight

Window.create -1e3 5e1 8e2 8e2 (fun w ->
    let canvas = canvas (candidates())
    C.dockPanel [
        C.stackPanel [
            slider 20.0 2.0
            |> C.withLabel "CrossDistance"

            C.colorPicker Colors.Black (fun c -> C.withBackground c w)
            |> C.withLabel "Background"

            C.colorPicker Colors.Navy (fun c ->
                canvas.Children
                |> Seq.cast<obj>
                |> Seq.choose (function
                    | :? System.Windows.Shapes.Line as line -> Some line
                    | _ -> None
                )
                |> Seq.iter (fun x -> x |> Shapes.withStroke (Brushes.ofColor c))
            )
            |> C.withLabel "GridArray"
        ]
        |>! C.dock Dock.Left
        |>! C.withBackground (Colors.fromCode 0xECE9D8)
        |>! C.withZIndex 1

        canvas
    ]
)
|>! C.withBackground Colors.Black
|> Window.show