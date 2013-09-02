#load "Load.fsx"

open Transform.Operators
module C = Controls
open C.Operators

Window.create -1e3 5e1 8e2 8e2 (fun _ ->
    let gray =
        Polygon.create [
            8e1, 0.0
            8e1, -1e2
            -8e1, -1e2
            -8e1, 0.0
        ]
        |>! Shapes.withFill Brushes.DarkGray

    let stripes =
        Canvas.create [
            for y in 0.0 .. 1.5 .. 1e2 ->
                Shapes.line (-8e1, y) (8e1, y)
                |>! Shapes.withStroke Brushes.Black
                |>! Shapes.withThickness 1.0
                :> _
        ]

    C.dockPanel [
        C.stackPanel [
            Slider.create 0.0 1.0 15.0
            |>! fun x -> x.Value <- 11.0
            |>! C.withWidth 1e2
            |>! Slider.onChange (fun _ n ->
                let setColor =
                    sprintf "%X" (int n)
                    |> String.replicate 6
                    |> sprintf "#%s"
                    |> Colors.ofString
                    |> Brushes.ofColor
                    |> Shapes.withFill

                setColor gray
            )
            |> C.withLabel "GrayLevel"
        ]
        |>! C.withBackground (Colors.fromCode 0xECE9D8)

        Canvas.create [
            stripes
            gray
        ]
        -+ Transform.flipY
        -+ Transform.scale (fromMili 1.0<mm>) (fromMili 1.0<mm>)
        |>! Elements.onLoad (fun c ->
            let w, h = c.ActualWidth / 2.0, c.ActualHeight / 2.0
            c -+ Transform.translate w h
            |> ignore
        )
    ]
)
|> Window.show