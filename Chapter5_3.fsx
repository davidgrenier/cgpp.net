#load "WPF.fsx"
open WPF
open FSharpx

module C = Controls

open C.Operators
open Transform.Operators

type Visibility = System.Windows.Visibility

Window.create -1e3 5e1 8e2 8e2 (fun _ ->
    let dot() = Shapes.dot 1.0 Brushes.Black
    let set1 =
        Canvas.create [
            dot()
            dot() -+ Transform.translate 4.0 0.0
            dot() -+ Transform.translate 8.0 0.0
        ]

    let set2, adjust =
        let canvas =
            Canvas.create []
            |>! fun x -> x.Visibility <- Visibility.Hidden
        let adjust n =
            let t = Transform.translate n 0.0
            canvas.Children.Clear()
            canvas -< [
                dot() -+ t
                dot() -+ Transform.translate 4.0 0.0 -+ t
                dot() -+ Transform.translate 8.0 0.0 -+ t
            ] |> ignore
        adjust 2.0
        canvas, adjust

    let flip = function
        | Visibility.Visible -> Visibility.Hidden
        | _ -> Visibility.Visible

    async {
        let context = System.Threading.SynchronizationContext.Current
        while true do
            do! Async.Sleep 250
            do! Async.SwitchToContext context
            set1.Visibility <- flip set1.Visibility
            set2.Visibility <- flip set2.Visibility
    }
    |> Async.StartImmediate

    C.dockPanel [
        C.stackPanel [
            Slider.create 12.0 4.0
            |>! C.withWidth 1e2
            |>! Slider.snaps
            |>! Slider.withTick Slider.Placement.BottomRight
            |>! Slider.withToolTip Slider.Placement.BottomRight
            |>! Slider.onChange (fun _ -> adjust)
            |> C.withLabel "Distance"
            |>! C.withMargins 3 3 0 3
        ]
        |>! C.withBackground (Colors.fromCode 0xECE9D8)
        |>! C.dock Dock.Left

        Canvas.create [
            set1
            set2
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