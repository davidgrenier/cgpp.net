#load "Load.fsx"

module C = Controls
open C.Operators
open Piglets

let margin x = C.withMargins 3 0 3 3 x

Window.create -1e3 5e1 8e2 8e2 (fun _ ->
    let skewer =
        Return (fun x y -> x, y)
        <*> Yield 0.0
        <*> Yield 0.0

    let slider stream =
        Controls.slider 0.0 2.0 2e1 stream
        |>! C.withWidth 1e2
        |>! Slider.snaps
        |>! Slider.withToolTip Slider.BottomRight
        |>! Slider.withTick Slider.BottomRight
        |>! margin

    skewer
    |> Render (fun x y ->
        C.dockPanel [
            C.menu [
                C.menuItem "File" [
                    C.menuItem "New" []
                    C.menuItem "Open" []
                ]
                C.menuItem "Edit" []
            ]
            |>! C.dock Dock.Top
        
            C.controlPanel [
                C.horizontalPanel [
                    C.stackPanel [
                        Yield ()
                        |> Run (fun _ -> printfn "test")
                        |> Render (Controls.button "Next ")
                        |>! margin

                        Yield ()
                        |> Run (fun _ -> System.Windows.MessageBox.Show "Clicked" |> ignore)
                        |> Render (Controls.button "Do it ")
                        |>! margin
                    ]

                    C.stackPanel [
                        slider x
                        slider y
                    ]
                ]
            ]
        
            Graph.create [
                x
                |> Stream.zip y
                |> Stream.map (fun (x, y) ->
                    [
                        x, 10.0
                        10.0, y
                        x, -10.0
                        -10.0, y
                    ]
                )
                |> Shapes.polygon
                |>! Shapes.withStroke Brushes.Black
                |>! Shapes.withFill Brushes.LightSeaGreen
            
                Shapes.dot 1.5 Brushes.LightPink
                |>! Animations.backAndForth (-4e1, 6e1) (0.0, 0.0) (Timespan.secs 3)
            ]
            |>! Graph.withAxes
        ]
    )
)
|> Window.show