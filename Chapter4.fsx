#load "Load.fsx"

module C = Controls
open C.Operators

let menu() =
    C.menu [
        C.menuItem "File" [
            C.menuItem "New" []
            C.menuItem "Open" []
        ]
        C.menuItem "Edit" []
    ]
    |>! C.dock Dock.Top

let controls moveDiamond =
    let b1click() = printfn "test"
    let b2click() = ()
    let fishify (_: float) n = moveDiamond (fun i x y -> if i % 2 = 0 then n, y else x, y)
    let happyfy (_: float) n = moveDiamond (fun i x y -> if i % 2 = 1 then x, n else x, y)

    let slider() =
        Slider.create 2e1 2.0
        |>! C.withWidth 1e2
        |>! Slider.snaps
        |>! Slider.withToolTip Slider.BottomRight
        |>! Slider.withTick Slider.BottomRight
        |>! C.withMargins 3 0 3 3

    C.stackPanel [
        C.textBlock "Controls"
        |>! C.withMargins 3 0 3 3

        C.horizontalPanel [
            C.stackPanel [
                Button.create "Next "
                |>! Button.onClick b1click
                |>! C.withMargins 3 0 3 3

                Button.create "Do it "
                |>! Button.onClick b2click
                |>! C.withMargins 3 0 3 3
            ]

            C.stackPanel [
                slider()
                |>! Slider.onChange fishify

                slider()
                |>! Slider.onChange happyfy
            ]
        ]
    ]
    |>! C.dock Dock.Left
    |>! C.withBackground (Colors.fromCode 0xECE9D8)

let graph() =
    let diamond =
        Polygon.create [
            0.0, 1e1
            1e1, 0.0
            0.0, -1e1
            -1e1, 0.0
        ]
        |>! Shapes.withStroke Brushes.Black
        |>! Shapes.withFill Brushes.LightSeaGreen

    let moveDiamond f =
        diamond.Points.Clone()
        |> Seq.cast<Point>
        |> Seq.mapi (fun i p -> f i p.X p.Y)
        |> Shapes.pointCollection
        |> fun points -> diamond.Points <- points

    let graph =
        Graph.create [
            diamond
            
            Shapes.dot 1.5 Brushes.LightPink
            |>! Animations.backAndForth (-4e1, 6e1) (0.0, 0.0) (Timespan.secs 3)
        ]
        |>! Graph.withAxes

    graph, moveDiamond

let panel _ =
    let graph, moveDiamond = graph()

    C.dockPanel [
        menu()
        controls moveDiamond
        graph
    ]
    |>! Controls.withLastChildFill

panel
|> Window.create -1e3 5e1 8e2 8e2
|> Window.show