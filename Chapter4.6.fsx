#load "Load.fsx"

module C = Controls
open C.Operators
open FSharpx

let poly () =
    Polygon.create []
    |>! Shapes.withStroke Brushes.Black
    |>! Shapes.withMiterLimit

let window _ =
    let smoothed =
        poly()
        |>! Shapes.withStroke Brushes.IndianRed
        |>! Shapes.withThickness 2.0

    let poly = poly()

    let addPoint (x, y) = Points.create x y |> poly.Points.Add

    let clearPoints () =
        poly.Points.Clear()
        smoothed.Points.Clear()

    let dualize () =
        let points = Polygon.points poly |> Seq.toList
        points.Tail @ [points.Head]
        |> Seq.zip points
        |> Seq.map (fun (p, next) -> p.X / 2.0 + next.X / 2.0, p.Y / 2.0 + next.Y / 2.0)
        |> Shapes.pointCollection
        |> poly.set_Points

    let ratio =
        Slider.create 0.0 0.01 1.0
        |>! C.withWidth 1e2
        |>! Slider.withToolTip Slider.BottomRight
        |>! Slider.initTo 0.25

    let (!) a b = ratio.Value * a + (1.0 - ratio.Value) * b
    let smooth ((x1, y1), (x2, y2)) = !x1 x2, !y1 y2
    let castle ((x1, y1), (x2, y2)) = !x2 x1, !y2 y1
    let scramble ((x1, y1), (x2, y2)) = !x2 x1, !y1 y2

    let subdivide style =
        let pointsOrDefault (target: Polygon.T) =
            match Seq.isEmpty target.Points with
            | true -> poly
            | _ -> target
            |> Polygon.points
            |> Seq.toList

        match pointsOrDefault smoothed with
        | [] | [_] -> failwith "Cannot subdivide when there are no points."
        | points ->
            let points = List.map Points.coords points

            points.Tail @ [points.Head]
            |> Seq.zip3 (List.last points :: List.dropLast points) points
            |> Seq.collect (fun (prev, cur, next) -> [prev, cur; next, cur])
            |> Seq.map style
            |> Shapes.pointCollection
            |> smoothed.set_Points

    let controls content =
        content
        |> Seq.cast<Elements.F>
        |> Seq.iter (fun x -> x |> C.withMargins 3 0 3 3)
        C.stackPanel content

    let smoothSelect, scrambleSelect, castleSelect =
        C.radioButton "Smooth" true
        , C.radioButton "Scramble" false
        , C.radioButton "Castle" false
        
    let subdivide () =
        match smoothSelect.IsChecked.Value, scrambleSelect.IsChecked.Value with
        | true, _ -> subdivide smooth
        | _, true -> subdivide scramble
        | _ -> subdivide castle
        
    C.dockPanel [
        C.menu [
            C.menuItem "File" [
                C.menuItem "Exit" []
            ]
        ]
        |>! C.dock Dock.Top

        C.controlPanel [
            ratio

            smoothSelect
            scrambleSelect
            castleSelect

            Button.create "Clear"
            |>! Button.onClick clearPoints
            Button.create "Subdivide"
            |>! Button.onClick subdivide
            Button.create "Dualize"
            |>! Button.onClick dualize
        ]
        |>! C.dock Dock.Left
        |>! C.withBackground (Colors.fromCode 0xECE9D8)
        
        Canvas.create [
            poly
            smoothed
        ]
        |>! Controls.withBackground Colors.Transparent
        |>! Canvas.onClick addPoint
    ]

window
|> Window.create -1e3 5e1 8e2 8e2
|> Window.show