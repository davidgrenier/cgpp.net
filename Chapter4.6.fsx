#load "Load.fsx"

module C = Controls
open C.Operators
open FSharpx
open Piglets

Window.create -1e3 5e1 8e2 8e2 (fun _ ->
//    let smoothed =
//        Polygon.create []
//        |>! Shapes.withMiterLimit
//        |>! Shapes.withStroke Brushes.IndianRed
//        |>! Shapes.withThickness 2.0

//    let clearPoints () =
//        poly.Points.Clear()
//        smoothed.Points.Clear()
//
//    let dualize () =
//        let points = Polygon.points poly |> Seq.toList
//        points.Tail @ [points.Head]
//        |> Seq.zip points
//        |> Seq.map (fun (p, next) -> p.X / 2.0 + next.X / 2.0, p.Y / 2.0 + next.Y / 2.0)
//        |> Shapes.pointCollection
//        |> poly.set_Points
//
//    let ratio =
//        Slider.create 0.0 0.01 1.0
//        |>! C.withWidth 1e2
//        |>! Slider.withToolTip Slider.BottomRight
//        |>! Slider.initTo 0.25
//
//    let (!) a b = ratio.Value * a + (1.0 - ratio.Value) * b
//    let smooth ((x1, y1), (x2, y2)) = !x1 x2, !y1 y2
//    let scramble ((x1, y1), (x2, y2)) = !x2 x1, !y1 y2
//
//    let subdivide style =
//        let pointsOrDefault (target: Polygon.T) =
//            match Seq.isEmpty target.Points with
//            | true -> poly
//            | _ -> target
//            |> Polygon.points
//            |> Seq.toList
//
//        match pointsOrDefault smoothed with
//        | [] | [_] -> failwith "Cannot subdivide when there are no points."
//        | points ->
//            let points = List.map Points.coords points
//
//            points.Tail @ [points.Head]
//            |> Seq.zip3 (List.last points :: List.dropLast points) points
//            |> Seq.collect (fun (prev, cur, next) -> [prev, cur; next, cur])
//            |> Seq.map style
//            |> Shapes.pointCollection
//            |> smoothed.set_Points
//
//    let smoothSelect, scrambleSelect =
//        C.radioButton "Smooth" true
//        , C.radioButton "Scramble" false
//        
//    let subdivide () =
//        match smoothSelect.IsChecked.Value with
//        | true -> subdivide smooth
//        | _ -> subdivide scramble
    
    YieldEmpty
    |> WithSubmit
    |> Render (fun point clear ->
        C.controlPanel [
            Controls.submit "Clear" clear
        ]
        -< [
            Controls.canvas point
            -< [
                point
                |> Reader.scan (fun ps p -> p :: ps) []
                |> Shapes.polygon
                |>! Shapes.withStroke Brushes.Black
                |>! Shapes.withMiterLimit
            ]
            |>! Controls.withBackground Colors.Transparent
        ]
    )
        
//    C.controlPanel [
//        ratio
//
//        smoothSelect
//        scrambleSelect
//
//        Button.create "Clear"
//        |>! Button.onClick clearPoints
//        Button.create "Subdivide"
//        |>! Button.onClick subdivide
//        Button.create "Dualize"
//        |>! Button.onClick dualize
//    ] -- (canvas -- smoothed)
)
|> Window.show