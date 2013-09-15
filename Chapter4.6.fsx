#load "Load.fsx"

module C = Controls
open C.Operators
open FSharpx
open Piglets

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
//
//    let smoothSelect, scrambleSelect =
//        C.radioButton "Smooth" true
//        , C.radioButton "Scramble" false
//        
//    let subdivide () =
//        match smoothSelect.IsChecked.Value with
//        | true -> subdivide smooth
//        | _ -> subdivide scramble

Window.create 8e1 5e1 8e2 8e2 (fun _ ->
    let point = Stream.empty
    let ratio = Stream.create 0.25
    let clear = Stream.create None
    let subdivide = Stream.empty
    
    C.controlPanel [
        Controls.button "Clear" clear
        Controls.button "Subdivide" subdivide

        Controls.slider 0.0 0.01 1.0 ratio
        |>! C.withWidth 1e2
        |>! Slider.withToolTip Slider.BottomRight
    ] -- (
        let points =
            point
            |> Reader.map Some
            |> Reader.merge clear
            |> Reader.scan (fun ps ->
                function
                | None -> []
                | Some p -> p :: ps
            ) []
            |> Stream.ofReader

        let subDivideFor points ratio n =
            let (!) a b = ratio * a + (1.0 - ratio) * b
            let smooth (x1, y1) (x2, y2) = !x1 x2, !y1 y2

            let rec subdivide points depth =
                match depth, Seq.toList points with
                | 0, _ | _, [] -> points
                | n, ps ->
                    ps.Tail @ [ps.Head]
                    |> Seq.zip3 (List.last ps :: List.dropLast ps) ps
                    |> Seq.collect (fun (prev, cur, next) -> [smooth prev cur; smooth next cur])
                    |> flip subdivide (n - 1)

            subdivide points n

        Controls.canvas point
        -< [
            Shapes.polygon points
            
            
            Reader.mapi (fun i _ -> i % 5 + 1) subdivide
            |> Reader.merge (Reader.tail clear |> Reader.map (konst 0))
            |> Reader.bind (fun depth ->
                Reader.head points
                |> Reader.zipLatest ratio
                |> Reader.map (fun (points, ratio) -> subDivideFor points ratio depth)
            )
            |> Shapes.polygon
            |>! Shapes.withStroke Brushes.IndianRed
            |>! Shapes.withThickness 2.0
        ]
    )
)
|> Window.show
        
//    C.controlPanel [
//        smoothSelect
//        scrambleSelect
//
//        Button.create "Dualize"
//        |>! Button.onClick dualize
//    ] -- (canvas -- smoothed)