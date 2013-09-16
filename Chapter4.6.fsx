#load "Load.fsx"

module C = Controls
open C.Operators
open FSharpx
open Piglets

let scramble (!) (x1, y1) (x2, y2) = !x2 x1, !y1 y2
let smooth (!) (x1, y1) (x2, y2) = !x1 x2, !y1 y2

let subDivideFor (!) points n =
    let rec subdivide points depth =
        match depth, Seq.toList points with
        | 0, _ | _, [] -> points
        | n, ps ->
            ps.Tail @ [ps.Head]
            |> Seq.zip3 (List.last ps :: List.dropLast ps) ps
            |> Seq.collect (fun (prev, cur, next) -> [!prev cur; !next cur])
            |> flip subdivide (n - 1)

    subdivide points n

let dual points =
    List.tail points @ [points.Head]
    |> Seq.zip points
    |> Seq.map (fun ((x, y), (nextx, nexty)) -> (x + nextx) / 2.0, (y + nexty) / 2.0)
    |> Seq.toList

type Action =
    | Clear
    | Dualize
    | AddPoint of (float * float)

Window.create 8e1 5e1 8e2 8e2 (fun _ ->
    let point = Stream.empty
    let ratio = Stream.create 0.25
    let subdivide = Stream.empty
    let adjuster = Stream.empty
    let act = Stream.create Clear
    let acts =
        point
        |> Reader.map AddPoint
        |> Reader.merge act
    
    let points =
        acts
        |> Reader.scan (fun ps ->
            function
            | Clear -> []
            | Dualize -> dual ps
            | AddPoint p -> p :: ps
        ) []
        |> Stream.ofReader

    let subdivided =
        subdivide
        |> Reader.mapi (fun i () -> i % 7 + 1)
        |> Reader.zipLatest acts
        |> Reader.bind (fun (depth, act) ->
            let depth =
                match act with
                | Clear -> 0
                | _ -> depth
            Reader.head points
            |> Reader.zipLatest ratio
            |> Reader.zipLatest adjuster
            |> Reader.map (fun ((points, ratio), adjuster) ->
                let adjust = adjuster (fun a b -> ratio * a + (1.0 - ratio) * b)
                subDivideFor adjust points depth
            )
        )
    
    C.controlPanel [
        Controls.button "Clear" Clear act
        Controls.button "Dualize" Dualize act

        Controls.radio adjuster [
            "Smooth", smooth
            "Scramble", scramble
        ]
        |> Controls.stackPanel

        Controls.slider 0.0 0.01 1.0 ratio
        |>! C.withWidth 1e2
        |>! Slider.withToolTip Slider.BottomRight

        Controls.button "Subdivide" () subdivide
    ] -- (

        Controls.canvas point
        -< [
            Shapes.polygon points
            
            Shapes.polygon subdivided
            |>! Shapes.withStroke Brushes.IndianRed
            |>! Shapes.withThickness 2.0
        ]
    )
)
|> Window.show