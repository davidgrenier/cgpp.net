#load "Load.fsx"

open Transform.Operators

module List =
    let rec phold f acc = function
        | [] -> acc
        | x::xs -> phold f (f acc x xs) xs

let inline mapP f min max = f min >> (*) (1.0 / (max - min))
let inline mapxF min (x, _, z) = 1.0 - (x/z - min)
let inline mapyF min (_, y, z) = y/z - min

let mapx = mapP mapxF -0.5 0.5
let mapy = mapP mapyF -0.5 0.5
let scale = 1e3
let mapPoint p = mapx p * scale, mapy p * scale

let inline (|Eq|_|) a = function
    | b when a = b -> Some ()
    | _ -> None

let twoEquals (x, y, z) =
    function
    | Eq x, Eq y, _ | Eq x, _, Eq z | _, Eq y, Eq z -> true
    | _ -> false

let cube() =
    let points =
        List.init 8 (fun x ->
            0.5 - (float (x &&& 1))
            , 1.5 - (float (x &&& 2 >>> 1))
            , 4.5 - (float (x &&& 4 >>> 2))
        )

    let edges =
        points
        |> List.phold (fun edges (x, y, z as p) points ->
            let newEdges =
                points
                |> List.choose (function
                    | q when twoEquals p q -> Some q
                    | _ -> None
                )
            (p, newEdges) :: edges
        ) []

    points, edges

let tetrahedron() =
    let points =
        let s = sqrt 2.0 / 4.0
        [
            -0.5 + 0.0, 0.5, 3.0
            -0.5 + s, -0.5, 3.0 - s
            -0.5 + -s, -0.5, 3.0 - s
            -0.5 + 0.0, -0.5, 3.0 + s
        ]

    let edges =
        points
        |> List.phold (fun edges p points ->
            points
            |> List.map (fun q -> p, q)
            |> List.append edges
        ) []

    points, edges

let vertexCircle (x, y) =
    Shapes.circle 3.0
    -+ Transform.translate x y
    |>! Shapes.withStroke Brushes.Red
    :> Elements.T

let drawModel f =
    let points, edges = f()

    Window.create -1e3 5e1 8e2 8e2 (fun _ ->
        Canvas.create [
            for p, q in edges ->
                (mapPoint p, mapPoint q)
                ||> Shapes.line
                |>! Shapes.withStroke Brushes.Black
                :> Elements.T

            for p in points ->
                vertexCircle(mapPoint p)
        ]
    )
    |> Window.show

drawModel (fun () ->
    let points, edges = cube()
    let edges =
        edges
        |> List.collect (fun (p, edgeEnds) -> edgeEnds |> List.map (fun q -> p, q))
    points, edges
)

drawModel tetrahedron

module Ex3_3 =
    let inline vec (x2, y2, z2) (x1, y1, z1) = x2 - x1, y2 - y1, z2 - z1
    let inline dot (x, y, z) (x', y', z') = x * x' + y * y' + z * z'
    let inline cross (vx, vy, vz) (ux, uy, uz) = uy * vz - uz*vy, uz*vx - ux*vz, ux*vy - uy*vx

    let faces =
        let cubePoints, _ = cube()
        let (x, y, z as p) = cubePoints.Head
        let q, rest =
            cubePoints.Tail
            |> List.fold (fun (q, rest) -> function
                | (Eq x, _, _) | (_, Eq y, _) | (_, _, Eq z) as r -> q, r :: rest
                | q -> q, rest
            ) (p, [])

        let findPlanePoints (x, y, z) o =
            let face1, face2, face3 =
                rest
                |> List.fold (fun (face1, face2, face3) (x', y', z' as point) ->
                    let (!) face v = function
                        | Eq v -> point :: face
                        | _ -> face
                    !face1 x x', !face2 y y', !face3 z z'
                ) ([], [], [])

            [face1; face2; face3]
            |> List.map (fun points ->
                (x, y, z) :: (points |> List.sortBy (twoEquals o)), (x, y, z) |> vec o
            )

        findPlanePoints p q
        @ findPlanePoints q p
        |> List.filter (function
            | p0 :: p1 :: p2 :: _, toCenter ->
                let (x, y, z) =
                    p0 |> vec p1
                    |> cross (p0 |> vec p2)
                let w =
                    match (x, y, z) |> dot toCenter with
                    | d when d > 0.0 -> -x, -y, -z
                    | _ -> x, y, z
                p0 |> dot w < 0.0
            | _ -> failwith "Shouldn't happen."
        )
        |> List.map fst
        |> List.map (function
            | [a; b; c; d] -> [b; a; c; d]
            | x -> x
        )

    fun _ ->
        Canvas.create [
            for face in faces do
                let face = face |> List.map mapPoint
                yield
                    Polygon.create face
                    |>! Shapes.withStroke Brushes.Black
                    :> _
                yield! face |> Seq.map vertexCircle
        ]
    |> Window.create -1e3 5e1 8e2 8e2
    |> Window.show

module Ex3_4 =
    let edges =
        cube()
        |> snd
        |> List.sortBy (fun ((_, _, z), edgeEnds) ->
            match edgeEnds with
            | [] -> -z, -z
            | _ -> -z, edgeEnds |> List.map (fun (_, _, z') -> -z') |> List.min
        )

    fun _ ->
        Canvas.create [
            for p, edgeEnds in edges do
                for p' in edgeEnds ->
                    (mapPoint p, mapPoint p')
                    ||> Shapes.line
                    |>! Shapes.withStroke Brushes.White
                    |>! Shapes.withThickness 6.0
                    :> _
                for p' in edgeEnds ->
                    (mapPoint p, mapPoint p')
                    ||> Shapes.line
                    |>! Shapes.withStroke Brushes.Black
                    :> _

            for p, _ in edges ->
                vertexCircle (mapPoint p)
        ]
    |> Window.create -1e3 5e1 8e2 8e2
    |> Window.show