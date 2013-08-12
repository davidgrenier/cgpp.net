#load "WPF.fsx"
open WPF

open System.Windows.Media
open Transform.Operators

let withMargin element =
    element

let handPoints =
    [
        -0.015, 0.05
        0.015, 0.05
        0.01, -0.4
        0.0, -0.45
        -0.01, -0.4
    ]

let fancyHand () =
    let hand = Shapes.geometry handPoints
    let hole = Shapes.geometry [-0.0025, -0.05; 0.0025, -0.05; 0.0025, -0.45; -0.0025, -0.45]

    CombinedGeometry(GeometryCombineMode.Exclude, hand, hole)
    |> Shapes.path
    |>! Shapes.withFill Brushes.Navy

let getClock s m h secAngle minAngle hrsAngle =
    let margin = Transform.translate (1.0/8.0) (1.0/8.0)
    Canvas.create [
        Shapes.circle 0.5
        -+ margin
        |>! Shapes.withFill Brushes.LightGray

        Shapes.circle (1.0/64.0)
        -+ margin
        -+ Transform.translate 0.0 -0.5
        |>! Shapes.withFill Brushes.Black

        Polygon.create handPoints
        -+ margin
        -+ Transform.scale 0.3 1.1
        -+ Transform.rotate secAngle
        -+ Animations.fullRevoOver (Timespan.secs s)
        |>! Shapes.withFill Brushes.Red

        fancyHand ()
        -+ margin
        -+ Transform.rotate minAngle
        -+ Animations.fullRevoOver (Timespan.mins m)

        fancyHand ()
        -+ margin
        -+ Transform.scale 1.7 0.7
        -+ Transform.rotate hrsAngle
        -+ Animations.fullRevoOver (Timespan.hrs h)
    ]
    -+ Transform.scale (fromInch 3.0<inch>) (fromInch 3.0<inch>)
    -+ Transform.translate (fromInch 1.5<inch>) (fromInch 1.5<inch>)

let fastClock () =
    getClock 10 10 4 18e1 -9e1 3e1

let accurateClock () =
    getClock 60 60 24
        (float System.DateTime.Now.Second * 6.0)
        (float System.DateTime.Now.Minute * 6.0)
        (float System.DateTime.Now.Hour * 3e1)

Window.create -1e3 50.0 8e2 8e2 (fun _ ->
    let clockContent = Controls.content (fastClock())
    let replace content _ = clockContent.Content <- content()

    Controls.stackPanel [
        Controls.horizontalPanel [
            Controls.radioButton "Fast" true
            |>! Controls.withWidth 5e1
            |>! Button.onClick (replace fastClock)

            Controls.radioButton "Accurate" false
            |>! Button.onClick (replace accurateClock)
        ]
        clockContent
    ]
)
|> Window.show