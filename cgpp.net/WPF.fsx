#r "PresentationFramework"
#r "PresentationCore"
#r "WindowsBase"
#r "System.Xaml"
#r "UIAutomationTypes"
#r @"C:\Bib\Projects\FSharpx\Build\FSharpx.Core.dll"
#r @"C:\Bib\Projects\ExtCore\ExtCore\bin\Release\ExtCore.dll"
#r @"C:\Bib\Projects\lib\Xceed.Wpf.Toolkit.dll"

open FSharpx

[<AutoOpen>]
module Primitives =
    let (|>!) e f = f e; e

    [<Measure>]
    type mm

    [<Measure>]
    type inch

    let fromInch (distance: float<inch>) = distance * 96.0<inch^-1>

    let fromMili (distance: float<mm>) = distance / 25.4 * 96.0<mm^-1>
    
    let pi = System.Math.PI

module WPFEventLoop =
    open System.Windows
    open System.Windows.Threading
    open Microsoft.FSharp.Compiler.Interactive

    type RunDelegate<'b> = delegate of unit -> 'b
    let create () =
        let app  =
            try
                Application()
                |>! fun _ -> (Window() |> ignore)
            with :? System.InvalidOperationException -> Application.Current
        let disp = app.Dispatcher
        {
            new IEventLoop with
                member x.Run() =
                    app.Run() |> ignore
                    false

                member x.Invoke f =
                    try
                        disp.Invoke(DispatcherPriority.Send, RunDelegate(f >> box)) |> unbox
                    with e ->
                        eprintf "\n\n ERROR: %O\n" e
                        reraise()

                member x.ScheduleRestart() = ()
         }

fsi.EventLoop <- WPFEventLoop.create()

open System.Windows
open System.Windows.Media

module Points =
    type T = Point

    let coords (p: T) = p.X, p.Y
    let create x y = T(x, y)

module Elements =
    type T = UIElement
    type F = FrameworkElement

    let onLoad f (element: #F) =
        element.Loaded |> Event.add (fun _ -> f element)

module Window =
    let create left top width height f =
        Window (Left = left, Top = top, Width = width, Height = height)
        |>! fun w -> w.Content <- f w

    let show =
        let win = ref None : Window option ref
        fun (window: Window) ->
            !win |> Option.iter (fun x -> x.Close())
            win := Some window
            window.Show()

    
    let onClick f (w: Window) =
        w.MouseLeftButtonUp
        |> Event.add (fun e ->
            let p = e.GetPosition w
            f (p.X, p.Y)
        )

module Transform =
    let scale scaleX scaleY = ScaleTransform(scaleX, scaleY)
    let rotate deg = RotateTransform deg
    let translate offsetX offsetY = TranslateTransform(offsetX, offsetY)
    let flipY = scale 1.0 -1.0

    [<AutoOpen>]
    module Operators =
        let inline (-+) (element: #Elements.T) transform =
            match element.RenderTransform with
            | :? TransformGroup as ts ->
                ts.Children.Add transform
            | _ ->
                element.RenderTransform <- TransformGroup(Children = TransformCollection [transform])
            element

module Shapes =
    open System.Windows.Shapes

    type T = Shape
    type Ellipse = System.Windows.Shapes.Ellipse
    type Path = System.Windows.Shapes.Path

    let path data = Path(Data = data)

    let ellipseGeometry width height = EllipseGeometry(RadiusX = width, RadiusY = height)
    let circleGeometry size = ellipseGeometry size size

    let ellipse width = ellipseGeometry width >> path
    let circle size = ellipse size size
    
//    let rectangleGeometry width height = RectangleGeometry(Rect(0.0, 0.0, width, height))
//    let rectangle width height = Rectangle(Width = width, Height = height)
//    let square size = rectangle size size

    let pointCollection points = PointCollection (Seq.map (uncurry Points.create) points)

    let withFill brush (shape: T) = shape.Fill <- brush

    let withStroke brush (shape: T) = shape.Stroke <- brush

    let segment a b = LineSegment(Points.create a b, false)

    let line (x1, y1) (x2, y2) = Line (X1 = x1, Y1 = y1, X2 = x2, Y2 = y2)

    let withThickness size (shape: T) = shape.StrokeThickness <- size

    let withMiterLimit (x: T) = x.StrokeMiterLimit <- 1.0

    let geometry =
        let geo points =
            let points = points |> List.map (uncurry Points.create)

            let segments = PolyLineSegment(points, false) :> PathSegment
            let pathFigure = PathFigure(Segments = PathSegmentCollection [segments])
            PathGeometry(Figures = PathFigureCollection [pathFigure])
        function
        | [] -> geo []
        | h::_ as points -> geo (points @ [h])

    let lineGeometry a b = LineGeometry(uncurry Points.create a, uncurry Points.create b)

    let dot size brush =
        circle size
        |>! withFill brush
        |>! withStroke Brushes.Black
        |>! withThickness 0.3

    let carveWith carveGeo geo =
        CombinedGeometry(GeometryCombineMode.Exclude, geo, carveGeo)

module Controls =
    open System.Windows.Controls

    let inline childrenProp arg = (^a : (member Children : UIElementCollection) arg)
    let inline itemsProp arg = (^a : (member Items : ItemCollection) arg)
    let inline withBackground color e = (^a : (member set_Background : Brush -> unit) (e, SolidColorBrush color))
    let inline withImage source e = (^a : (member set_Background : Brush -> unit) (e, System.Windows.Media.ImageBrush source))

    let inline addItems items element =
        let itemCollection = itemsProp element
        items |> Seq.iter (itemCollection.Add >> ignore)

    let inline addChildren (children: #Elements.T list) element =
        let childrenCollection = childrenProp element
        children |> Seq.iter (childrenCollection.Add >> ignore)

    [<AutoOpen>]
    module Operators =
        let inline (-<) element children = addChildren children element; element
        let inline (--) element child = element -< [child]

    let stackPanel (elements: Elements.T list) =
        StackPanel() -< elements

    let horizontalPanel = stackPanel >> fun x -> x.Orientation <- Orientation.Horizontal; x

    let content content = ContentControl(Content = content)

    let radioButton text check =
        RadioButton(Content = text, IsChecked = System.Nullable check)

    let withWidth width (arg: FrameworkElement) = arg.Width <- width

    let dockPanel (content: Elements.T list) = DockPanel() -< content

    let withLastChildFill (d: DockPanel) =
        d.LastChildFill <- true

    let menu content =
        Menu()
        |>! addItems content

    let menuItem label content =
        MenuItem(Header = label)
        |>! addItems content

    let button label = Button(Content = label)
    
    let align align (e: Elements.F) =
        e.HorizontalAlignment <- align

    let dock dock control =
        DockPanel.SetDock(control, dock)
        
    let textBlock text =
        TextBlock(Text = text)

    let withMargins left top right bottom (e: Elements.F) =
        e.Margin <- System.Windows.Thickness(float left, float top, float right, float bottom)

    let withLeftMargin size (e: Elements.F) =
        e.Margin <- System.Windows.Thickness(float size, e.Margin.Top, e.Margin.Right, e.Margin.Bottom)

    let withTopMargin size (e: Elements.F) =
        e.Margin <- System.Windows.Thickness(e.Margin.Left, float size, e.Margin.Right, e.Margin.Bottom)

    let withLabel text control =
        horizontalPanel [
            Label(Content = text)
            control
        ]
    
    let withZIndex z e = System.Windows.Controls.Panel.SetZIndex(e, z)

    let colorPicker color f =
        Xceed.Wpf.Toolkit.ColorPicker(SelectedColor = color)
        |>! fun x ->
            x.SelectedColorChanged
            |> Event.add (fun x -> f x.NewValue)

module Button =
    open System.Windows.Controls.Primitives
    
    let onClick f (b: ButtonBase) = b.Click |> Event.add (ignore >> f)

module Animations =
    open System.Windows.Media.Animation

    let duration ts = Duration ts

    let fullRevo start finish timespan =
        let rotation = DoubleAnimation(start, finish, duration timespan, RepeatBehavior = RepeatBehavior.Forever)

        Transform.rotate 0.0
        |>! fun t -> t.BeginAnimation(RotateTransform.AngleProperty, rotation)

    let fullRevoOver = fullRevo 0.0 36e1

    let backAndForth (x1, y1) (x2, y2) ts (shape: Shapes.Path) =
        let pa =
            PointAnimation (
                Points.create x1 y1,
                Points.create x2 y2,
                System.Windows.Duration ts,
                AutoReverse = true,
                RepeatBehavior = RepeatBehavior.Forever
            )

        shape.Data.BeginAnimation(EllipseGeometry.CenterProperty, pa)

module Slider =
    open System.Windows.Controls
    open System.Windows.Controls.Primitives

    type T = Slider
    type Placement =
        | BottomRight

    let create maxLimit frequency =
        let tickDigits =
            frequency
            |> sprintf "%g"
            |> Seq.skipWhile (fun x -> x <> '.')
            |> Seq.length
            |> flip (-) 1
            |> max 0
        T(Maximum = maxLimit, TickFrequency = frequency, AutoToolTipPrecision = tickDigits)

    let withToolTip placement (slider: T) =
        match placement with
        | BottomRight ->
            slider.AutoToolTipPlacement <- AutoToolTipPlacement.BottomRight

    let withTick placement (slider: T) =
        match placement with
        | BottomRight -> slider.TickPlacement <- TickPlacement.BottomRight

    let snaps (slider: T) = slider.IsSnapToTickEnabled <- true

    let onChange f (slider: RangeBase) =
        slider.ValueChanged
        |> Event.add (fun e -> e.Handled <- true; f e.OldValue e.NewValue)

module Canvas =
    open Controls.Operators

    type T = System.Windows.Controls.Canvas

    let onClick f (c: T) =
        c.MouseLeftButtonUp
        |> Event.add (fun e ->
            let p = e.GetPosition c
            f (p.X, p.Y)
        )

    let create (elements: Elements.T list) =
        T ((*ClipToBounds = true*)) -< elements

module Polygon =
    open System.Windows.Shapes

    type T = Polygon

    let points (p: T) = p.Points |> Seq.cast<Points.T> |> Seq.toList

    let create points = T(Points = Shapes.pointCollection points)

    let arrow (x1, y1) (x2, y2) brush =
        let heel theta = x2 + 5.0 * cos theta, y2 + 5.0 * sin theta

        let slope =
            let width, height = x2 - x1, y2 - y1
            match width with
            | 0.0 -> pi / 2.0
            | _ -> atan (height / width)

        create [
            x1, y1
            x2, y2
            heel (11.0 * pi / 12.0 + slope)
            heel (13.0 * pi / 12.0 + slope)
            x2, y2
        ]
        |>! Shapes.withFill brush
        |>! Shapes.withStroke brush
        |>! Shapes.withThickness 0.4
        |>! Shapes.withMiterLimit

module Graph =
    open Controls
    open Transform.Operators

    let create content =
        Canvas.create content
        -+ Transform.scale (fromMili 1.0<mm>) (fromMili 1.0<mm>)
        -+ Transform.flipY
        |>! Elements.onLoad (fun c ->
            let w, h = c.ActualWidth / fromMili 2.0<mm>, c.ActualHeight / fromMili 2.0<mm>
            
            c |> addChildren [
                let gridLine a b =
                    Shapes.line a b
                    |>! Shapes.withStroke Brushes.LightBlue
                    |>! Shapes.withThickness 0.1

                for x in [0.0..(-1e1)..(-w)] @ [1e1..1e1..w] -> gridLine (x, -h) (x, h)
                for y in [0.0..(-1e1)..(-h)] @ [1e1..1e1..h] -> gridLine (-w, y) (w, y)
            ]
        )
        |>! Elements.onLoad (fun c ->
            let w, h = c.ActualWidth / 2.0, c.ActualHeight / 2.0
            c -+ Transform.translate w h
            |> ignore
        )

    let withAxes (c: Canvas.T) =
        c
        |> Elements.onLoad (fun c ->
            let w, h = c.ActualWidth / fromMili 2.0<mm>, c.ActualHeight / fromMili 2.0<mm>

            c |> addChildren [
                Polygon.arrow (0.0, -2e1) (0.0, 0.9 * h) Brushes.Black
                Polygon.arrow (-2e1, 0.0) (0.9 * w, 0.0) Brushes.Black
            ]
        )

module Timespan =
    type TS = System.TimeSpan
    let secs = float >> TS.FromSeconds
    let mins = float >> TS.FromMinutes
    let hrs = float >> TS.FromHours
    let days = float >> TS.FromDays

type System.Windows.Media.Colors with
    static member ofString =
        System.Windows.Media.ColorConverter.ConvertFromString
        >> unbox<System.Windows.Media.Color>
    static member fromCode =
        sprintf "#%06X"
        >> Colors.ofString

type System.Windows.Media.Brushes with
    static member fromCode code = SolidColorBrush(Colors.fromCode code)
    static member ofColor color = SolidColorBrush color

type Colors = System.Windows.Media.Colors
type Brushes = System.Windows.Media.Brushes
type Point = System.Windows.Point
type Dock = System.Windows.Controls.Dock
type Align = System.Windows.HorizontalAlignment