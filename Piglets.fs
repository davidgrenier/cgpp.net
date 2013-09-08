module Piglets

open FSharpx
open System.Reactive.Linq
open System.Reactive.Subjects

type Reader<'a> = System.IObservable<'a>
type Writer<'a> = System.IObserver<'a>
type Stream<'a> = ISubject<'a>

type System.IObserver<'a> with
    member x.Write v = x.OnNext v

type System.IObservable<'a> with
    member x.Current =
        x.MostRecent Unchecked.defaultof<_>
        |> Seq.head

module Stream =
    let create x = new BehaviorSubject<_>(x)

    let map f (stream: Stream<_>) =
        create Unchecked.defaultof<_>
        |>! fun s -> stream.Add (f >> s.Write)

    let zip (reader2: Reader<_>) (reader1: Reader<_>) =
        create Unchecked.defaultof<_>
        |>! fun s ->
            reader1.CombineLatest(reader2, fun x y -> x, y).Add s.Write

type Piglet<'a, 'v> =
    {
        Stream: Stream<'a>
        ViewBuilder: 'v
    }

let Return x =
    {
        Stream = Stream.create x
        ViewBuilder = id
    }

let Yield x =
    let s = Stream.create x
    {
        Stream = s
        ViewBuilder = fun f -> f s
    }

let (<*>) pig1 pig2 =
    let s1, s2 = pig1.Stream, pig2.Stream
    let s = Stream.map (fun v -> s1.Current v) s2
    
    s1.Add (fun f -> f s2.Current |> s.Write)
        
    {
        Stream = s
        ViewBuilder = pig1.ViewBuilder >> pig2.ViewBuilder
    }
    
let Render f piglet = piglet.ViewBuilder f

let Run f piglet =
    (piglet.Stream.Skip 1).Add f
    piglet

let map f pig =
    let s = Stream.map f pig.Stream
    {
        Stream = s
        ViewBuilder = fun f -> f s
    }

module Controls =
    open System.Windows.Controls

    module C = Controls

    let input (stream: Stream<_>) =
        TextBox()
        |>! fun tb ->
            stream.Add tb.set_Text
            tb.TextChanged.Add(fun _ -> stream.Write tb.Text)

    let radio text (stream: Stream<_>) =
        RadioButton(Content = text)
        |>! fun rb ->
            stream.Add (fun isChecked -> rb.IsChecked <- System.Nullable isChecked)
            rb.Checked.Add(fun _ -> stream.Write true)
            rb.Unchecked.Add(fun _ -> stream.Write false)

    let slider min increment max (stream: Stream<_>) =
        Slider.create min increment max
        |>! fun slider ->
            slider.Value <- stream.Current
            slider.ValueChanged.Add (fun e -> stream.Write e.NewValue)

    let button text (writer: Writer<_>) =
        Button.create text
        |>! Button.onClick writer.Write

    let placeHolder (reader: Reader<_>) =
        ContentControl()
        |>! fun content -> reader.Add content.set_Content

    let textBlock (reader: Reader<_>) =
        TextBlock()
        |>! fun tb -> reader.Add tb.set_Text

    let label (reader: Reader<_>) =
        Label()
        |>! fun lbl -> reader.Add lbl.set_Content

module Shapes =
    open System.Windows.Shapes

    let polygon (reader: Reader<_>) =
        Polygon()
        |>! fun p -> reader.Add (Shapes.pointCollection >> p.set_Points)