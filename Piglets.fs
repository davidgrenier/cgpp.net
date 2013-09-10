module Piglets

open FSharpx
open System
open System.Reactive.Linq
open System.Reactive.Subjects

type Reader<'a> = System.IObservable<'a>
type Writer<'a> = System.IObserver<'a>
type Stream<'a> = ISubject<'a>

type System.IObserver<'a> with
    member x.Write v = x.OnNext v

module Reader =
    let map = Observable.map
    let zipWithSeq (xs: _ seq) reader = Observable.Zip(reader, xs, fun a b -> a, b)

module Stream =
    let createEmpty<'a>() =
        let sub = new BehaviorSubject<'a option>(None)
        {
            new ISubject<'a>
                interface IObserver<'a> with
                    member x.OnNext v = sub.OnNext (Some v)
                    member x.OnError e = sub.OnError e
                    member x.OnCompleted() = sub.OnCompleted()
                interface IObservable<'a> with
                    member x.Subscribe obs =
                        sub.Subscribe (Option.iter obs.OnNext)
        }

    let create x = new BehaviorSubject<_>(x)

    let map f (stream: Stream<_>) =
        createEmpty()
        |>! fun s -> stream.Add (f >> s.Write)

    let zipLatest (reader2: Reader<_>) (reader1: Reader<_>) =
        createEmpty()
        |>! fun s ->
            reader1.CombineLatest(reader2, fun a b -> a, b).Add s.Write

type Piglet<'a, 'v> =
    {
        Stream: Stream<'a>
        ViewBuilder: 'v
    }

let private retrn s =
    {
        Stream = s
        ViewBuilder = id
    }

let private yeld s =
    {
        Stream = s
        ViewBuilder = fun f -> f s
    }

let Return x =
    Stream.create x
    |> retrn

let Yield x =
    Stream.create x
    |> yeld

let (<*>) pig1 pig2 =
    {
        Stream =
            pig1.Stream
            |> Stream.zipLatest pig2.Stream
            |> Stream.map (fun (f, v) -> f v)
        ViewBuilder = pig1.ViewBuilder >> pig2.ViewBuilder
    }
    
let Render f piglet = piglet.ViewBuilder f

let WithSubmit piglet =
    let sub = Stream.createEmpty<unit>()
    Return (fun a _ -> a)
    <*> piglet
    <*> yeld sub

let Run f piglet =
    piglet.Stream.Add f
    piglet

let map f pig =
    pig.Stream
    |> Stream.map f
    |> yeld

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
            slider.ValueChanged.Add (fun e -> stream.Write e.NewValue)

    let button text (stream: Stream<_>) =
        let count = ref 1
        Button.create text
        |>! Button.onClick (fun () ->
            stream.Write !count
            incr count
        )

    let submit text (submit: Stream<_>) =
        Button.create text
        |>! Button.onClick (fun () -> printfn "submitting"; submit.Write ())

    let placeHolder (reader: Reader<_>) =
        ContentControl()
        |>! fun content -> reader.Add content.set_Content

    let textBlock (reader: Reader<_>) =
        TextBlock()
        |>! fun tb -> reader.Add tb.set_Text

    let label (reader: Reader<_>) =
        Label()
        |>! fun lbl -> reader.Add lbl.set_Content

    let image (reader: Reader<_>) =
        Image()
        |>! fun img -> reader.Add img.set_Source

module Shapes =
    open System.Windows.Shapes

    let polygon (reader: Reader<_>) =
        Polygon()
        |>! fun p -> reader.Add (Shapes.pointCollection >> p.set_Points)