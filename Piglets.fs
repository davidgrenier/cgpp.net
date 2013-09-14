module Piglets

open System.Reactive.Subjects
open System.Reactive.Linq
open FSharpx.Prelude
open System

type Writer<'a> = IObserver<'a>

type System.IObserver<'a> with
    member x.Write v = x.OnNext v

module Reader =
    module Observable = FSharp.Control.Observable
    type T<'a> = System.IObservable<'a>

    let add = Observable.add
    let pairwise = Observable.pairwise
    let filter = Observable.filter
    let choose = Observable.choose
    let map = Observable.map
    let mapi = Observable.mapi
    let scan = Observable.scan
    let zip x = flip Observable.zip x
    let zipLatest x = flip Observable.combineLatest x
    let take (n: int) reader = Observable.Take(reader, n)
    let head = Observable.FirstAsync
    let bind (f: 'a -> T<'b>) (reader: T<'a>) = Observable.SelectMany(reader, f)

    let zipWithSeq (xs: _ seq) reader = Observable.Zip(reader, xs, fun a b -> a, b)
    let merge (reader2: T<_>) (reader1: T<_>) = reader1.Merge reader2

    let wireInto f (writer: Writer<_>) (reader: T<_>) = reader.Add (f >> writer.Write)

module Stream =
    type T<'a> = ISubject<'a>

    let create x = new BehaviorSubject<_>(x)

    let empty<'a> : T<'a> =
        let sub = create None
        {
            new T<'a>
                interface IObserver<'a> with
                    member x.OnNext v = sub.OnNext (Some v)
                    member x.OnError e = sub.OnError e
                    member x.OnCompleted() = sub.OnCompleted()
                interface Reader.T<'a> with
                    member x.Subscribe obs = sub.Subscribe (Option.iter obs.OnNext)
        }

    let ofReader (reader: Reader.T<_>) =
        empty |>! fun s -> reader.Add s.Write

    let map f stream = Reader.map f stream |> ofReader

type Piglet<'a, 'v> =
    {
        Stream: Stream.T<'a>
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

let YieldEmpty<'a, 'b> : Piglet<'a, (Stream.T<'a> -> 'b) -> 'b> =
    yeld Stream.empty<'a>

let (<*>) pig1 pig2 =
    {
        Stream =
            pig1.Stream
            |> Reader.zipLatest pig2.Stream
            |> Reader.map (fun (f, v) -> f v)
            |> Stream.ofReader
        ViewBuilder = pig1.ViewBuilder >> pig2.ViewBuilder
    }
    
let Render f piglet = piglet.ViewBuilder f

let WithSubmit piglet =
    let submit = YieldEmpty<unit, _>
    let s =
        piglet.Stream
        |> Reader.map Some
        |> Reader.merge(submit.Stream |> Reader.map (konst None))
        |> Reader.pairwise
        |> Reader.choose (function a, None -> a | _ -> None)
        |> Stream.ofReader

    {
        Stream = s
        ViewBuilder = piglet.ViewBuilder >> submit.ViewBuilder
    }

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

    let input (stream: Stream.T<_>) =
        TextBox()
        |>! fun tb ->
            stream.Add tb.set_Text
            tb.TextChanged.Add(fun _ -> stream.Write tb.Text)

    let radio text (stream: Stream.T<_>) =
        RadioButton(Content = text)
        |>! fun rb ->
            stream.Add (fun isChecked -> rb.IsChecked <- System.Nullable isChecked)
            rb.Checked.Add(fun _ -> stream.Write true)
            rb.Unchecked.Add(fun _ -> stream.Write false)

    let slider min increment max (value: Stream.T<_>) =
        Slider.create min increment max
        |>! fun slider ->
            value |> Reader.head |> Reader.add slider.set_Value
            slider.ValueChanged.Add (fun e -> value.Write e.NewValue)

    let button text (counted: Writer<_>) =
        Button.create text
        |>! fun b ->
            b.Click
            |> Reader.mapi (fun i _ -> i + 1)
            |> Reader.add counted.Write

    let submit text (submit: Writer<_>) =
        Button.create text
        |>! Button.onClick submit.Write

    let placeHolder (reader: Reader.T<_>) =
        ContentControl()
        |>! fun content -> reader.Add content.set_Content

    let textBlock (reader: Reader.T<_>) =
        TextBlock()
        |>! fun tb -> reader.Add tb.set_Text

    let label (reader: Reader.T<_>) =
        Label()
        |>! fun lbl -> reader.Add lbl.set_Content

    let image (reader: Reader.T<_>) =
        Image()
        |>! fun img -> reader.Add img.set_Source

    let canvas (points: Writer<_>) =
        Canvas.T()
        |>! Canvas.onClick points.Write
        

module Shapes =
    open System.Windows.Shapes

    let polygon (reader: Reader.T<_>) =
        Polygon()
        |>! fun p -> reader.Add (Shapes.pointCollection >> p.set_Points)