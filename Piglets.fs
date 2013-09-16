module Piglets

open System.Reactive.Subjects
open System.Reactive.Linq
open FSharpx.Prelude
open System

type Writer<'a> = IObserver<'a>

type IObserver<'a> with
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
    let startWith v (reader: T<_>) = Observable.StartWith(reader, [v])

    let bind (f: 'a -> T<'b>) (reader: T<'a>) = Observable.SelectMany(reader, f)
    let distinctUntilChanged = Observable.DistinctUntilChanged
    let tail reader = Observable.Skip(reader, 1)
    let append second first = Observable.Concat(first, second)

    let Do f (reader: T<_>) = reader.Do(fun x -> f x)

    let zipWithSeq (xs: _ seq) reader = Observable.Zip(reader, xs, fun a b -> a, b)
    let merge (reader2: T<_>) (reader1: T<_>) = reader1.Merge reader2

    let into (writer: Writer<_>) f (reader: T<_>) = reader.Add (f >> writer.Write)
    
module Stream =
    type T<'a> = ISubject<'a>

    let create x = new BehaviorSubject<_>(x)

    let empty<'a> =
        let subject = create None
        {
            new T<'a>
                interface IObserver<'a> with
                    member x.OnNext v = subject.Write (Some v)
                    member x.OnError e = subject.OnError e
                    member x.OnCompleted() = subject.OnCompleted()
                interface Reader.T<'a> with
                    member x.Subscribe writer = subject.Subscribe (Option.iter writer.Write)
        }

    let ofReader (reader: Reader.T<_>) =
        empty |>! fun s -> reader |> Reader.into s id

    let map f stream = Reader.map f stream |> ofReader
    let mapi f stream = Reader.mapi f stream |> ofReader

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
    let submit = YieldEmpty
    let clicked =
        submit.Stream
        |> Reader.bind (fun _ -> Reader.head piglet.Stream)

    {
        Stream = Stream.ofReader clicked
        ViewBuilder = piglet.ViewBuilder >> submit.ViewBuilder
    }

let Run f piglet =
    piglet.Stream.Add f
    piglet

let map f pig =
    Stream.map f pig.Stream
    |> yeld

let mapi f pig =
    Stream.mapi f pig.Stream
    |> yeld

module Content =
    open System.Windows.Controls

    let empty<'a> = ContentControl()

    let label() = Label()
    
    let show (reader: Reader.T<_>) f (content: #ContentControl) =
        reader.Add (f >> content.set_Content)
        content

module Controls =
    open System.Windows.Controls

    module C = Controls

    let input (stream: Stream.T<_>) =
        TextBox()
        |>! fun tb ->
            stream.Add tb.set_Text
            tb.TextChanged
            |> Reader.into stream (fun _ -> tb.Text)

    let radio (writer: Writer<_>) =
        Seq.mapi (fun i (label, value) ->
            RadioButton(Content = label)
            |>! fun rb ->
                if i = 0 then
                    rb.IsChecked <- System.Nullable true
                    writer.Write value

                rb.Checked
                |> Reader.into writer (konst value)
        )

    let slider min increment max value =
        Slider.create min increment max
        |>! fun slider ->
            Reader.head value
            |> Reader.add slider.set_Value

            slider.ValueChanged
            |> Reader.into value (fun e -> e.NewValue)

    let button text value clicks =
        Button.create text
        |>! fun b ->
            b.Click
            |> Reader.into clicks (konst value)

    let submit text submit =
        Button.create text
        |>! fun b ->
            b.Click
            |> Reader.into submit ignore
        
    let textBlock (reader: Reader.T<_>) =
        TextBlock()
        |>! fun tb -> reader.Add tb.set_Text

    let image (reader: Reader.T<_>) =
        Image()
        |>! fun img -> reader.Add img.set_Source

    let canvas (points: Writer<_>) =
        Canvas.T()
        |>! Canvas.onClick points.Write
        |>! Controls.withBackground Colors.Transparent
        

module Shapes =
    open System.Windows.Shapes

    let polygon (reader: Reader.T<_>) =
        Polygon()
        |>! fun p -> reader.Add (Shapes.pointCollection >> p.set_Points)
        |>! Shapes.withStroke Brushes.Black
        |>! Shapes.withMiterLimit