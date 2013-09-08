module Piglets

open FSharpx

type Reader<'a> =
    abstract Current: 'a
    abstract Add: ('a -> unit) -> unit

type Writer<'a> =
    abstract Write: 'a -> unit

type Stream<'a> =
    inherit Reader<'a>
    inherit Writer<'a>

type Piglet<'a, 'v> =
    {
        Stream: Stream<'a>
        ViewBuilder: 'v
    }

module Stream =
    let create x =
        let evt = Event<_>()
        let readers = evt.Publish
        let current = ref x
        {
            new Stream<'a>
                interface Reader<'a> with
                    member __.Current = !current
                    member __.Add f = readers.Add f
                interface Writer<'a> with
                    member __.Write x = evt.Trigger x
        }

    let map f (stream: Stream<_>) =
        create (f stream.Current)
        |>! fun s -> stream.Add (f >> s.Write)

//    let join (stream: Stream<Stream<_>>) =
//        let s = create stream.Current.Current
//        let current = ref stream.Current
//        stream.Add(fun newStream -> current := newStream; 

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
    piglet.Stream.Add f
    piglet

let map f pig =
    let s = Stream.map f pig.Stream
    {
        Stream = s
        ViewBuilder = fun f -> f s
    }

module Controls =
    module C = Controls

    let input (stream: Stream<_>) =
        C.textBox stream.Current
        |>! fun tb ->
            stream.Add tb.set_Text
            tb.TextChanged.Add(fun _ -> stream.Write tb.Text)

    let textBlock (reader: Reader<_>) =
        C.textBlock reader.Current
        |>! fun tb ->
            reader.Add tb.set_Text

    let label (reader: Reader<_>) =
        C.label reader.Current
        |>! fun lbl ->
            reader.Add lbl.set_Content

    let radio text (stream: Stream<_>) =
        C.radioButton text stream.Current
        |>! fun rb ->
            rb.Checked.Add(fun _ -> stream.Write true)
            rb.Unchecked.Add(fun _ -> stream.Write false)

    let placeHolder (stream: Stream<_>) =
        C.content stream.Current
        |>! fun content ->
            stream.Add content.set_Content

    let slider min increment max (stream: Stream<_>) =
        Slider.create min increment max
        |>! fun slider ->
            slider.Value <- stream.Current
            slider |> Slider.onChange (konst stream.Write)

    let polygon (stream: Stream<_>) =
        Polygon.create stream.Current
        |>! fun p ->
            stream.Add (Shapes.pointCollection >> p.set_Points)

    let button text (stream: Stream<_>) =
        Button.create text
        |>! Button.onClick stream.Write