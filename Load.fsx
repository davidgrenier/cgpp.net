#r "PresentationFramework"
#r "PresentationCore"
#r "WindowsBase"
#r "System.Xaml"
#r "UIAutomationTypes"
#load "Primitives.fs"
#load "WPF.fs"
#load "Controls.fs"

#r "lib\FSharpx.Core.dll"
#r "lib\ExtCore.dll"
#r "lib\Xceed.Wpf.Toolkit.dll"

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