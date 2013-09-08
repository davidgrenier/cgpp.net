#r "PresentationFramework"
#r "PresentationCore"
#r "WindowsBase"
#r "System.Xaml"
#r "UIAutomationTypes"
#r "System.Runtime"

#I @"C:\Bib\Projects\cgpp.net\packages\"
#r @"FSharpx.Core.1.8.38\lib\40\FSharpx.Core.dll"
#r @"Extended.Wpf.Toolkit.2.0.0\lib\net40\Xceed.Wpf.Toolkit.dll"
#r @"Rx-Interfaces.2.1.30214.0\lib\Net45\System.Reactive.Interfaces.dll"
#r @"Rx-Core.2.1.30214.0\lib\Net45\System.Reactive.Core.dll"
#r @"Rx-PlatformServices.2.1.30214.0\lib\Net45\System.Reactive.PlatformServices.dll"
#r @"Rx-Linq.2.1.30214.0\lib\Net45\System.Reactive.Linq.dll"
#r @"Rx-XAML.2.1.30214.0\lib\Net45\System.Reactive.Windows.Threading.dll"

#load "Primitives.fs"
#load "WPF.fs"
#load "Controls.fs"
#load "Piglets.fs"

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