#load "WPF.fsx"
open WPF

module C = Controls

open C.Operators
open Transform.Operators

let controlPanel content =
    C.dockPanel [
        C.textBlock "Controls"
        |>! C.withMargins 3 2 0 3

        C.stackPanel content
        |>! C.withBackground (Colors.fromCode 0xECE9D8)
        |>! C.dock Dock.Left
        |>! C.withZIndex 1
    ]