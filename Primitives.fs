[<AutoOpen>]
module Primitives

let inline (|>!) e f = f e; e

[<Measure>]
type mm

[<Measure>]
type inch

let fromInch (distance: float<inch>) = distance * 96.0<inch^-1>

let fromMili (distance: float<mm>) = distance / 25.4 * 96.0<mm^-1>
    
let pi = System.Math.PI