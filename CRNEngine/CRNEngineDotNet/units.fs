namespace Microsoft.Research.CRNEngine

open Parser

[<JavaScript>]
[<WebSharper.NamedUnionCases>]
type Time = 
  | Seconds of Seconds:float 
  static member from_string = function
    | "nanoseconds" | "ns" -> Seconds 1E-9
    | "microseconds" | "us" -> Seconds 1E-6
    | "milliseconds" | "ms" -> Seconds 1E-3
    | "seconds" | "s" -> Seconds 1.0
    | "minutes" | "min" -> Seconds 60.0
    | "hours"   | "h" -> Seconds 3600.0
    | s -> failwith ("Unknown time unit: " + s)
  member time.to_string = 
    match time with
    | Seconds 1E-9 -> "ns"
    | Seconds 1E-6 -> "us"
    | Seconds 1E-3 -> "ms"
    | Seconds 1.0 -> "s"
    | Seconds 60.0 -> "min"
    | Seconds 3600.0 -> "h"
    | Seconds s -> sprintf "%fs" s 

[<JavaScript>]
[<WebSharper.NamedUnionCases>]
type Space = 
  | Metres of Metres:int
  static member from_string = function
    | "metres" | "meters" | "m" -> Metres 0
    | "millimetres" | "millimeters" | "mm" -> Metres -3
    | "micrometres" | "micrometers" | "uM" -> Metres -6
    | "nanometres"  | "nanometers"  | "nM" -> Metres -9
    | "picometres"  | "picometers"  | "pM" -> Metres -12
    | "femtometres" | "femtometers" | "fM" -> Metres -15
    | s -> failwith ("Unknown space unit: " + s)
  member space.to_string = 
    match space with
    | Metres   0 -> "m"
    | Metres  -3 -> "mm"
    | Metres  -6 -> "um"
    | Metres  -9 -> "nm"
    | Metres -12 -> "pm"
    | Metres -15 -> "fm"
    | Metres   n -> "m*10\^" + (string n)

[<JavaScript>]
[<WebSharper.NamedUnionCases>]
type Concentration = 
  | Molar of Molar:int
  static member from_string = function
    | "molar"      | "M"  -> Molar 0
    | "milimolar"  | "mM"                 // ND: This should be spelt "millimolar". I'm not changing because I don't know the consequences
    | "millimolar"        -> Molar -3     // CG : I've added the correct spelling, so at least we're not forcing people to use the wrong one
    | "micromolar" | "uM" -> Molar -6
    | "nanomolar"  | "nM" -> Molar -9
    | "picomolar"  | "pM" -> Molar -12
    | "femtomolar" | "fM" -> Molar -15
    | "attomolar"  | "aM" -> Molar -18
    | "zeptomolar" | "zM" -> Molar -21
    | "yoctomolar" | "yM" -> Molar -24
    | s -> failwith ("Unknown concentration unit: " + s)
  member concentration.to_string = 
    match concentration with
    | Molar   0 -> "M"
    | Molar  -3 -> "mM"
    | Molar  -6 -> "uM"
    | Molar  -9 -> "nM"
    | Molar -12 -> "pM"
    | Molar -15 -> "fM"
    | Molar -18 -> "aM"
    | Molar -21 -> "zM"
    | Molar -24 -> "yM"
    | Molar   n -> "M*10\^" + (string n)

open System.Diagnostics
[<JavaScript>]
[<DebuggerDisplay("")>] // displays CRNs as strings in VS debugger
type Units = 
  { concentration:Concentration; time:Time; space:Space }
  static member defaults = {
    concentration = Molar -9;
    time = Seconds 1.0;
    space = Metres -3;
  }
  member units.to_string = 
    Lib.emit_record Units.defaults [
      "concentration" , fun u -> u.concentration.to_string;
      "time" ,  fun u -> u.time.to_string;
      "space" , fun u -> u.space.to_string;
    ] units 

  static member parse : Parser.t<Units> =
    let pconcentration = 
      choice [
        kw "M"  >>. preturn (Molar   0)
        kw "mM" >>. preturn (Molar  -3)
        kw "uM" >>. preturn (Molar  -6)
        kw "nM" >>. preturn (Molar  -9)
        kw "pM" >>. preturn (Molar -12)
        kw "fM" >>. preturn (Molar -15)
        kw "aM" >>. preturn (Molar -18)
        kw "zM" >>. preturn (Molar -21)
        kw "yM" >>. preturn (Molar -24)
      ]

    let ptime = 
      choice [
        kw "ns"  >>. preturn (Seconds 1E-9  )
        kw "us"  >>. preturn (Seconds 1E-6  )
        kw "ms"  >>. preturn (Seconds 1E-3  )
        kw "s"   >>. preturn (Seconds 1.0   )
        kw "min" >>. preturn (Seconds 60.0  )
        kw "h"   >>. preturn (Seconds 3600.0)      
      ]

    let pspace = 
      choice [
        kw "m"  >>. preturn (Metres 0  )
        kw "mm" >>. preturn (Metres -3 )
        kw "um" >>. preturn (Metres -6 )
        kw "nm" >>. preturn (Metres -9 )
        kw "pm" >>. preturn (Metres -12)
        kw "fm" >>. preturn (Metres -15)
      ]
    
    Parser.record Units.defaults [ 
      "concentration",   pconcentration .>> Parser.spaces |>> fun d s -> { s with concentration = d }
      "time",            ptime          .>> Parser.spaces |>> fun d s -> { s with time          = d }
      "space",           pspace         .>> Parser.spaces |>> fun d s -> { s with space         = d }
    ]



