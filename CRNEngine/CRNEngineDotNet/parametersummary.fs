namespace Microsoft.Research.CRNEngine

open Microsoft.Research.Filzbach
open Parser

[<JavaScript>]
type ParameterSummary = 
  {
    name:string
    mle:float
    mean: float
    variance: float
    median: float
    lb: float
    ub: float
    l68: float
    u68: float
    l95: float
    u95: float
    ptype:Interval
  }
  static member defaults = 
    { name      = ""
    ; mle       = 0.0
    ; mean      = 0.0
    ; variance  = 0.0
    ; median    = 0.0
    ; lb        = 0.0
    ; ub        = 0.0
    ; l68       = 0.0
    ; u68       = 0.0
    ; l95       = 0.0
    ; u95       = 0.0
    ; ptype     = Real }
  static member create (name, mle, mean, variance, median, lb, ub, l68, u68, l95, u95, ptype) =
    { name      = name
    ; mle       = mle
    ; mean      = mean    
    ; variance  = variance
    ; median    = median  
    ; lb        = lb      
    ; ub        = ub      
    ; l68       = l68     
    ; u68       = u68     
    ; l95       = l95     
    ; u95       = u95     
    ; ptype     = ptype    }


  static member to_string (ps:ParameterSummary) =
    #if JavaScript
    let fmt = sprintf "%1.6f"
    #else
    let fmt = sprintf "%1.6g"
    #endif
    Lib.emit_record ParameterSummary.defaults 
      [ "mle"      , fun p -> fmt p.mle
      ; "mean"     , fun p -> fmt p.mean
      ; "variance" , fun p -> fmt p.variance
      ; "median"   , fun p -> fmt p.median
      ; "lb"       , fun p -> fmt p.lb
      ; "ub"       , fun p -> fmt p.ub
      ; "l68"      , fun p -> fmt p.l68
      ; "u68"      , fun p -> fmt p.u68
      ; "l95"      , fun p -> fmt p.l95
      ; "u95"      , fun p -> fmt p.u95
      ; "type"     , fun p -> Interval.to_string p.ptype
      ] ps
    |> sprintf "parameterSummary %s %s" ps.name
  static member parse = 
    let name = (many1Satisfy isLetter .>>. manySatisfy (fun c -> isLetter c || isDigit c || c = '_'|| c = '\'' || c = '.') |>> fun (a,b) -> a + b) <?> "an identifier"
    kw "parameterSummary" >>. name .>> Parser.spaces >>= fun name -> 
    Parser.record { ParameterSummary.defaults with name = name }
      [
        "mle"     , Parser.pfloat  |>> fun d x -> { x with mle      = d }
        "mean"    , Parser.pfloat  |>> fun d x -> { x with mean     = d }
        "variance", Parser.pfloat  |>> fun d x -> { x with variance = d }
        "median"  , Parser.pfloat  |>> fun d x -> { x with median   = d }
        "lb"      , Parser.pfloat  |>> fun d x -> { x with lb       = d }
        "ub"      , Parser.pfloat  |>> fun d x -> { x with ub       = d }
        "l68"     , Parser.pfloat  |>> fun d x -> { x with l68      = d }
        "u68"     , Parser.pfloat  |>> fun d x -> { x with u68      = d }
        "l95"     , Parser.pfloat  |>> fun d x -> { x with l95      = d }
        "u95"     , Parser.pfloat  |>> fun d x -> { x with u95      = d }
        "type"    , Interval.parse |>> fun d x -> { x with ptype    = d }
      ] 
  static member from_string = Parser.from_string ParameterSummary.parse
  static member fromFilzbach (p:FilzbachAnalysis.ParameterSummary) = 
    let ptype = 
        match p.range.pType with 
        | Parameters.ParameterType.Log -> Interval.Log 
        | _ -> Interval.Real
    { name=p.name
    ; mle=p.mle
    ; mean=p.summary.mean
    ; variance=p.summary.variance
    ; median = p.qsummary.median
    ; lb=p.range.lb
    ; ub=p.range.ub
    ; l68 = p.qsummary.lb68
    ; u68 = p.qsummary.ub68
    ; l95 = p.qsummary.lb95
    ; u95 = p.qsummary.ub95
    ; ptype=ptype 
    }
  member ps.toNormalDist () = Distribution.Normal { mean = ps.mean; stdev = sqrt ps.variance }
  /// Approximate Log-Normal distribution parameters from first and second-order statistics
  member ps.toLogNormalDist () = 
    // See https://en.wikipedia.org/wiki/Log-normal_distribution)
    let b = 1.0 + ps.variance/ps.mean/ps.mean
    Distribution.LogNormal { mu = log (ps.mean / sqrt b); sigma = log b }
  member ps.toTruncatedNormalDist () = Distribution.TruncatedNormal { mean = ps.mean; stdev = sqrt ps.variance; min=ps.lb; max=ps.ub } 