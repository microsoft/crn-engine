namespace Microsoft.Research.Filzbach

open DataStructures
open Parameters
open Filzbach

[<JavaScript>]
module FilzbachAnalysis =
    type summaryType = {count:int; min:float; max:float; mean:float; variance:float}
    /// Produces cumulant summary of the data using fast one-pass algorithm.
    let summary data =
        let folder summary d =
            if System.Double.IsNaN(d) || System.Double.IsInfinity(d) then 
                summary 
            else
                let delta = d - summary.mean
                let n = summary.count + 1
                let mean = summary.mean + delta/float n
                {
                    count = n
                    min = (min d summary.min)
                    max = (max d summary.max)
                    mean = mean
                    variance = summary.variance + delta*(d-mean)
                }
        let pass =
            Seq.fold folder {
                                count=0
                                min=System.Double.PositiveInfinity
                                max=System.Double.NegativeInfinity
                                mean=0.0
                                variance=0.0
                                } data
        if pass.count<2 then
            pass
        else
            let pass = {pass with variance=pass.variance/(float(pass.count-1))}
            pass

    type qsummaryType = {min:float; lb95:float; lb68:float; median:float; ub68:float; ub95:float; max:float}
    /// Produces quantile summary of the data.
    let qsummary data =
        let a = data |> Seq.filter(fun d -> not (System.Double.IsNaN(d) || System.Double.IsInfinity(d))) |> Seq.toArray
        Array.sortInPlace a
        let n = a.Length
        if n<1 then {min=nan; lb95=nan; lb68=nan; median=nan; ub68=nan; ub95=nan; max=nan}
        else
            let q p =
                let h = p*(float n + 1./3.)-2./3.
                if h <= 0.0 then a.[0]
                elif h >= float (n-1) then a.[n-1]
                else
                    let fh = floor h
                    a.[int fh]*(1.0-h+fh) + a.[int fh + 1]*(h - fh)
            {min=a.[0]; lb95=q(0.025); lb68=q(0.16); median=q(0.5); ub68=q(0.84); ub95=q(0.975); max=a.[n-1]}
    
    type ParameterSummary = {
        name:string
        mle:float
        summary:summaryType
        qsummary:qsummaryType
        range:ParameterRange
    }

    type InformationCriteria =
        {
            AIC:float
            BIC:float
            DIC:float
        }
    
    let public extractBayesTable run =
        let floatChain = Seq.map (fun elem -> Array.append [|elem.logLikelihood|] elem.values._dataArray) run.chain |> Array.ofSeq
        if floatChain.Length=0 then BayesTable([| |],NamesMap([||]))
        else
            let extendedNamesMap = NamesMap([| yield "logLikelihood"; yield! run.space._namesMap.Keys |])
            let reindexedFloatChain = Array.init (run.space._namesMap.Keys.Length+1) (fun i -> Array.init floatChain.Length (fun j -> floatChain.[j].[i]))
            BayesTable(reindexedFloatChain,extendedNamesMap)

    let public informationCriteria (sampling:Sampling) samplesCount =
        let maxlglk = sampling.mle.logLikelihood
        let freeParams = sampling.space._dataArray.Length
        let aic = -2.0*maxlglk + 2.0* float(freeParams)
        let bic = -2.0*maxlglk + log(float(samplesCount))*float(freeParams)
        let dic = 
            let bayestable = extractBayesTable sampling
            if bayestable.ColumnCount=0 then nan else
                let dmean = bayestable.GetColumn "logLikelihood" |> Array.map (fun b -> -2.0*b) |> Array.average
                let datmean = -2.0*maxlglk
                -1.0*datmean + 2.0 *dmean
        {
            AIC=aic
            BIC=bic
            DIC=dic
        }


    let public getParameterSummary (bayestable:BayesTable) (run:Sampling) (name:string) =
        let columnData = bayestable.GetColumn name
        let num = run.space._namesMap.[name]
        {
          name=name
          mle=run.mle.values.[name]
          summary=summary columnData
          qsummary = qsummary columnData
          range=run.space.[num]
        }

    let public getSummary run =
        let bayesTable = extractBayesTable run
        if bayesTable.ColumnNames.Length<=1 then [||]
        else
            [| for name in Seq.skip 1 bayesTable.ColumnNames do yield getParameterSummary bayesTable run name |]

    let stringOfRange (range:ParameterType) = 
        match range with
        | Real  -> "real"
        | Log   -> "log"
        | Fixed -> "fixed"
    
    let public stringOfSummary (state:RunPhase) (opts:RunOptions) (numData:int) : string =
        let delim = "\t"
        let newline = "\r\n"
        //let string_of_float str = sprintf "%1.4g" str
        //let string_of_int str = sprintf "%d" str
        let string_of_float = string
        let string_of_int = string
            
        match state with
        |   BurninPhase(burnin) ->
            let phase = "Phase: Burn-In"            
            let summary = [| "Params" + delim + string_of_int (Array.length burnin.space.Names)
                           ; "Free_params" + delim + (burnin.space.ToArray() |> Array.filter (fun p -> p.pType <> Parameters.Fixed) |> Array.length |> string_of_int)
                           ; "Burnin" + delim + string_of_int opts.burninLen
                           ; "Samples" + delim + string_of_int opts.sampleLen
                           ; "Max loglikelihood" + delim + string_of_float burnin.mle.logLikelihood
                          |]
            String.concat newline (Array.concat [| [|phase|]; summary|])
        |   SamplingPhase(sampling) ->
            let parameters = getSummary sampling
            let mle = sampling.mle.values
            let ics = informationCriteria sampling numData
            let summary = [| "Params" + delim + string_of_int (Array.length parameters)
                           ; "Free_params" + delim + (parameters |> Array.filter (fun p -> p.range.pType <> Parameters.Fixed) |> Array.length |> string_of_int)
                           ; "Burnin" + delim + string_of_int opts.burninLen
                           ; "Samples" + delim + string_of_int opts.sampleLen
                           ; "Max loglikelihood" + delim + string_of_float sampling.mle.logLikelihood
                           ; "AIC" + delim + string_of_float ics.AIC
                           ; "BIC" + delim + string_of_float ics.BIC
                           ; "DIC" + delim + string_of_float ics.DIC
                          |]            
            let header = String.concat delim ["Name"; "Type"; "Lower Bound"; "Upper Bound"; "MLE"; "Posterior mean"; "Posterior variance"; "Posterior l95"; "Posterior l68"; "Posterior median"; "Posterior u68"; "Posterior u95"]
            let pstrs = parameters |> Array.map (fun p ->
                String.concat delim (p.name :: stringOfRange p.range.pType :: (List.map string_of_float [p.range.lb; p.range.ub; mle.[p.name]; p.summary.mean; p.summary.variance; p.qsummary.lb95; p.qsummary.lb68; p.qsummary.median; p.qsummary.ub68; p.qsummary.ub95])))             
            String.concat newline (Array.concat [| summary; [|" "; header|]; pstrs |])