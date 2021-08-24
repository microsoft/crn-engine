// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

namespace Microsoft.Research.Filzbach

open Parameters
open DataStructures

[<JavaScript>]
module FilzbachMultiple =
    
    type likelihood_updater = Ids -> ParameterValues -> float list
    type score = float list
    type step<'a> = int -> score -> 'a -> 'a * score * bool

    //let mutable logLikelihoods = []
    let partial_logLikelihoods (changed:bool []) (dependencies:Parameters.Ids list) (likefn:likelihood_updater) oldVals (ps:ParameterValues) =
        let ids = List.zip (changed |> List.ofArray) dependencies |> List.filter fst |> List.map snd
        if (List.contains Parameters.All ids)
        then likefn Parameters.All ps
        else 
          let subset = 
            ids 
            |> List.fold (fun s deps -> match deps with Parameters.Subset sub -> sub @ s | Parameters.All -> failwith "Can't be possible") [] 
            |> List.distinct 
            |> List.sort
          let subs_loglike = likefn (Parameters.Subset subset) ps
          List.mapi (fun i li -> match List.tryFindIndex (fun j -> j=i) subset with Some j -> subs_loglike.[j] | None -> li) oldVals
     
    let evaluateAcceptance logLikelihood dependencies iteration oldVals space priors state innovationGens indexes rng = 
        let proposed,changedParametersMap,innovationGens,rng = Filzbach.proposeValues space state rng innovationGens
        let outOfRange = Filzbach.isStateOutOfRange changedParametersMap proposed space indexes
        if outOfRange 
        then Filzbach.Rejected, rng, oldVals, innovationGens, changedParametersMap,outOfRange
        else 
            let lVals = partial_logLikelihoods changedParametersMap dependencies logLikelihood oldVals proposed
            let result, rng = Filzbach.acceptProposedValues iteration (List.sum lVals) (Filzbach.lglkPriors proposed priors) proposed state rng
            result, rng, lVals, innovationGens,changedParametersMap,outOfRange

    let public burninStep logLikelihood dependencies (options:Filzbach.RunOptions) iteration oldVals (burnin:Filzbach.Burnin) =
        let acceptanceResult,rng,lVals,_,changed,outOfRange = evaluateAcceptance logLikelihood dependencies iteration oldVals burnin.space burnin.priors burnin.state burnin.innovationGens burnin.indexes burnin.randGen
        //printfn "Changed: %d, %s" (Array.filter id changed).Length (match acceptanceResult with Filzbach.Accepted _ -> "Accepted" | Filzbach.Rejected -> "Rejected")
        //match acceptanceResult with 
        //| Filzbach.Accepted _ -> printf "(%d)" (Array.filter id changed).Length
        //| Filzbach.Rejected -> printf "."
        let nextState,newMLE,newVals,correctedStats = //counters of acceptance and alteration corrected with respect of the acceptance of newly proposed state
            match acceptanceResult with
            | Filzbach.Rejected -> 
                ( { burnin.state with iteration=iteration }
                , burnin.mle
                , oldVals
                , (burnin.stats, changed) ||> Array.map2 (fun stat flag -> if flag then { stat with altered=stat.altered+1} else stat)
                ) 
            | Filzbach.Accepted(s) ->
                let effectiveMLE = if s.logLikelihood > burnin.mle.logLikelihood then s else burnin.mle
                ( s
                , effectiveMLE
                , lVals
                , (burnin.stats, changed) ||> Array.map2 (fun stat flag -> if flag then {accepted=stat.accepted+1; altered=stat.altered+1} else stat)
                ) 
        let generatorsToUpdate = Array.mapi (fun idx value -> if value.altered=20 then Some(idx) else None) correctedStats |> Array.choose (fun dummy -> dummy)
        let correctedInnovationGens = Filzbach.correctInnovationGens generatorsToUpdate burnin.space burnin.stats burnin.innovationGens
        //resetting counters of corrected generators
        let flushedStats = Array.mapi (fun idx stats -> if Array.contains idx generatorsToUpdate then {accepted=0; altered=0} else stats) correctedStats
        let burnin = {burnin with stats=flushedStats; innovationGens=correctedInnovationGens; state=nextState; mle = newMLE; randGen=rng}
        let burnin' = 
          if burnin.thinningSkippedCount = options.thinning-1 then
            { burnin with chain = nextState::burnin.chain; thinningSkippedCount=0 }
          else
            { burnin with thinningSkippedCount=burnin.thinningSkippedCount+1 }
        burnin', newVals, outOfRange

    let public sampleStep logLikelihood dependencies (options:Filzbach.RunOptions) iteration oldVals (sampling:Filzbach.Sampling) =
        let acceptanceResult,rng,lVals,innovationGens,changed,outOfRange = evaluateAcceptance logLikelihood dependencies iteration oldVals sampling.space sampling.priors sampling.state sampling.innovationGens sampling.indexes sampling.randGen
        let nextState,newMLE,newVals,accepted =
            match acceptanceResult with
            | Filzbach.Rejected -> { sampling.state with iteration=iteration }, sampling.mle, oldVals, sampling.accepted
            | Filzbach.Accepted(newState) -> 
                //if newState.logLikelihood > 0.0 
                //then newVals |> List.map string |> String.concat "\n- " |> failwithf "Iteration %d\n%s" iteration
                newState, (if newState.logLikelihood > sampling.mle.logLikelihood then newState else sampling.mle), lVals, sampling.accepted+1
        let sampling = { sampling with state=nextState; mle = newMLE; randGen=rng; innovationGens=innovationGens; accepted=accepted }
        let sampling' = 
          if sampling.thinningSkippedCount = options.thinning-1 then
            { sampling with chain = nextState::sampling.chain; thinningSkippedCount=0 }
          else
            { sampling with thinningSkippedCount=sampling.thinningSkippedCount+1 }
        sampling', newVals, outOfRange
    
    let step_many options (step:'a step) print_summary run init_score length = //to enable tail recursion optimization in WebSharper, rec function is defined inside
        let mutable mle_score = List.sum init_score
        let rec loop scores run count outOfRange =            
            if count > length
            then run
            else
                if (count % 5000 = 0 && count > 0) then print_summary count run
                let stepped,newscore,oo = step count scores run 
                let score_sum = List.sum newscore
                let improved = score_sum > mle_score
                if improved then mle_score <- score_sum
                Filzbach.print_improvement improved options count score_sum
                loop newscore stepped (count+1) (outOfRange + if oo then 1 else 0)
        loop init_score run 1 0    
    
    let get_sampling (logLikelihood_updater:likelihood_updater) (parameters:Parameter list) (dependencies:Ids list) space (options:Filzbach.RunOptions) = // burnInLen seed =
        let logLikelihood = logLikelihood_updater Parameters.All 
        let p0, rng = Filzbach.paramsInit parameters (Lib.LCGRng options.seed)
        let burninFirstState = Filzbach.burninInit (logLikelihood >> List.sum) parameters p0 space rng
        let summaryNames = parameters |> List.filter (fun p -> p.summary) |> List.map (fun p -> p.name)
        let initScore = logLikelihood p0
        let print c (b:Filzbach.Burnin) =             
            #if JavaScript
            ()
            #else
            if options.print_console then printfn "\nBurn-in phase: Completed %d iterations" c
            if options.print_summary then Filzbach.print_summary summaryNames b.mle b.state
            #endif
        let calibrated = step_many options (burninStep logLikelihood_updater dependencies options) print burninFirstState initScore options.burninLen
        Filzbach.samplingInit calibrated

    // Convenience
    let paramsInit dependencies (parameters:Parameter list) = 
        let fixedPars, notFixedPars = List.zip parameters dependencies |> List.partition (fun (p,d) -> p.isFixed ()) 
        let space = notFixedPars |> List.map (fun (p,d) -> NamedObject(p.name,p.range)) |> AssociativeArray<ParameterRange>.ofSeq
        let variableDependencies = List.map snd notFixedPars
        let reorderedParameters = List.append notFixedPars fixedPars |> List.map fst //reordered parameters so the fixed parameters goes last
        reorderedParameters, variableDependencies, space
        
    let public run (logLikelihood:likelihood_updater) (parameters:Parameter list) dependencies (options:Filzbach.RunOptions) = // burnInLen samplingLen seed =
        let reorderedParameters, variableDependencies, space = parameters |> Filzbach.paramsCheck |> paramsInit dependencies
        let burnin = get_sampling logLikelihood (reorderedParameters:Parameter list) variableDependencies space options //burnInLen seed
        let summaryNames = parameters |> List.filter (fun p -> p.summary) |> List.map (fun p -> p.name)
        let print c (s:Filzbach.Sampling) =             
            #if JavaScript
            ()
            #else
            if options.print_console then printfn "\nSampling phase: Completed %d iterations (plus %d burn-in)" c options.burninLen
            if options.print_summary then Filzbach.print_summary summaryNames s.mle s.state
            #endif
        let score = logLikelihood Parameters.All burnin.mle.values
        let samples = step_many options (sampleStep logLikelihood variableDependencies options) print burnin score options.sampleLen // as we already have one sample
        if options.print_console then printfn "\nFinished: Completed %d sampling iterations and %d burn-in" options.sampleLen options.burninLen
        if options.print_summary then Filzbach.print_summary summaryNames samples.mle samples.state
        samples

    let public run_seq (logLikelihood_updater:likelihood_updater) (parameters:Parameter list) dependencies (options:Filzbach.RunOptions) = //burnInLen samplingLen seed =        
        let reorderedParameters, variableDependencies, space = parameters |> Filzbach.paramsCheck |> paramsInit dependencies
        let logLikelihood = logLikelihood_updater Parameters.All
        let step state =
            let state,iteration,oldVals = state            
            let nextState, newVals =
                match state with
                | Filzbach.BurninPhase(burnin) ->
                    if iteration >= options.burninLen 
                    then Filzbach.SamplingPhase(Filzbach.samplingInit burnin), oldVals
                    else 
                        let burnin, newVals, _ = burninStep logLikelihood_updater variableDependencies options iteration oldVals burnin
                        Filzbach.BurninPhase(burnin), newVals
                | Filzbach.SamplingPhase(sampling) -> 
                    let sampling, newVals, _ = sampleStep logLikelihood_updater variableDependencies options iteration oldVals sampling
                    Filzbach.SamplingPhase(sampling), newVals
            nextState,iteration+1,newVals
        let firstParams, rng = Filzbach.paramsInit reorderedParameters (Lib.LCGRng options.seed)
        let initArray = logLikelihood firstParams
        let firstState = Filzbach.burninInit (logLikelihood >> List.sum) reorderedParameters firstParams space rng
        Filzbach.step_to_seq step (Filzbach.BurninPhase(firstState),1,initArray) (options.burninLen+options.sampleLen) |> Seq.map Lib.fst3