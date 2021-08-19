namespace Microsoft.Research.Filzbach

open Parameters
open Lib
open DataStructures

[<JavaScript>]
module Filzbach =
    
    type RunOptions = {
        burninLen : int
        sampleLen : int
        thinning : int
        seed : uint32
        print_console : bool
        print_summary : bool
    }

    let defaultRunOptions = {
      burninLen = 200;
      sampleLen = 200;
      thinning = 10;
      seed = 0u;
      print_console = true;
      print_summary = true;
    }

    type Burnin = {
        space: ParameterSpace //contains only non-fixed parameters information
        state: EvaluatedValues //contains all parameters (non-fixed followed by fixed)
        thinningSkippedCount:int //how many steps are passed after last save of state to the chain
        chain: Chain
        stats: ParameterStatistics
        randGen : IRng
        innovationGens: InnovationGenerators
        mle: EvaluatedValues
        priors: PriorValue option array
        indexes: int array
    }

    type Sampling = {
        space: ParameterSpace //contains only non-fixed parameters information
        state: EvaluatedValues //contains all parameters (non-fixed followed by fixed)
        thinningSkippedCount:int //how many steps are passed after last save of state to the chain
        chain: Chain
        burnin: Chain
        randGen : IRng
        innovationGens: InnovationGenerators //normal distribution generators for each of the non-fixed parameters (have different sigmas)
        mle: EvaluatedValues
        priors: PriorValue option array
        indexes: int array
        accepted: int
    }

    type AcceptanceResult = 
        |   Accepted of EvaluatedValues
        |   Rejected

    let public parameter name pType lowerBound upperBound initialValue prior: Parameter =
      {
        name = name
        range = { pType = pType; lb = lowerBound; ub = upperBound }
        initValue = initialValue
        prior = prior
        summary = true
      }

    let public vectorParameter name pType lowerBound upperBound initialValue prior length =
        let initializer (idx:int)=
          {
            name = name+"["+idx.ToString()+"]"
            range = { pType = pType; lb = lowerBound; ub = upperBound }
            initValue = initialValue
            prior = prior
            summary = false
          }
        Array.init length initializer

    let lglkPriors (values:ParameterValues) priors = 
        (priors, values.ToArray())
        ||> Array.map2 (fun prior value -> 
            match prior with 
            | Some (PriorValue.NormalPrior pr)          -> Lib.logOfNormPDF pr.mean pr.stdev value
            | Some (PriorValue.LogNormalPrior pr)       -> Lib.logOfNormPDF pr.mu pr.sigma (log value) - (log value)
            | Some (PriorValue.TruncatedNormalPrior pr) -> Lib.logOfNormPDF pr.mean pr.stdev value - log(pr.denominator)
            | None -> 0.0
        )
        |> Array.sum
    
    let paramsCheck parameters = 
        // Argument checks
        List.iter (fun p -> 
          match p.initValue with 
          | Some v -> 
              if (v > p.range.ub || v < p.range.lb) 
              then failwithf "The initial value of parameter %s is outside the specified bounds (%f,%f)" p.name p.range.lb p.range.ub 
          | None -> ()
        ) parameters 
        if parameters |> List.exists (fun p -> p.isFixed() && p.initValue.IsNone) 
        then failwith "All fixed parameters must have initial value"
        if parameters |> List.exists (fun p -> p.range.lb>p.range.ub)  
        then failwith "One of the parameter definitions contains upper bound lower than lower bound" 
        if parameters |> List.exists (fun p -> not (p.isReal()) && p.range.lb<0.0) 
        then failwith "One of the log-scale parameter definitions contains negative lower bound"

        // Argument coercion
        let zeroBoundForLogCoercer p = 
            if p.range.pType=ParameterType.Log && p.range.lb=0.0
            then { p with range = { p.range with lb = System.Double.Epsilon}}
            else p
        List.map zeroBoundForLogCoercer parameters 
        
    let public paramsInit parameters (rng: IRng) =
        let getStartValue v (range:ParameterRange) (rng:IRng) =
            match v with
            | Some v -> v,rng
            | None   ->
                let rn,rng = rng.NextDouble()
                range.lb+rn*(range.ub-range.lb),rng
        let folder (state:NamedObject<float>*_) p =
            let _,rng = state
            let v,rng = getStartValue p.initValue p.range rng
            NamedObject(p.name,v),rng
        let states = parameters |> List.scan folder (NamedObject("dummy",0.0),rng) |> List.skip 1 |> Array.ofList
        let result = Array.map fst states
        if states.Length <> parameters.Length then failwith "unexpected scan behaviour"
        AssociativeArray<float>.ofArray result,snd states.[states.Length-1]

    let public burninInit logLikelihoodFunction parameters startVals (space:ParameterSpace) (rng:IRng) =        
        let innovGens =
            let getStartDelta range =
                match range.pType with
                | Real  -> 0.5*(range.ub-range.lb)
                | Log   -> 0.5*(log(range.ub)-log(range.lb))
                | Fixed -> failwith "Must not be called. Innovation generator is not needed for fixed parameter"
            Array.mapi (fun i r -> i,NormalGenerator(0.0,getStartDelta r)) space._dataArray |> Map.ofSeq
        let priors =
            parameters
            |> List.map (fun p -> p.prior)
            |> List.toArray
        let values = 
            { iteration = 0
              values = startVals;
              logLikelihood = logLikelihoodFunction startVals
              logPrior = lglkPriors startVals priors }
        
        { space = space
          state = values
          mle = values
          thinningSkippedCount = 0
          chain = []
          randGen = rng
          innovationGens = innovGens
          stats = Array.map (fun _ -> { accepted = 0; altered = 0 }) space._dataArray
          indexes = Array.init (innovGens.Count) id
          priors = priors }

    let public samplingInit (run:Burnin) =
        {
            space = run.space
            state = run.state
            mle = run.mle
            thinningSkippedCount = 0
            chain = []
            burnin = run.chain
            randGen = run.randGen
            innovationGens = run.innovationGens
            indexes = run.indexes
            priors = run.priors
            accepted = 0
        }
    
    let proposeValues (space:ParameterSpace) state (rng:IRng) (innovationGens:InnovationGenerators) =
        let currentValues = state.values
        let prevVals = state.values._dataArray
        let len = space._dataArray.Length
        if len = 0
        then currentValues, [||], innovationGens, rng
        else
          let fixedVals = Array.sub prevVals len (prevVals.Length-len)
          let buildParamsToChangeMap (rng:IRng) = //what parameters to alter at the next jump
              let rn,rng  = rng.NextDouble()
              let isSingleDirection = rn < 0.67
              let rn,rng  = rng.NextDouble()
              (*if len = 0 
              then Array.create values.Names.Length false, rng
              else*)
              if isSingleDirection then //returning primary dimension to change and possibly adjacent dimensions
                  let toChangeMap = Array.zeroCreate len //we use imperative constructs here to gain performance. this is "bit" map of parameter indeces to change.
                  let rn2,rng  = rng.NextDouble()
                  let rn3,rng  = rng.NextDouble()
                  let directionIdx = (int)(floor(rn*(float)len))
                  if directionIdx>0 && rn2< 0.5 then toChangeMap.[directionIdx-1] <- true
                  toChangeMap.[directionIdx] <- true
                  if directionIdx<len-1 && rn3< 0.5 then toChangeMap.[directionIdx+1] <- true
                  toChangeMap,rng
              else //change many parameters at once
                  let talt = 3.0/(float)len
                  let palt =
                      let p = talt*exp(4.0*(rn- 0.5))
                      if p <0.1*talt then 0.1*talt
                      else if p>0.99 then 0.99
                      else p
                
                  let rec GetChangedMap rng =
                      let rec GenPList l (rng:IRng) size =
                          let n,rng = rng.NextDouble()
                          if size=1 then
                              n::l,rng
                          else
                              GenPList (n::l) rng (size-1)
                      let pList,rng = GenPList [] rng len
                      let ps = List.toArray pList
                      if Array.exists (fun x -> x<palt) ps then
                          Array.map (fun x -> x<palt) ps,rng
                      else
                          GetChangedMap rng
                  GetChangedMap rng
            
          let proposeValue range currentVal (generator:NormalGenerator) (rng:IRng) =
              let addition,generator,rng = generator.nextDouble(rng)
              let v =
                  match range with
                  | Real   -> currentVal+addition
                  | Log    -> currentVal*exp(addition)
                  | Fixed  -> failwith "Proposing to update fixed value"
              v,generator,rng
          let paramsToChangeMap,rng = buildParamsToChangeMap rng

          let folder (state:float*_*_*_)  isToChange = 
              let _,rng,(generators:InnovationGenerators),(idx:int)  = state
              let v,generator,rng =
                  if isToChange then
                      proposeValue space.[idx].pType currentValues.[idx] generators.[idx] rng
                  else
                      prevVals.[idx],generators.[idx],rng
              v,rng,(Map.add idx generator generators),idx+1
          let newValuesTuples = Seq.scan folder (0.0,rng,innovationGens,0) paramsToChangeMap |> Seq.skip 1 |> Seq.toArray
          let newValues = Array.map (fun x -> let a,_,_,_ = x in a) newValuesTuples
          let newValues = Array.append newValues fixedVals
          let _,rng,innovationGens,_ = newValuesTuples.[newValuesTuples.Length-1]
          //printfn "%f" newValues.[1]
          currentValues.CopyWithNewValues newValues, paramsToChangeMap,innovationGens, rng
    
    let isStateOutOfRange changedParametersMap (proposed:ParameterValues) (space:ParameterSpace) indices =
        let checkOutOfRange (idx:int) = let v,r=(proposed.[idx],space.[idx]) in v<r.lb || v>r.ub
        Array.exists2 (fun flag idx -> if flag then checkOutOfRange idx else false) changedParametersMap indices
    
    /// The Metropolis-Hastings acceptance criterion
    let acceptProposedValues iteration logLikelihood logPrior (proposedValues:ParameterValues) currentState rng =
        let accept newLogLkh oldLogLkh (rng:IRng) =
            let rn,rng = rng.NextDouble()
            if newLogLkh > oldLogLkh 
            then true, rng
            else
                let logUnif = log(rn)
                newLogLkh-oldLogLkh > logUnif, rng
        let isAccepted,rng = accept (logPrior + logLikelihood) (currentState.logLikelihood + currentState.logPrior) rng
        let decision = 
            if isAccepted then
                Accepted({ iteration = iteration; values = proposedValues; logLikelihood = logLikelihood; logPrior = logPrior})
            else
                Rejected
        decision,rng
    
    let correctInnovationGens generatorsToUpdate (space:ParameterSpace) (stats:ParameterStatistics) innovationGens= //innovation generators corrected with respect of latest acceptance statistics
        let updateInnovationGen (gen:NormalGenerator) (range:ParameterRange) (accepted:int) =
            let sigma = gen.Sigma
            let newSigma =
                let proposedSigma =
                    match accepted with
                    | c when c<5  ->  sigma*0.8
                    | c when c>5  ->  sigma*1.2
                    | _   -> sigma
                match range.pType with
                | Real ->
                    let r = range.ub-range.lb
                    let s_lb,s_ub = 0.001*r, 0.5*r
                    max (min proposedSigma s_ub) s_lb
                | Log ->
                    max (min proposedSigma 10.0) 0.01
                | Fixed -> failwith "Update of fixed value"
            NormalGenerator(gen.Mean,newSigma)
        Map.map (fun (idx:int) gen -> if Array.exists (fun idx2 -> idx=idx2) generatorsToUpdate then updateInnovationGen gen space.[idx] stats.[idx].accepted else gen) innovationGens
       
    let evaluateAcceptance logLikelihood iteration space priors state innovationGens indexes rng = 
        let proposed,changedParametersMap,innovationGens,rng = proposeValues space state rng innovationGens
        let outOfRange = isStateOutOfRange changedParametersMap proposed space indexes
        if outOfRange 
        then Rejected, rng, innovationGens, changedParametersMap,outOfRange
        else 
            let result, rng = acceptProposedValues iteration (logLikelihood proposed) (lglkPriors proposed priors) proposed state rng
            result, rng, innovationGens,changedParametersMap,outOfRange
    
    let public burninStep logLikelihood options iteration (burnin:Burnin) =
        let acceptanceResult,rng,_,changed,outOfRange = evaluateAcceptance logLikelihood iteration burnin.space burnin.priors burnin.state burnin.innovationGens burnin.indexes burnin.randGen
        let nextState, newMLE, correctedStats = // Counters of acceptance and alteration corrected with respect of the acceptance of newly proposed state
            match acceptanceResult with
            | Rejected -> { burnin.state with iteration=iteration }, burnin.mle, (burnin.stats, changed) ||> Array.map2 (fun stat flag -> if flag then { stat with altered=stat.altered+1} else stat) 
            | Accepted(s) ->
                let effectiveMLE = if s.logLikelihood > burnin.mle.logLikelihood then s else burnin.mle
                s,effectiveMLE, (burnin.stats, changed) 
                ||> Array.map2 (fun stat flag -> if flag then {accepted=stat.accepted+1; altered=stat.altered+1} else stat) 
        let generatorsToUpdate = Array.mapi (fun idx value -> if value.altered=20 then Some(idx) else None) correctedStats |> Array.choose id
        let correctedInnovationGens = correctInnovationGens generatorsToUpdate burnin.space burnin.stats burnin.innovationGens
        // Resetting counters of corrected generators
        let flushedStats = Array.mapi (fun idx stats -> if Array.exists (fun idx2 -> idx=idx2) generatorsToUpdate then {accepted=0; altered=0} else stats) correctedStats
        let burnin = { burnin with stats=flushedStats; innovationGens=correctedInnovationGens; state=nextState; mle = newMLE; randGen=rng }
        if burnin.thinningSkippedCount = options.thinning-1 then
            {burnin with chain = nextState::burnin.chain; thinningSkippedCount=0}, newMLE.logLikelihood, outOfRange
        else
            {burnin with thinningSkippedCount=burnin.thinningSkippedCount+1}, newMLE.logLikelihood, outOfRange

    let public sampleStep logLikelihood options iteration sampling =
        let acceptanceResult,rng,innovationGens,_,outOfRange = evaluateAcceptance logLikelihood iteration sampling.space sampling.priors sampling.state sampling.innovationGens sampling.indexes sampling.randGen
        let nextState,newMLE,accepted =
            match acceptanceResult with
            | Rejected -> {sampling.state with iteration=iteration}, sampling.mle, sampling.accepted
            | Accepted(newState) -> newState, (if newState.logLikelihood > sampling.mle.logLikelihood then newState else sampling.mle), sampling.accepted+1
        let sampling = { sampling with state=nextState; mle = newMLE; randGen=rng; innovationGens=innovationGens; accepted=accepted }
        if sampling.thinningSkippedCount = options.thinning-1 then
            { sampling with chain = nextState::sampling.chain; thinningSkippedCount=0 }, newMLE.logLikelihood, outOfRange
        else
            { sampling with thinningSkippedCount=sampling.thinningSkippedCount+1 }, newMLE.logLikelihood, outOfRange
    
    let print_improvement improved options count newscore = 
        #if JavaScript 
        ()
        #else
        if options.print_console then 
            if improved 
            then System.Console.WriteLine("- Iteration {0}: {1}", sprintf "%d" count, sprintf "%f" newscore)
            else System.Console.Write("- Iteration {0}:\r", sprintf "%d" count)
        #endif

    let step_many options step print_summary run init_score length = //to enable tail recursion optimization in WebSharper, rec function is defined inside
        let mutable mle_score = init_score
        let rec loop run count outOfRange =       
            if count > length
            then run
            else
                if (count % 5000 = 0 && count > 0) then print_summary count run
                let stepped,newscore,oo = step count run 
                let improved = newscore > mle_score 
                if improved then mle_score <- newscore
                print_improvement improved options count newscore
                loop stepped (count+1) (outOfRange + if oo then 1 else 0)
        loop run 1 0

    let step_to_seq step sampling length =
      let init = sampling, length
      let gen (s, l) =
        if l <= 0 then None else
        let new_s = step s
        Some (new_s, (new_s, l-1))
      Seq.unfold gen init

    let print_summary names (mle:EvaluatedValues) (state:EvaluatedValues) = 
        #if JavaScript
        ()
        #else
        printfn "Max log-likelihood score: %f" mle.logLikelihood
        printfn "=================================================="
        printfn "Name\tMLE\tCurrent"
        printfn "--------------------------------------------------"
        names
        |> List.iter (fun n -> printfn "%s\t%1.3g\t%1.3g" n mle.values.[n] state.values.[n])
        printfn "=================================================="
        #endif
        
    let get_sampling logLikelihood (parameters:Parameter list) space (options:RunOptions) = // burnInLen seed =
        let rng = Lib.LCGRng options.seed
        let initParameters, rng = paramsInit parameters rng
        let burninFirstState = burninInit logLikelihood parameters initParameters space rng
        let summaryNames = parameters |> List.filter (fun p -> p.summary) |> List.map (fun p -> p.name)
        let local_print_summary c (b:Burnin) = 
            #if JavaScript
            ()
            #else
            if options.print_console then printfn "\nBurn-in phase: Completed %d iterations" c
            if options.print_summary then print_summary summaryNames b.mle b.state
            #endif
        let calibrated = step_many options (burninStep logLikelihood options) local_print_summary burninFirstState System.Double.NegativeInfinity options.burninLen
        samplingInit calibrated

    let public run_seq_burnin logLikelihood burnin options = 
        step_to_seq (fun (b,i) -> burninStep logLikelihood options i b |> Lib.fst3,i) burnin options.burninLen

    let public run_seq_sampling logLikelihood sampling options =
        step_to_seq (fun (s,i) -> sampleStep logLikelihood options i s |> Lib.fst3,i) sampling options.sampleLen // as we already have one sample

    type RunPhase = 
    |   BurninPhase of Burnin
    |   SamplingPhase of Sampling

    // Convenience
    let paramsSetup (parameters:Parameter list) = 
        let fixedPars, notFixedPars = parameters |> List.partition (fun p -> p.isFixed ()) 
        let space = notFixedPars |> List.map (fun p -> NamedObject(p.name,p.range)) |> AssociativeArray<ParameterRange>.ofSeq
        let reorderedParameters = List.append notFixedPars fixedPars //reordered parameters so the fixed parameters goes last
        reorderedParameters, space
        
    let public run logLikelihood (parameters:Parameter list) (options:RunOptions) = // burnInLen samplingLen seed =
        let reorderedParameters, space = parameters |> paramsCheck |> paramsSetup
        let burnin = get_sampling logLikelihood reorderedParameters space options //burnInLen seed
        let summaryNames = parameters |> List.filter (fun p -> p.summary) |> List.map (fun p -> p.name)
        let local_print_summary c (s:Sampling) = 
            #if JavaScript
            ()
            #else
            if options.print_console then printfn "\nSampling phase: Completed %d iterations (plus %d burn-in)" c options.burninLen
            if options.print_summary then print_summary summaryNames s.mle s.state 
            #endif
        let samples = step_many options (sampleStep logLikelihood options) local_print_summary burnin burnin.mle.logLikelihood options.sampleLen // as we already have one sample
        if options.print_console then printfn "\nFinished: Completed %d sampling iterations and %d burn-in" options.sampleLen options.burninLen
        if options.print_summary then print_summary summaryNames samples.mle samples.state
        samples

    let public run_seq logLikelihood (parameters:Parameter list) (options:RunOptions) = //burnInLen samplingLen seed =        
        let reorderedParameters, space = parameters |> paramsCheck |> paramsSetup
        let step (state, iteration) =
            let nextState =
                match state with
                |   BurninPhase(burnin) ->
                    if iteration >= options.burninLen 
                    then SamplingPhase(Lib.fst3 (sampleStep logLikelihood options iteration (samplingInit burnin)))
                    else BurninPhase(Lib.fst3 (burninStep logLikelihood options iteration burnin))
                |   SamplingPhase(sampling) -> SamplingPhase(Lib.fst3 (sampleStep logLikelihood options iteration sampling))
            nextState,iteration+1
        let firstParas, rng = paramsInit reorderedParameters (Lib.LCGRng options.seed)
        let firstState = burninInit logLikelihood reorderedParameters firstParas space rng
        step_to_seq step (BurninPhase(firstState), 0) (options.burninLen+options.sampleLen) |> Seq.map fst
