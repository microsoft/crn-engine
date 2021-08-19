module Microsoft.Research.Biology.StabilityZ3.GRNs.GRN

open Microsoft.Research.Biology.StabilityZ3
open Microsoft.Research.Biology.StabilityZ3.Solver
//open System.Globalization
open Microsoft.Research.CRNEngine.Expression
let And = Array.reduce (fun a b -> BAnd (a,b))


type Interactions = int[][]           // NxN matrix  {+1.0 :positive, -1.0:negative, 0.0:no interaction

type Regulation = 
    | Competitive       //Competitive model from Scholes et al. (2018)
    | NonCompetitive    //non-Competitive model from Scholes et al. (2018)
    | Mixed             //regulation model from Zheng et al. (2016)
    | Linear            //linear regulation model from Marcon et al. (2016)
    member this.ToInfo() = 
        match this with
        | Competitive    -> "Competitive [Scholes et al. (2018)]"
        | NonCompetitive -> "Non-Competitive [Scholes et al. (2018)]"
        | Mixed          -> "Mixed [Zheng et al. (2016)]"
        | Linear         -> "Linear [Marcon et al. (2016)]"
    override this.ToString() = 
        match this with
        | Competitive    -> "Competitive"
        | NonCompetitive -> "NonCompetitive"
        | Mixed          -> "Mixed"
        | Linear         -> "Linear"

    static member fromString (s:string) = 
        match s with
        | "Competitive"       -> Competitive    
        | "NonCompetitive"    -> NonCompetitive
        | "Mixed"             -> Mixed         
        | "Linear"            -> Linear
        | _                   -> failwithf "Unknown regulation %s" s

type GrnHypothesis = 
    { regulation     : Regulation 
    ; coop           : float 
    ; pars           : Map<string,float> //known parameters
    ; degradation    : bool
    ; basal          : bool
    ; precision      : float option      //range for exporting parameters
    }
    override this.ToString() = 
        [ sprintf "\tCoop:\t\t%f" this.coop
        ; sprintf "\tDegradation:\t%A" this.degradation
        ; sprintf "\tBasal expression:\t%A" this.basal
        ; sprintf "\tRegulation:\t%s" (this.regulation.ToInfo())
        ]
        |> String.concat "\r\n"


    static member Create(regulation:Regulation) (coop:float) = 
        { regulation   = regulation
        ; coop         = coop
        ; pars         = Map.empty
        ; degradation  = (regulation <> Linear)
        ; basal        = (regulation <> Linear)
        ; precision    = Some 1e-5
        }

let speciesNames = [|"A"; "B"; "C"; "D"; "E"; "F"; "G"; "H"|]
type GRN = 
    { species        : string[]          // N Vector
    ; M              : int               // the first M species are diffusible
    ; interactions   : Interactions          
    }   
    static member ToArguments(g:GRN) =     
        let interactions = g.interactions |> Array.map (Array.map string >> String.concat " ") |> String.concat " "        
        sprintf "%i %i %s" g.species.Length g.M interactions

    static member Create numSpecies numDiffusibles interactions =
        { species = speciesNames |> Array.take numSpecies
        ; interactions = interactions |> Array.chunkBySize numSpecies
        ; M = numDiffusibles
        }

    static member FromArguments (f:string[]) = 
        //let f = s.Split(' ')
        let S = int f.[0]        
        let interactions = f.[2..] |> Array.map int
        GRN.Create S (int f.[1]) interactions

    static member IsStronglyConnected (I:Interactions) = 
        let N = I.Length
        let G = Array.init N (fun i -> I.[i] |> Array.indexed |> Array.filter (fun (_,v) -> v <> 0) |> Array.map fst |> Set.ofSeq)                        
        Microsoft.Research.Biology.Graph.isStronglyConnected G

    static member Isomorphisms M (I:Interactions) = 
        let N = I.Length
        let diff_ids = [0..M-1]
        let nondiff_ids = [M..N-1] 
        
        let diff_perms = Lib.getPerms diff_ids.Length diff_ids
        let nondiff_perms = Lib.getPerms nondiff_ids.Length nondiff_ids
        
        [| for dp in diff_perms do
              for nd in nondiff_perms do 
                let order = dp@nd |> Array.ofList
                let lookup x = order.[x]
                let interactions' = I |> Array.map (Array.permute lookup) |> Array.permute lookup
                yield interactions'
        |]
        |> Set.ofArray
        |> Set.add I
    

    //minimum distance between any isomorphism of GRN a and GRN b
    static member Distance (a:GRN) (b:GRN) = 
        let metric (I1:Interactions) (I2:Interactions) =             
            Array.map2 (fun ia ib -> Array.map2 (fun iia iib -> if iia=iib then 1 else 0) ia ib |> Array.sum) I1 I2
            |> Array.sum            
        
        if a.M <> b.M || a.species.Length <> b.species.Length then
            failwith "Incompatible GRNs"

        GRN.Isomorphisms a.M a.interactions
        |> Set.toArray
        |> Array.map (fun ai -> metric ai b.interactions)
        |> Array.min        




    member this.Connectivity = 
        this.interactions |> Array.map (Array.map System.Math.Abs) 

    member this.NumInteractions = 
        this.interactions |> Array.map (Array.map System.Math.Abs >> Array.sum) |> Array.sum
            

    static member Generate (species:string[]) (M:int) = 
        let N = species.Length
        let blank = 
            { species      = species
            ; M            = M
            ; interactions = Array.init N (fun _ -> Array.init N (fun _ -> 0))            
            }
        
        let all_GRNs = 
            let rec GenerateInteractionsVector n = 
                if n <= 1 then 
                    [[-1]; [0]; [1]]
                else
                    GenerateInteractionsVector (n-1)
                    |> List.map(fun v -> [-1::v; 0::v; 1::v])
                    |> List.concat            
            
            GenerateInteractionsVector (int ((float N)**2.0))
            |> List.map (List.splitInto N >> List.map (List.toArray) >> List.toArray)                   
      

        let connected_GRNs = all_GRNs |> List.filter GRN.IsStronglyConnected
            
        let mutable to_add = connected_GRNs |> Set.ofList 
        let unique_GRNs =
            [
                while not (Set.isEmpty to_add) do                    
                    //pick a network
                    let I = to_add |> Set.toArray |> Array.head                           
                    to_add <- to_add - (GRN.Isomorphisms M I)
                    yield I
            ]        
        
        //return
        let grns = unique_GRNs |> List.map (fun I -> {blank with interactions = I}) |> Array.ofList
        grns, (all_GRNs.Length, connected_GRNs.Length, unique_GRNs.Length)
                
    //member this.AddPars (p:Map<string,float>) = 
    //    match this.crnSettings with 
    //    | Some s -> 
    //        let pars' = 
    //            s.pars
    //            |> Map.toArray
    //            |> Array.append (p |> Map.toArray)
    //            |> Map.ofArray                    
    //        {this with crnSettings = Some {s with pars = pars'}}
    //    | None -> this

    member this.InteractionsString= 
        this.interactions |> Array.map (Array.map string >> String.concat " ") |> String.concat " " |> sprintf "%s"

    member this.InteractionsMatrixString = 
        this.interactions |> Array.map (Array.map string >> String.concat ",") |> String.concat ";\n//\t" |> sprintf "//\t%s"

    static member ToCRN (hypothesis:GrnHypothesis)  (grn:GRN) =         
        let MkPar n = 
            if hypothesis.pars.ContainsKey n then 
                match hypothesis.precision with 
                | Some x -> 
                    let xx = hypothesis.pars.[n]
                    sprintf "%s = %f, {distribution=Uniform(%f,%f)}" n xx (xx-xx*x) (xx+xx*x)
                | None -> sprintf "%s = %f" n hypothesis.pars.[n] 
            else 
                sprintf "%s = 1.0, {variation=Random}" n        
        
        //parameters directive
        let degradation_params = 
            if hypothesis.degradation then 
                grn.species |> Array.map (sprintf "g_%s" >> MkPar)
            else  
                Array.empty
                
        let basal_expression_params = 
            if hypothesis.basal then 
                grn.species |> Array.map (sprintf "b_%s" >> MkPar)
            else
                Array.empty

        let max_expression_params = 
            match hypothesis.regulation with 
            | Linear -> Array.empty //no maximal expression for linear CRNs
            | _      -> grn.species |> Array.map (sprintf "h_%s" >> MkPar) //called V in Scholes et al. (2018)
           
        let hill_params =                 
            grn.species 
            |> Array.mapi(fun i s ->
                grn.species
                |> Array.mapi(fun j s' ->
                    if grn.interactions.[i].[j] = 0 then Array.empty
                    else                    
                        [|sprintf "K_%s%s" s s' |> MkPar|]
                    )
                |> Array.concat
                )
            |> Array.concat                        

        let all_params = 
            [| degradation_params
             ; basal_expression_params
             ; max_expression_params
             ; hill_params
            |] 
            |> Array.concat
            |> String.concat ";\n\t"        
        
        let directive_parameters = sprintf "directive parameters [\n\t%s\n]" all_params
                               
        //spatial directive
        let diffusion_params = 
            let var_diff = 
                grn.species.[1..grn.M-1] 
                |> Array.map(fun s -> 
                    let d =  sprintf "D_%s" s
                    if hypothesis.pars.ContainsKey d then sprintf "%s=%f" s hypothesis.pars.[d]
                    else sprintf "%s=%s" s d)
                |> String.concat ";"
            sprintf "[%s=1.0;%s]" grn.species.[0] var_diff
        
        let directive_spatial = sprintf "directive spatial { nx=101; xmax=0.05; diffusibles=%s; random=0.2 }" diffusion_params

        //reactions
        let degradation_rxn = 
            if hypothesis.degradation then 
                grn.species |> Array.map (fun s -> sprintf "%s->{g_%s}" s s)
            else
                Array.empty

        let basal_rxn = 
            if hypothesis.basal then 
                grn.species |> Array.map (fun s -> sprintf "->{b_%s} %s" s s)
            else
                Array.empty
        
        //given a set of activators (A) and a set of repressors (R), produces the equation of the expression rate
        let expression_rate target (A:string seq) (R:string seq) = 
            let activation s = sprintf "(1.0 / (1.0 + (K_%s%s/[%s])^%f))" s target s hypothesis.coop
            let repression s = sprintf "(1.0 / (1.0 + ([%s]/K_%s%s)^%f))" s s target hypothesis.coop
            let ratio s = sprintf "(([%s]/K_%s%s)^%f)" s s target hypothesis.coop
            (*let activation s = 
                match hypothesis.coop with
                | 0.5 -> sprintf "(1.0 / (1.0 + (K_%s%s/[%s])^0.5))" s target s
                | 1.0 -> sprintf "(1.0 / (1.0 + (K_%s%s/[%s])))" s target s
                | 2.0 -> sprintf "(1.0 / (1.0 + (K_%s%s*K_%s%s/[%s]/[%s])))" s target s target s s
                | _   -> failwithf "Unhandled cooperativity %f" hypothesis.coop
            let repression s = 
                match hypothesis.coop with
                | 0.5 -> sprintf "(1.0 / (1.0 + ([%s]/K_%s%s)^0.5))" s s target
                | 1.0 -> sprintf "(1.0 / (1.0 + ([%s]/K_%s%s)))" s s target
                | 2.0 -> sprintf "(1.0 / (1.0 + ([%s]*[%s]/K_%s%s/K_%s%s)))" s s s target s target
                | _   -> failwithf "Unhandled cooperativity %f" hypothesis.coop
            let ratio s = 
                match hypothesis.coop with
                | 0.5 -> sprintf "(([%s]/K_%s%s)^0.5)" s s target
                | 1.0 -> sprintf "([%s]/K_%s%s)" s s target
                | 2.0 -> sprintf "([%s]*[%s]/K_%s%s/K_%s%s)" s s s target s target
                | _   -> failwithf "Unhandled cooperativity %f" hypothesis.coop*)

            let combine (S:string seq) (op:string) (fn:string -> string) = 
                if Seq.isEmpty S then "1.0"
                else
                    S
                    |> Seq.map fn
                    |> String.concat op
                    |> sprintf "(%s)"

            match hypothesis.regulation with
            | Competitive    -> // Eqn (3) in Scholes et al. (2018)
                //NOTE: we need a different default value for combine because of the addition                
                let repression_rate = if Seq.isEmpty R then "0.0" else combine R "+" ratio
                if Seq.isEmpty A then  //special case for repression only
                    sprintf "h_%s*(1.0/(1.0+%s))" target repression_rate
                else
                    let activation_rate = if Seq.isEmpty A then "0.0" else combine A "+" ratio
                    sprintf "h_%s*(%s/(1.0+%s+%s))" target activation_rate activation_rate repression_rate
                
            | NonCompetitive -> // Eqn (4) in Scholes et al. (2018)            
                let activation_rate = combine A "*" activation
                let repression_rate = combine R "*" repression                         
                sprintf "h_%s*%s*%s" target activation_rate repression_rate            
            
            | Mixed ->          // from Zheng et al. (2016)
                let activation_rate = combine A "+" activation                    
                let repression_rate = combine R "*" repression                             
                sprintf "h_%s*%s*%s" target activation_rate repression_rate

            | Linear -> //from Marcon et al. (2016)
                let linearAct s = sprintf "([%s]*K_%s%s)" s s target                         
                let linearRep s = sprintf "(-[%s]*K_%s%s)" s s target                                                         
                let activation_rate = if Seq.isEmpty A then "0.0" else combine A "+" linearAct
                let repression_rate = if Seq.isEmpty R then "0.0" else combine R "+" linearRep
                sprintf "%s+%s" activation_rate repression_rate

        let expression_rxn =             
            grn.species
            |> Array.mapi(fun i s' -> 
                let activators, repressors = 
                    grn.interactions                    
                    |> Array.indexed
                    |> Array.fold(fun (pos,neg) (j, interactions) ->
                        if interactions.[i] > 0 then grn.species.[j]::pos, neg
                        elif interactions.[i] < 0 then pos, grn.species.[j]::neg
                        else pos, neg
                        ) ([], [])      
                        
                sprintf "->[%s] %s" (expression_rate s' activators repressors) s' //overall reaction
                )
               
        let initials = 
            grn.species
            |> Array.choose(fun s -> if hypothesis.pars.ContainsKey s then Some (sprintf "init %s %f" s hypothesis.pars.[s]) else None)
                                     
        let reactions =             
            [| initials
             ; degradation_rxn
             ; basal_rxn
             ; expression_rxn
            |] 
            |> Array.concat                
            |> String.concat " |\n"
             
        sprintf "//%s\n\n%s\n%s\n%s" grn.InteractionsString directive_parameters directive_spatial reactions
        //|> Microsoft.Research.CRNEngine.Crn.from_string
    



    static member CheckTuring (hypothesis:GrnHypothesis) (solver:solverType) solverSettings (grn:GRN)= 
        let encode_to_z3 = true //for larger systems, it is more efficient to avoid the intermediate constraints        
        //let solver       = Solver.solverType.PortfolioTO 5000u              
            
        let S = grn |> GRN.ToCRN hypothesis |> Microsoft.Research.CRNEngine.Crn.from_string |> Dynamical.fromCRN
        
        //add interaction constraints
        //let regulation_cst = 
        //    this.interactions
        //    |> Array.map (fun i -> 
        //        let a, b = Var(i.a), Var(i.b)
        //        if i.positive then Gt (a, b) else Lt (a, b))   //note that a=b is not allowed (no regulation)                
        //    |> And
        
        //let sensitivity_cst = 
        //    this.interactions
        //    |> Array.map (fun i -> 
        //        let K = Var(i.K)
        //        Gt(K,Float 0.0))
        //    |> And

        //let LBcst = 
        //    S.Eqns 
        //    |> Array.map(fun e -> ExpressionFunctions.GetVars e) 
        //    |> Array.reduce (+)                    
        //    |> Set.toArray
        //    |> Array.map(fun v -> BGT(Key v,Float 0.0))
        //    |> And
        
        let diffusion_cst = 
            grn.species 
            |> Array.skip 1 //first diffusible
            |> Array.take (grn.M - 1)
            |> Array.map(fun s -> BLT(Key (sprintf "D_%s" s), Float 1.0))
            |> And


        
        let result = 
            S
            |> Dynamical.setAllLB 0.0
            //|> Dynamical.addCst regulation_cst
            //|> Dynamical.addCst sensitivity_cst
            //|> Dynamical.addCst LBcst
            |> Dynamical.addCst diffusion_cst 
            |> Solver.CheckTuring encode_to_z3 solver solverSettings
                                
        let satCRN = 
            match result.solution with 
            | SAT (s,_) -> GRN.ToCRN {hypothesis with pars = s} grn |> Some
            | _         -> None

        {S with solution = result.solution}, satCRN //TODO: consistent return types?        