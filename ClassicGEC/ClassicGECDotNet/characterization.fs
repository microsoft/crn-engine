module GEC.Characterization

open Microsoft.Research.Filzbach //MCMC inference
open System.Collections.Generic
open System
open System.Text
open System.Text.RegularExpressions



type GrowthModels = Gompertz = 0 | Richards = 1 | Logistic = 2
type GrowthModelType = {
    mu: float;
    lag: float;
    tm: float;
    K: float;
    res: int;
    model: (int * float -> float); 
}
type NoiseModel = Linear=0| Squared=1

type DataTypes = Data | Gain | Blank | Negative
type Well = {
    Row: char;
    Col: int;
    Content: string;
    Colony: string;
    DataType: DataTypes;
    Condition: Map<string,double>;
    
    Signals: double[][];
    SignalNames: string[];
    T: double[];
    
    GrowthModel: GrowthModelType option;
    Filter: int -> bool; //given an signal array index, is this part of the selection
    SteadyStates: Map<(string*string),(double*double)>;
    Activity: Map<(string*string),double>;
}
type Plate = {
    Name: string;
    Data: seq<Well>;
    Blanks: Map<string,double>;
    
    Conditions: Map<string,double[]>;
    Cname: string;
    C: double[]; //conditions
    Properties: double[][][]; //computed or measured properties
    PropertyNames: string[];    //name of properties

    Gain: Map<string*string,double>;    
    log:string;
}

let EmptyPlate = {Name=""; Data = Seq.empty; Blanks = Map.empty; Conditions = Map.empty; C = [||];Properties=[||]; PropertyNames=[||]; Cname=""; Gain = Map.empty; log=""}
let EmptyWell = {
    Row = ' ';
    Col = -1;
    Content = "";
    Colony = "";
    DataType = DataTypes.Blank;
    Condition = Map.empty;
    Signals = Array.empty;
    SignalNames = Array.empty;
    T = Array.empty;
    GrowthModel = None;
    Filter = (fun x -> true);
    SteadyStates = Map.empty;
    Activity = Map.empty;
}
type Parameter = {
    pname: string;
    value: double;
    dist: double[];
    log: bool;
}
type ModelTypes = 
    | Constitutive = 0
    | Linear = 1
    | Michaelis_Menten_Activation = 2
    | Michaelis_Menten_Repression = 3
    | Michaelis_Menten_General = 4
    | Dimer_Activation = 5
    | Dimer_Repression = 6
    | Dimer_General = 7
    | Hill_Activation = 8
    | Hill_Repression = 9
    | Hill_General = 10 
    | Sequential_Binding = 11

type ResponseTypes = 
    | EYFP_ECFP = 0
    | EYFP_OD = 1
    | ECFP_OD = 2
    | mu = 3
    | K = 4
    | lag = 5
    | EYFP_mRFP1 = 6
    | ECFP_mRFP1 = 7
    | mRFP1_OD = 8

type Model = {
    modelType: ModelTypes;
    AIC: double;
    BIC: double;
    CIC: double; //rename this information criterion to the right abbreviation
    model: int * double -> double;
    res: int;
    parameters: Parameter[];
    X: double[];
    bayestable: List<double[]>;
}


let string_of_response response = match response with
                                  | ResponseTypes.EYFP_ECFP -> "EYFP/ECFP"
                                  | ResponseTypes.EYFP_OD -> "EYFP/OD"
                                  | ResponseTypes.ECFP_OD -> "ECFP/OD"
                                  | ResponseTypes.mu -> "mu"   
                                  | ResponseTypes.K -> "K"
                                  | ResponseTypes.lag -> "lag"     
                                  | ResponseTypes.EYFP_mRFP1 -> "EYFP/mRFP1"
                                  | ResponseTypes.ECFP_mRFP1 -> "ECFP/mRFP1"
                                  | ResponseTypes.mRFP1_OD -> "mRFP1/OD"



let Interp (X:double[]) (Y:double[]) (Z:double[]) = 
    let InterpVal xt = 
        match Array.tryFindIndex(fun x -> x=xt) X with
        | Some n -> Y.[n] //exact match -> return the value
        | None -> //no exact match, interpolate 
            let (P,N) = Array.fold2(fun (P,N) x y ->
                                                 let dx = x-xt in 
                                                 if dx>0.0 then ((dx,y)::P,N)
                                                 else (P,(dx,y)::N)) ([],[]) X Y in                               
            let (x1,y1) = Seq.maxBy(fun (x,y) -> x) N in //largest negative element (on the left of target)
            let (x2,y2) = Seq.minBy(fun (x,y) -> x) P in //smallest positive element (on the right of target)
        
            let (x1,x2) = (x1+xt, x2+xt) in //expresss xt as a linear combination
            let a = (xt - x2)/(x1-x2) in
            let y = a*y1 + (1.0-a)*y2 in
            y 
    in 
    Array.map(fun x -> InterpVal x) Z      



let LoadPlate (data:string) =    
    let ParseLine (str: string list) =
        let fields = str.Head.Split([|','|]) |> Array.toList
        (fields, str.Tail)

    let rec Skip n l = 
          match (n, l) with
          | 0, _ -> l
          | _, [] -> []
          | n, _ :: ls -> Skip (n-1) ls
    in
                                     
    //load the time and ids information from the header files; returns (signal name * id * time in min) 
    let ParseHeaders (hdr:string list) =                 
       let regex = new Regex(@"\((?<signal>.*)\)\s*(?<id>\d+)\s*-\s*(?<h>\d+)\s*h\s*((?<m>\d+)\s*min)?", RegexOptions.IgnoreCase) in                
       Seq.map(fun x-> let mc = regex.Match(x)
                       let signal = mc.Groups.Item("signal").ToString()
                       let id = (int)(mc.Groups.Item("id").ToString())
                       let h = (double)(mc.Groups.Item("h").ToString())
                       let m = let ms = mc.Groups.Item("m").ToString()
                               if (ms <> "") then (double)ms else 0.0
                       signal,id , h * 60.0 + m) (Skip 5 hdr)    //The number of skips has to match the meta-data columns of the file    
        

    let data = data.Replace('\r',' ') in
    let lines = data.Split([|'\n'|]) |> Array.toList  in     
    let (headers, lines) = ParseLine(lines) in //read in the headers line
    let headers = ParseHeaders(headers) in //parse the headers and extract time and samples information
    let lines = Skip 1 lines in //get rid of averages line
    let lines = Seq.fold(fun acc (x:string) -> if x.Replace(',',' ').Trim()="" then acc else acc@[x]) [] lines in //remove empty lines
    //parse the wells and data
    let data = Seq.map(fun (line:string) ->
                                    let fields = line.Split(',') |> List.ofArray in //String.split([',']) line in 
                                    match fields with 
                                    | content::colony::col::row::cond::data ->        
                                        let signals =   Seq.map2(fun (s,_,t) v -> (s,t,(double)v)) headers data                                                                                                                                                                       
                                                        |> Seq.groupBy(fun (s,t,v) -> s)
                                                        |> Seq.map(fun (k,x) -> (k,Seq.map(fun (s,t,v) -> t,v) x |> Seq.sortBy(fun (t,v) -> t)))
                                                        |> Seq.map(fun (k,x) -> (k,Seq.map(fun (t,_) -> t) x |> Array.ofSeq , Seq.map(fun (_,y) -> y) x |> Array.ofSeq)) in
                                        let (_,T,OD)  = Seq.find(fun (s:string,_,_) -> s.ToLower()="od") signals in                                                      
                                        let signals = Seq.map(fun (s,Tc,S) -> if Tc=T then (s,S) 
                                                                              else (s, Interp Tc S T)
                                                              ) signals in  //TODO: THROW A WARNING WHEN INTERPOLATING
                                        let condition = Seq.fold(fun acc (x:string) -> 
                                                                    if (x.Length>0) then 
                                                                        let y = x.Split '='  in 
                                                                        (y.[0],Double.Parse(y.[1]))::acc
                                                                    else
                                                                        acc
                                                                ) [] (cond.Split(';')) 
                                                         |> Map.ofSeq in            
                                        let dT = if content.ToLower().Contains("gain") then Gain 
                                                 else if content.ToLower().Contains("blank") then Blank
                                                 else if content.ToLower().Contains("negative") then Negative
                                                 else Data in                                        

                                        let names = Seq.map(fun (n,_) ->n) signals |> Array.ofSeq in
                                        let signals = Seq.map(fun (_,s) ->s) signals |> Array.ofSeq in
                                        let row = row.Trim().[0] in
                                        let col = Convert.ToInt32(col) in
                                        {
                                            EmptyWell with
                                            Row = row;
                                            Col = col;
                                            Content = content;
                                            Colony = colony;
                                            DataType = dT;
                                            Condition = condition;
                                            Signals = signals;
                                            SignalNames = names;
                                            T = T;                                            
                                        }
                                    //| _ -> failwith "Headers in the wrong format."
                        ) lines in
    let conditions = Seq.fold (fun ag well -> Seq.append(ag) (well.Condition|>Map.toSeq)) Seq.empty data 
                     |> Seq.groupBy fst
                     |> Seq.map(fun (c,vals) -> c, Seq.map snd vals |> Seq.distinct |> Array.ofSeq)
                     |> Map.ofSeq in

    {EmptyPlate with Data=data; Conditions=conditions}


let getSignal(well:Well, str:string) = 
    match Array.tryFindIndex(fun x -> x=str) well.SignalNames with
    |Some n -> (well.T,well.Signals.[n])
    |None -> (Array.empty, Array.empty)
let getSignals(well:Well, s1:string, s2:string) = 
    match (getSignal(well,s1), getSignal(well,s2)) with
    |((X1,Y1), (X2,Y2)) when not (Array.isEmpty(Y1)) && not (Array.isEmpty(Y2)) -> (Y1,Y2)
    |_ -> (Array.empty, Array.empty)



let getFluorescence (well:Well) = 
    let namedSignals = Array.map2(fun name signal -> (name,signal)) well.SignalNames well.Signals 
                       |> Seq.filter(fun (name,signal) -> not (name="OD")) in     //ignore the growth signal
    (well.T,namedSignals)


let getFOd (well:Well) = 
    let namedSignals = Array.map2(fun name signal -> (name,signal)) well.SignalNames well.Signals in
    let F = Seq.filter(fun (n,_) -> not (n = "OD")) namedSignals in
    let OD = snd (Seq.find(fun (n,_) -> n="OD") namedSignals) in
    (OD,F)

let getFF (well:Well, chan:string) = 
    let namedSignals = Array.map2(fun name signal -> (name,signal)) well.SignalNames well.Signals in
    let F = Seq.filter(fun (n,_) -> not (n = chan || n="OD")) namedSignals in
    let X = snd (Seq.find(fun (n,_) -> n=chan) namedSignals) in
    (X,F)



let MergePlates (plate1:Plate, plate2:Plate) = 
    let name = "Merged " + plate1.Name + " and " + plate2.Name in
    let data = Seq.append plate1.Data plate2.Data in
    let conditions = Map.fold (fun s k v ->
        match Map.tryFind k s with
        | Some v' -> Map.add k (Array.ofSeq (Seq.distinct (Seq.append (Seq.ofArray v) (Seq.ofArray v')))) s
        | None -> Map.add k v s) plate1.Conditions plate2.Conditions in
    { EmptyPlate with Name = name; Data = data; Conditions = conditions }

let Median (vals:seq<double>) = 
    let sorted = Seq.sort vals |> List.ofSeq in
    let n = Seq.length sorted in
    if n % 2 =0 then
        sorted.[n/2] 
    else
        let mp = Convert.ToDouble(n)/2.0 in
        let l = Convert.ToInt32(floor(mp/2.0)) in
        let h = Convert.ToInt32(ceil(mp/2.0)) in
        (sorted.[l] + sorted.[h]) /2.0



(* Linear least squares *)
let LsqLin (Xi:double[]) (Yi:double[]) =  
    let sum X = Array.fold(fun s x -> s + x) 0.0 X 
    let ave X = (sum X)/((float)(Array.length X)) 
    let mx = ave Xi 
    let my = ave Yi
    let S1 = Array.map2(fun x y -> (x-mx)*(y-my)) Xi Yi |> sum
    let S2 = Array.map(fun x-> Math.Pow((x-mx),2.0)) Xi |> sum
    let a = if S2 = 0.0 then failwith "Linear regression failed because of divide by zero." else S1/S2
    let b = my - a * mx
    (a,b)


let CorrectBlanks (plate:Plate) =
    let ComputeBlanks (plate:Plate) = 
        let blanks = Seq.filter(fun x -> x.DataType = DataTypes.Blank)  plate.Data in
        let signals = Seq.fold(fun acc well ->  Array.map2(fun name signal -> (name,signal)) well.SignalNames well.Signals |> Seq.append(acc)) Seq.empty blanks
                      |> Seq.groupBy(fun (s,x) ->s)  
                      |> Seq.map(fun (s,x) -> (s, Seq.fold(fun acc (_,v) -> Seq.append(acc) v) Seq.empty x|> Median)) //compute the median of the background                                            
                      //|> Seq.map(fun (s,x) -> (s, Seq.fold(fun acc (_,v) -> Seq.append(acc) v) Seq.empty x|> Seq.average)) //compute the average of the background
                      //|> Seq.filter(fun (s,_) -> not (s = "OD")) //use this to avoid the correction of OD data
                      //|> Seq.filter(fun (s,_) -> (s = "OD")) //use this to only allow the correction of OD data
                      |> Map.ofSeq          in
                      {plate with Blanks=signals}    //Check this?  
        in
    let BlankCorrect (blanks:Map<string,double>) (well:Well) = 
        {well with Signals = Array.map2(fun name signal -> if blanks.ContainsKey(name) then
                                                               let df = blanks.Item(name) in
                                                               Array.map(fun x -> x - df) signal
                                                           else
                                                               signal
                                       ) well.SignalNames well.Signals}
    in
    let plate = ComputeBlanks plate in
    let bc = BlankCorrect plate.Blanks in
    let newData = Seq.map(fun w -> bc w) plate.Data |> Seq.cache in
    {plate with Data = newData;}
 


// let CorrectBlanksAndGain (plate:Plate) = 
//    let ComputeBlanks (plate:Plate) = 
//        let blanks = Seq.filter(fun x -> x.DataType = DataTypes.Blank)  plate.Data in
//        let signals = Seq.fold(fun acc well ->  Array.map2(fun name signal -> (name,signal)) well.SignalNames well.Signals|> Seq.append(acc)) Seq.empty blanks
//                      |> Seq.groupBy(fun (s,x) ->s)  
//                      |> Seq.map(fun (s,x) -> (s, Seq.fold(fun acc (_,v) -> Seq.append(acc) v) Seq.empty x|> Median))
//                      //|> Seq.filter(fun (s,_) -> not (s = "OD")) //use this to avoid the correction of OD data
//                      |> Map.ofSeq          in
//            {plate with Blanks=signals}      
//        in
//    let BlankCorrect (blanks:Map<string,double>) (well:Well) = 
//        {well with Signals = Array.map2(fun name signal -> if blanks.ContainsKey(name) then
//                                                               let df = blanks.Item(name) in
//                                                               if name="OD" then
//                                                                Array.map(fun x -> x - df) signal //use the original (subtraction) for OD [no gain]
//                                                               else
//                                                                Array.map(fun x -> x/df-1.0) signal //use the modified procedures for all other (fluorescent) channels
//                                                            else
//                                                               signal) well.SignalNames well.Signals}
//    in
//    let plate = ComputeBlanks plate in
//    let bc = BlankCorrect plate.Blanks in
//    let newData = Seq.map(fun w -> bc w) plate.Data |> Seq.cache in
//    {plate with Data = newData;}





let Add (s1:double[]) (s2:double[]) = Array.map2(fun x y -> x+y) s1 s2
let Div (s:double[]) (n:double) = Array.map(fun x -> x/n) s

let CorrectAutofluorescence (plate:Plate) =
    let negatives = Seq.filter(fun x -> x.DataType = DataTypes.Negative) plate.Data |> Seq.map(fun w -> w) in
    
    if Seq.isEmpty negatives then  //no autofluorescence information was found, proceed without correcting but raise a message
        //TO DO: Include a delegate for displaying information to the user        
        {plate with log=plate.log + "\n No autofluorescence callibration was performed!\n";}
    else    
//        let T = (Seq.head negatives).T in
//        let autofl = Seq.fold(fun acc well -> let ns = (Seq.map2(fun name signal -> (name,Interp well.T signal T)) well.SignalNames well.Signals) in Seq.concat([acc; ns])) Seq.empty negatives
//                         |> Seq.groupBy(fun (s,x) ->s) 
//                         |> Seq.map(fun (s,x) -> (s,Seq.map(fun (_,y) -> y) x))                   
//                         |> Seq.map(fun (s,x) -> 
//                            let n = Convert.ToDouble(Seq.length x) in
//                            (s,Seq.fold(fun acc y -> Add acc (Div y n)) (Div (Seq.head x) n) (Seq.skip 1 x)))
//                         |> Map.ofSeq
//                       in                       
//        let AutoCorrect (well:Well) = 
//            {well with Signals = Array.map2(fun name signal -> if not (name="OD") then
//                                                                let Df = Interp T (autofl.Item(name)) well.T in
//                                                                Array.map2(fun x df -> x - df) signal Df
//                                                           else
//                                                                signal) well.SignalNames well.Signals}
//        in
//        let newData = Seq.map(fun w -> AutoCorrect w) plate.Data |> Seq.cache in
//       {plate with Data = newData}
//            
        let autofl = Seq.map(fun w -> let od= getSignal(w,"OD") |> snd |> Seq.mapi(fun i x -> (i,x)) |> Seq.filter(fun (i,x) -> w.Filter(i)) |> Seq.map(fun (i,x) -> x) |> Array.ofSeq in                                                                  
                                      Seq.map2(fun name signal -> if not (name="OD") then 
                                                                    let signal = signal |> Seq.mapi(fun i x -> (i,x)) |> Seq.filter(fun (i,x) -> w.Filter(i)) |> Seq.map(fun (i,x) -> x) |> Array.ofSeq in
                                                                    (name,od,signal) 
                                                                  else 
                                                                    ("",[||],[||])
                                               ) w.SignalNames w.Signals
                                    |> Seq.filter(fun (n,_,_) -> not (n=""))
                            ) negatives
                    |> Seq.fold(fun acc s -> Seq.append(acc) s) Seq.empty
                    |> Seq.groupBy(fun (s,_,_) ->s)                         
                    |> Seq.map(fun (s,x) -> (s,Seq.fold(fun (OD,F) (_,od,f) -> (Seq.append(OD) od,Seq.append(F) f)) (Seq.empty,Seq.empty) x))                        
                    |> Seq.map(fun (s,(OD,F)) -> (s,LsqLin (OD |> Array.ofSeq) (F |> Array.ofSeq)))
                    |> Map.ofSeq
        in               
        let AutoCorrect (well:Well) = 
            {well with Signals = let od = getSignal(well,"OD") |> snd in
                                 Array.map2(fun name signal -> 
                                    if not (name="OD") then
                                         let (a,b) = autofl.Item(name) in                                                                    
                                         Array.mapi(fun i x -> x - a*od.[i]+b) signal                                                                 
                                    else
                                         signal) well.SignalNames well.Signals}
        in
        let newData = Seq.map(fun w -> AutoCorrect w) plate.Data |> Seq.cache in
        {plate with Data = newData}















    //model parameters, Filzbach parameters, likelihood (param->double)
//Replace this with the built-in Filzbach scriptable
type Filzbach(mparam,likefn) =    
    class                            
        inherit ModelBase(new System.Random())                  
        override this.setup_data() = ()
        override this.setup_parameters() =
            Seq.iter(fun (name,lb,ub,v,t) -> this.parameter_create(name,lb,ub,v,t,0,0)) mparam;
        override this.likelihood() =            
            let cv = Seq.map(fun (name,_,_,_,_) -> this.cv(name)) mparam 
                     |> Seq.cache in
            let log_like = likefn cv in            
            this.set_metr_ltotnew(log_like);
            this.inc_metr_number_ok(1.0);                       
        member this.ltotmax = 0.0;
        member this.Run(fparam) = 
            this.setup_data();
            this.setup_parameters();            
            let burnin,eststeps,burnin2,mleexp = fparam in
            this.runmcmc(burnin,eststeps,burnin2,mleexp, null, null, null,null);                                                  
            this.params_set_to_MLE(); //or set_to_posterior_mean?
            Seq.map(fun (name,_,_,_,_) -> this.cv(name)) mparam            
            |> Seq.cache
        member this.GetParamHist() = this.bayestable
    end
    





//growth models, inverse and derivatives
(* 
Gompertz model is reparametrized as in:
Zwietering MH, Jongenburger I, Rombouts FM, Van’t Riet K (1990) 
Modelling of the bacterial growth curve. 
Appl Environ Microbiol 56:1875–1881 
*)
let Gompertz(mu, K, lag) t = K*exp(-exp((lag-t)*(mu*exp(1.0)/K)+1.0)) //Gompertz growth model. 
//let GompInv(mu, K, lag) od  = lag-(log(-log(od/K))-1.0)*K/(mu*exp(1.0)) //Inverse Gompertz function 
//let GompDer(mu, K, lag) t = mu*exp(exp(1.0)*mu*(lag-t)/K-exp(exp(1.0)*mu*(lag-t)/K+1.0)+2.0) //Gompertz derivative function
let Richards(mu,K,lag,v) t = 
   // let A = Math.Pow(mu/K*(1.0+v),(1.0+1.0/v))*(lag-t) in
    let A = mu/K*Math.Pow((1.0+v),(1.0+1.0/v))*(lag-t) in
    let B = 1.0+v*exp(1.0+v)*exp(A) in
    K*Math.Pow(B,-1.0/v)
let Logistic(mu,K,lag) t = 
    K * exp(-exp(mu*exp(1.0)/K*(lag-t)+1.0))


//runs Filzbach using a selected growth model and returns the identified parameters (mu,K,lag)

let FitGrowthModel (modelType) (plate:Plate) = 
    let FitGrowthModelOD(ODi, model) =                                      
            

        //od fit
        //let od0 = Seq.map(fun (t, od) -> od) ODi |> Seq.min in //use smallest OD measurement to avoid negative value                              
//        let od0 = snd (Seq.nth 0 ODi) in //use the first OD value                
//        let OD = Seq.map(fun (t,od) -> (t, od/od0)) ODi  |> Seq.cache in    

        //window slope calculation procedure for growth        
//        let od0 = ODi |> Seq.map(fun (_,x) -> x) |> Seq.min in
//        let OD = Seq.map(fun (t,od) -> (t,od - od0) ) ODi in
//        let OD = Seq.map(fun (t,od) -> (t, log(od))) OD  |> Seq.cache in    
//        let mus = new List<float>() in
//        let ws = 50 in //window size
//        for i in ws..((Seq.length OD)-ws) do
//            let a = OD|> Seq.skip(i) |> Seq.take(5) |> Seq.fold(fun (acc1, acc2) (x,y) -> (List.concat([acc1; [x]]), List.concat([acc2;[y]]))) ([],[]) in
//            let (A,B) = (fst a |> Array.ofSeq, snd a |> Array.ofSeq) in
//            mus.Add(fst (LsqLin A B));        
//        done;                
//        let mx = mus |> Seq.filter(fun x-> not (Double.IsNaN(x) || Double.IsInfinity(x))) |> Seq.max in

        let OD = Seq.map(fun (t,od) -> (t,if od<=0.0 then 1.0e-2 else od)) ODi |> Seq.cache in //negative values are bad when using log fits            
        //let OD = Seq.filter(fun (t,od) -> od>0.0) ODi |> Seq.cache in //negative values are bad when using log fits    
        let od0 = snd (Seq.nth 0 OD) in //use the first OD value                
        let OD = Seq.map(fun (t,od) -> (t, log (od/od0))) OD  |> Seq.cache in    

        match model with 
        | GrowthModels.Logistic -> 
            let fparam = 2000, 2000, 500, 500 in    
            let mparam = Seq.ofList[("mu", 0.0,  1.0,  0.25, 0); 
                                    ("K",  0.0,  10.0, 1.0,   0); 
                                    ("lag",-500.0,  500.0, 100.0, 0);
                                    ("s",  1e-8, 1e2,  1e-8, 1)] in
            let likefn param =  let (mu,K,lag,s) = Seq.nth 0 param,Seq.nth 1 param,Seq.nth 2 param,Seq.nth 3 param in
                                Seq.fold (fun sum (x,y) ->                 
                                    let obs = Logistic(mu, K, lag) x in                                                                
                                    let noise = s in                                
                                    sum + log(ModelBase.normal_density(y,obs,noise))) 0.0 OD in        
            let FilzGrowth = Filzbach(mparam,likefn) in
            let param = FilzGrowth.Run(fparam) in
            let mu = Seq.nth 0 param in
            let K = Seq.nth 1 param in
            let lag = Seq.nth 2 param in
            let e = Math.Exp(1.0) in
            let maturation_offset = 50.0 in
            let tm  = lag + K/(2.0*mu) + maturation_offset in
            let ParamHist = FilzGrowth.GetParamHist() in     
            let res = ParamHist.Count in   
            let RandLogistic(id, x) = 
                let param = ParamHist.[id] in //random parameter from posterior
                let (mu,K,lag) = param.[2], param.[3],param.[4] in
                let y = Logistic(mu,K,lag) x in
                od0 * Math.Exp(y)                
                //y

            in
            mu, lag, tm, K,res, RandLogistic, OD
        
            
        | GrowthModels.Gompertz -> 
            let fparam = 2000, 2000, 500, 500 in   
            let mparam = Seq.ofList[("mu", 0.0,  1.0,  0.25, 0); 
                                    ("K",  0.0,  10.0, 1.0,   0); 
                                    ("lag",-500.0,  500.0, 100.0, 0);                                 
                                    ("s",  1e-8, 1e3,  1e-8, 1)] in                                   
            let likefn param =  let (mu,K,lag,s) = Seq.nth 0 param,Seq.nth 1 param,Seq.nth 2 param,Seq.nth 3 param in
                                Seq.fold (fun sum (x, y) ->                 
                                    let obs = Gompertz(mu, K, lag) x in                                
                                    let noise = s in        
                                    sum + log(ModelBase.normal_density(y,obs,noise))) 0.0 OD in        
            let FilzGrowth = Filzbach(mparam,likefn) in
            let param = FilzGrowth.Run(fparam) in
            let mu = Seq.nth 0 param in         
            let K = Seq.nth 1 param in
            let lag = Seq.nth 2 param in            
            let e = Math.Exp(1.0) in
            let maturation_offset = 50.0 in
            let tm  = (K + e*lag*mu)/(e*mu) + maturation_offset in
            let ParamHist = FilzGrowth.GetParamHist() in   
            let res = ParamHist.Count in
            let RandGompertz(id, x) = 
                let param = ParamHist.[id] in //random parameter from posterior
                let (mu,K,lag) = param.[2], param.[3],param.[4] in
                let y = Gompertz(mu,K,lag) x in
                od0*Math.Exp(y)                
                //y
            in 
            mu, lag, tm, K,res, RandGompertz, OD
     //       mx, lag, tm, K,res, RandGompertz, OD


        | GrowthModels.Richards -> 
            //let fparam = 2000, 2000, 500, 500 in    
            let fparam = 2000, 2000, 500, 500 in   
            let mparam = Seq.ofList[("mu", 0.0,  1.0,  0.25, 0); 
                                    ("K",  0.0,  10.0, 1.0,   0); 
                                    ("lag",-500.0,  500.0, 100.0, 0);
                                    ("v",0.0,  1.0, 0.01, 0);
                                    ("s",  1e-8, 1e2,  1e-8, 1)] in
            let likefn param =  let (mu,K,lag,v,s) = Seq.nth 0 param,Seq.nth 1 param,Seq.nth 2 param,Seq.nth 3 param, Seq.nth 4 param in
                                Seq.fold(fun sum (x, y) ->                 
                                    let obs = Richards(mu, K, lag,v) x in                                                                
                                    let noise = s in                                
                                    sum + log(ModelBase.normal_density(y,obs,noise))) 0.0 OD in        
            let FilzGrowth = Filzbach(mparam,likefn) in
            let param = FilzGrowth.Run(fparam) in
            let mu = Seq.nth 0 param in
            let K = Seq.nth 1 param in
            let lag = Seq.nth 2 param in
            let e = Math.Exp(1.0) in
            let maturation_offset = 50.0 in
            let tm  = (K + e*lag*mu)/(e*mu) + maturation_offset in
            //construct a random growth sampling function
            let ParamHist = FilzGrowth.GetParamHist() in
            let res = ParamHist.Count in
            let RandRichards(id, x) = 
                let param = ParamHist.[id] in //random parameter from posterior
                let (mu,K,lag,v) = param.[2], param.[3],param.[4],param.[5] in
                let y = Richards(mu,K,lag,v) x in
                od0 * Math.Exp(y)                
                //y
            in
            //return results
            mu, lag, tm, K,res, RandRichards , OD
    in
    let FitGrowthModelWell (modelType) (well:Well)  = 
        let (T,OD) = getSignal(well, "OD") in
        let OD = Seq.map2(fun t od -> (t,od)) T OD in
        let rec runner OD modelType = 
            let (mu,lag,tm,K,res,md,nOD) = FitGrowthModelOD(OD,modelType) in
            if (tm < 0.0) 
            then runner OD modelType 
            else (mu,lag,tm,K,res,md,nOD) in
        let (mu,lag,tm,K,res,md,nOD) = runner OD modelType in
        let nOD = Seq.map(fun (t,od) -> od) nOD |> Array.ofSeq in
        let model = {mu=mu;lag=lag;tm=tm;K=K; res=res;model=md} in
        //let newSignals = Array.mapi(fun i s -> if well.SignalNames.[i]="OD" then nOD else s) well.Signals in
        //{well with Signals = newSignals; GrowthModel = Some model;}
        {well with GrowthModel = Some model;}
    in      
    let newData = Seq.map(fun w -> FitGrowthModelWell modelType w ) plate.Data |> Seq.cache in 
    {plate with Data = newData;}


//physical properties of commonly used fluorescent proteins
//assume all FPs have the same degradation and translation rates - specific values from (Elowitz and Leibler, Nature, 2000) are used.
//CFP, YFP maturation rates are taken from (J. Brown, 2011) which refers to (Gordon et. al., Nat. Meth 2007)
type ReporterPropertiesType = {name:string; degradation:double; maturation:double; translation: double; mRNAdeg:double}

//let degradationRate = 0.0693
let degradationRate = 0.0005

let  ReporterProperties (name:string) = 
    let reporter = name.ToLower() in
    match reporter with
    | "gfpmut3" -> Some {name="GFPmut3"; degradation=degradationRate; maturation=0.0277; translation=0.1443; mRNAdeg=0.001;}
    | "mcherry" -> Some {name="mCherry"; degradation=degradationRate; maturation=0.0186; translation=0.1443; mRNAdeg=0.001;}
    | "ecfp"    -> Some {name="ECFP"; degradation=degradationRate; maturation=0.0139; translation=0.1443; mRNAdeg=0.001;}
    | "eyfp"    -> Some {name="EYFP"; degradation=degradationRate; maturation=0.0173; translation=0.1443; mRNAdeg=0.001;}
    | "mrfp1"    -> Some {name="mRFP1"; degradation=degradationRate; maturation=0.0173; translation=0.1443; mRNAdeg=0.001;}
    | _         -> None

//construct a filter function for each well to capture the exponential phase based on the inferred growth curve.
let SetFilters plate = 
    let newData = Seq.map(fun w ->               
        let newFilter = 
            match w.GrowthModel with 
            | Some model -> 
                 let filter id = 
                    let t = w.T.[id] in
                    t > model.lag && t < 2.0*model.tm-model.lag in                 
                 filter
            | None -> w.Filter
        in               
        { w with Filter = newFilter }
        ) plate.Data |> Seq.cache in
    {plate with Data = newData;}


let FindSteadyStates plate =     
    let filteri filter X = 
        let mapped = Array.mapi (fun i x -> (i,x)) X in
        let filtered = Array.filter (fun (i,x) -> filter i) mapped in
        Array.map (fun (i,x) -> x) filtered
    in   
    let FindSS well = 
        let Filter = filteri well.Filter in
        let Signals = Array.map(fun s -> Filter s) well.Signals in //filter signals
        Array.fold2(fun (map:Map<(string*string),(double*double)>) name signal ->  //for each pair of signals, compute the cross slope
                    Array.fold2(fun map2 name2 signal2 -> 
                        let fit = LsqLin signal2 signal in                        //signal2 is the X, signal is Y;
                        map2.Add((name,name2),fit)) map well.SignalNames Signals)
                   Map.empty well.SignalNames Signals 
    in    
    let newData = Seq.map(fun w -> {w with SteadyStates = FindSS w;}) plate.Data |> Seq.cache in
    {plate with Data=newData;}

let FindActivities (plate:Plate) =     
    let copies = 1.0 in //single copy number assumption
    let FindActivity (well:Well) = 
        Map.toSeq well.SteadyStates
            |> Seq.fold(fun (map:Map<(string*string),double>) ((s1,s2), (a,_)) -> 
                if not (s1="OD") && s2="OD" then
                    let propOpt = ReporterProperties s1 in
                    match propOpt with
                    | Some prop -> 
                        let Pdeg = prop.degradation in
                        let Pmat = prop.maturation in
                        let Ptrans = prop.translation in
                        let mRNAdeg = prop.mRNAdeg in
                        let mu = well.GrowthModel.Value.mu in
                        let factor = mRNAdeg * (Pdeg + mu)*(Pdeg + mu + Pmat)/(Pmat*Ptrans*copies)  in
                        map.Add((s1,s2),factor*a)
                    | None -> map
                else if not (s1="OD") && not (s2="OD") && not(s1=s2) then
                    let prop1 = ReporterProperties s1 in
                    let prop2 = ReporterProperties s2 in
                    match (prop1,prop2)  with
                    | (Some p1, Some p2) -> 
                        let Pdeg1 = p1.degradation in
                        let Pmat1 = p1.maturation in
                        let Ptrans1 = p1.translation in
                        let mRNAdeg1 = p1.mRNAdeg in

                        let Pdeg2 = p2.degradation in
                        let Pmat2 = p2.maturation in
                        let Ptrans2 = p2.translation in
                        let mRNAdeg2 = p2.mRNAdeg in

                        let mu = well.GrowthModel.Value.mu in

                        let F1 = mRNAdeg1 * (Pdeg1 + mu)*(Pdeg1 + mu + Pmat1)/(Pmat1*Ptrans1) in 
                        let F2 = mRNAdeg2 * (Pdeg2 + mu)*(Pdeg2 + mu + Pmat2)/(Pmat2*Ptrans2) in 
                                                                                                
                        map.Add((s1,s2),(F1/F2)*a)
                    | _ -> map
                else
                    map
                ) Map.empty
    in
    let newData = Seq.map(fun w -> {w with Activity = FindActivity w;}) plate.Data |> Seq.cache in
    {plate with Data=newData;}


let ComputeProperties condition default_values (plate:Plate)  = 
    let devices = Seq.groupBy(fun w -> w.Content) plate.Data //group wells by device
                  |> Seq.filter(fun (s:string,w) ->   //remove controls
                    let name = s.ToLower() in 
                    not (name.Contains("blank") || 
                            name.Contains("negative") || 
                            name.Contains("gain"))) in             
    let device = snd (Seq.head devices) in     //select the first devices (TODO: generalize for multiple devices)
    //let d0 = Seq.head device in //select the first well to explore the available conditions

    //TODO: FIX EMPTY CONDITION

    // Filter the wells for matched conditions and their values
    let get_condition well key = if well.Condition.ContainsKey key then well.Condition.[key] else 0.0 in
    
    let device = 
      let filtered_values = Seq.filter (fun (cnd,_) -> not (cnd=condition)) default_values in
      Seq.filter (fun w -> Seq.fold (fun res (c,v) -> res && (get_condition w c = v)) true filtered_values) device in        
       
    let measurements = Seq.groupBy(fun w -> get_condition w condition) device
            |> Seq.map(fun (c, wells) -> (c,Seq.map(fun w -> 
            let activities = Map.toSeq w.Activity |> Seq.map(fun ((s1,s2),v) -> (s1 + "/" + s2, v)) in
            let growth = [("mu", w.GrowthModel.Value.mu); ("lag", w.GrowthModel.Value.lag);("K", w.GrowthModel.Value.K);] |> Seq.ofList in
            Seq.concat([activities; growth])
            |> Map.ofSeq) wells))
    in
    let propertyNames = Seq.head measurements |> snd |> Seq.head |> Map.toSeq |> Seq.map(fun (s,_) -> s) |> Array.ofSeq in
    let C =  Seq.map(fun (s,_) -> s) measurements |> Array.ofSeq in

    let mMap = measurements |> Map.ofSeq in
    let properties = Seq.map(fun c ->  //for each condition
            let d = mMap.[c] in
            Seq.map(fun n -> Seq.map(fun (s:Map<string,double>) -> s.[n]) d |> Array.ofSeq) propertyNames //for each property
            |> Array.ofSeq) C |> Array.ofSeq in
    {plate with C=C; Properties=properties; PropertyNames=propertyNames; Cname=condition;}

let FilterByDevice plate device = 
    let data = Seq.filter (fun w -> w.Content = device) plate.Data in
    { plate with Data = data }


let AdjustGain(plate:Plate) =
    let ComputeGain(plate:Plate) =       
        let gain = Seq.filter(fun w -> w.DataType=DataTypes.Gain) plate.Data //filter only the "Gain" wells
        |> Seq.map(fun w -> w.Activity |> Map.toSeq)   //take the calculated activity values and represent as sequences
        |> Seq.fold(fun acc s -> Seq.append(acc) s) Seq.empty  //append all replicates
        |> Seq.groupBy(fun (s,_) -> s) //group by the signal type
        |> Seq.map(fun (s,x) ->  //compute the average gain of each channel
            let ave = Seq.map(fun (_,X) -> X) x |> Seq.average in            
            (s,ave))
        |> Seq.filter(fun ((s1,s2),_) -> not (s1="OD" || s2="OD")) //leave only fluorescent channels
        |> Map.ofSeq in
        {plate with Gain=gain}
    in
    let AdjustSignals(plate:Plate) = 
        let newData = Seq.map(fun w -> 
            if w.DataType = DataTypes.Data then 
                let activity = w.Activity |> Map.toSeq in
                let newActivity = Seq.map(fun (key,value) -> 
                                    let newValue = if plate.Gain.ContainsKey(key) then value/(plate.Gain.[key]) else value in
                                    (key,newValue)) activity 
                                  |> Map.ofSeq in
                {w with Activity=newActivity}
            else 
                w
            ) plate.Data in
        {plate with Data = newData;}
    in
    plate
    |> ComputeGain
    |> AdjustSignals




let Characterize(plate:Plate, growthModel) = 
    let def_cond =  plate.Conditions |> Map.toSeq |> Seq.head |> fst in
    let def_cond_vals = plate.Conditions |> Map.toSeq |> Seq.map(fun (c,_) -> c, 0.0) in   
    plate
    |> CorrectBlanks               //compute and subtract background signals from blank media
    |> FitGrowthModel growthModel  //fit the parameters of a growth curve to each werll 
    |> SetFilters                  //use the growth curve to select a range of data
    |> CorrectAutofluorescence     //compute and subtract the background fluorescence of nonfluorescent cells (autofluorescence)  
    |> FindSteadyStates            //compute the steady states of each reporter (individually)
    |> FindActivities              //compute the promoter activities from the fluorescence
    |> AdjustGain                  //compute the average gain adjustment and modify the signals accordingly
    |> ComputeProperties def_cond def_cond_vals         //collect a represenation of the different properties (activities, growht, etc) as a function of condition    
    
    
    

    


let getKeys map = Seq.map(fun (k,_) -> k) (Map.toSeq map)

let getProperties (plate:Plate, property:string) = 
    let id = Array.findIndex(fun x -> x = property) plate.PropertyNames in
    Seq.map2(fun x (y:double[][]) -> (x,y.[id])) plate.C plate.Properties



let DataOutput(plate:Plate) = 
    let result = "" in 
    
    let conditions = Seq.fold(fun acc w -> Seq.append(acc) (w.Condition |> Map.toSeq |> Seq.map(fun (s,_) -> s))) Seq.empty plate.Data |> Set.ofSeq in            
    let result = Seq.fold(fun acc c -> acc + c + ",") result conditions in
    let signals = (Seq.head plate.Data).SignalNames in
    let has_rfp = Array.contains "mRFP1" signals in
    let result = result 
               + "F(EYFP/OD),F(ECFP/OD)," 
               + (if has_rfp then "F(mRFP1/OD)," else "")
               + "F(EYFP/ECFP),"
               + (if has_rfp then "F(EYFP/mRFP1),F(ECFP/mRFP1)," else "")
               + "P(EYFP/OD),P(ECFP/OD),"
               + (if has_rfp then "P(mRFP1/OD)," else "")
               + "P(EYFP/ECFP),"
               + (if has_rfp then "P(EYFP/mRFP1),P(ECFP/mRFP1)," else "")
               + "mu,K,lag\n" in
    let result = Seq.fold(fun acc well ->             
                //let acc = acc + well.Row.ToString() + "," + well.Col.ToString() + "," + well.DataType.ToString() + "," in
                //let acc = acc + well.Condition.Aggregate("", (acc, y) => acc + y.Key + "=" + y.Value + ";") + "," in
                let acc = Seq.fold(fun acc c -> acc + (if well.Condition.ContainsKey(c) then well.Condition.[c].ToString() else "0.0") + ",") acc conditions in
                let acc = acc 
                    + (fst well.SteadyStates.[("EYFP", "OD")]).ToString() + ","
                    + (fst well.SteadyStates.[("ECFP", "OD")]).ToString() + "," 
                    + (if has_rfp then (fst well.SteadyStates.[("mRFP1", "OD")]).ToString() + "," else "")
                    + (fst well.SteadyStates.[("EYFP", "ECFP")]).ToString() + ","  
                    + (if has_rfp then (fst well.SteadyStates.[("EYFP", "mRFP1")]).ToString() + "," 
                    + (fst well.SteadyStates.[("ECFP", "mRFP1")]).ToString() + "," else "") in 

                let acc = acc 
                    + well.Activity.[("EYFP", "OD")].ToString() + "," 
                    + well.Activity.[("ECFP", "OD")].ToString() + "," 
                    + (if has_rfp then well.Activity.[("mRFP1", "OD")].ToString() + "," else "")
                    + well.Activity.[("EYFP", "ECFP")].ToString() + ","
                    + (if has_rfp then well.Activity.[("EYFP", "mRFP1")].ToString() + ","
                    + well.Activity.[("ECFP", "mRFP1")].ToString() + "," else "") in

                let acc = acc + well.GrowthModel.Value.mu.ToString() + "," in
                let acc = acc + well.GrowthModel.Value.K.ToString() + "," in
                let acc = acc + well.GrowthModel.Value.lag.ToString() + "\n" in
                acc
            ) result (Seq.filter( fun w -> w.DataType=DataTypes.Data) plate.Data) in
    result


//--------------------------------------------------------------------------
//The following block of code deals with different regulation models
//
//This is a VERY preliminary prototype! These procedures should be generalized
// in order to avoid code duplication
//--------------------------------------------------------------------------

let Hill(a,b,K,n) x =  (a*x**n + b*K**n)/(x**n+K**n)

let Hill_General(B)=    
    let fparam = (50000, 50000, 2000, 2000) in     
    //let fparam = (500000, 500000, 20000, 20000) in       
    let mparam = Seq.ofList[("a", 0.0,   5000.0, 5.0,  0);
                            ("b", 0.0,   200.0, 0.0,  0); 
                            ("K", 1e-15, 1e15,  1e-8, 1); 
                            ("n", 0.0,   5.0,  1.0,  0);                            
                            ("s", 1e-5,  10.0, 1.0,  1)] in      

// //use these settings for the growth model fits -> smaller ranges for a,b parameters
//    let mparam = Seq.ofList[("a", 0.0,   0.05, 5.0,  0);
//                            ("b", 0.0,   0.05, 0.0,  0); 
//                            ("K", 1e-15, 1e15,  1e-8, 1); 
//                            ("n", 0.0,   5.0,  1.0,  0);                            
//                            ("s", 1e-5,  10.0, 1.0,  1)] in      

    let ltotmax = ref Double.MinValue in
    let likefn param =  let (a,b,K,n,s) = Seq.nth 0 param,Seq.nth 1 param,Seq.nth 2 param,Seq.nth 3 param,Seq.nth 4 param in                        
                        let logLk = Seq.fold (fun s1 (conc,vals) ->
                                                            let obs = Hill(a,b,K,n) conc in 
                                                            Seq.fold(fun s2 y -> 
                                                                let noise = if (y<0.0) then 0.0 else Math.Sqrt(obs)*s in                                                                                                                                                    
                                                                let v =   ModelBase.normal_density(y,obs,noise) in       
                                                                let v = if v<=0.0 || Double.IsInfinity(v) then 0.0 else log(v) in
                                                                s2 + v) s1 vals) 0.0 B in
                        ltotmax:= if (!ltotmax<logLk) then logLk else !ltotmax;
                        logLk in
                            
    let FilzHill = Filzbach(mparam,likefn) in
    let param = FilzHill.Run(fparam) in    
    let ICs = FilzHill.calc_IC(!ltotmax) in
    let ParamHist = FilzHill.GetParamHist() in
    let res = ParamHist.Count in
    let RandFunc(id,v) = 
        let rnd = System.Random() in                
        let param = ParamHist.[id] in
        let (a,b,K,n) = param.[2], param.[3],param.[4],param.[5] in
        Hill(a,b,K,n) v
    in
    let paramDist = List.mapi(fun i (n,log) -> 
                                let dist = Seq.map(fun (y:float[]) -> y.[i+2]) ParamHist 
                                            |> Array.ofSeq in                                
                                (n,log,dist)) [("a",false);("b",false);("K",true);("n",false); ("s",true)] in
    let parameters = List.mapi(fun i (name,log,dist) -> {pname=name; value= Seq.nth i param; log=log; dist=dist;}) paramDist |> Array.ofList in 
    let (AIC,BIC,CIC) = ICs in
    let X = Seq.map(fun (x,_) -> x) B |> Array.ofSeq in
    {
        modelType =  ModelTypes.Linear;
        AIC = AIC;
        BIC = BIC;
        CIC = CIC;
        model = RandFunc;
        res = res;
        parameters = parameters;
        X = X;
        bayestable = ParamHist;
    }

let Hill_Activation(B)=    
    let fparam = (50000, 50000, 2000, 2000) in    
    let mparam = Seq.ofList[("a", 0.0,   200.0, 5.0,  0);                   
                            ("K", 1e-15, 1e15,  1e-8, 1); 
                            ("n", 0.0,   5.0,  1.0,  0);                            
                            ("s", 1e-5,  10.0, 1.0,  1)] in         
    let ltotmax = ref Double.MinValue in
    let likefn param =  
        let (a,K,n,s) = Seq.nth 0 param,Seq.nth 1 param,Seq.nth 2 param,Seq.nth 3 param in                        
            let logLk = Seq.fold (fun s1 (conc,vals) ->
                                        let obs = Hill(a,0.0,K,n) conc in
                                        Seq.fold(fun s2 y -> 
                                            let noise = if (y<0.0) then 0.0 else Math.Sqrt(obs)*s in                                                                                                                                                                                                                
                                            let v =   ModelBase.normal_density(y,obs,noise) in       
                                            let v = if v<=0.0 || Double.IsInfinity(v) then 0.0 else log(v) in
            s2 + v) s1 vals) 0.0 B in
        ltotmax:= if (!ltotmax<logLk) then logLk else !ltotmax;
        logLk 
    in
                            
    let FilzHill = Filzbach(mparam,likefn) in
    let param = FilzHill.Run(fparam) in    
    let ICs = FilzHill.calc_IC(!ltotmax) in
    let ParamHist = FilzHill.GetParamHist() in
    let res = ParamHist.Count in
    let RandFunc(id,v) = 
        let rnd = System.Random() in                
        let param = ParamHist.[id] in
        let (a,K,n) = param.[2], param.[3],param.[4] in
        Hill(a,0.0,K,n) v
    in
    let paramDist = List.mapi(fun i (n,log) -> 
                                let dist = Seq.map(fun (y:float[]) -> y.[i+2]) ParamHist 
                                            |> Array.ofSeq in                                
                                (n,log,dist)) [("a",false);("K",true);("n",false); ("s",true)] in
    let parameters = List.mapi(fun i (name,log,dist) -> {pname=name; value= Seq.nth i param; log=log; dist=dist;}) paramDist |> Array.ofList in 
    let (AIC,BIC,CIC) = ICs in
    let X = Seq.map(fun (x,_) -> x) B |> Array.ofSeq in
    {
        modelType =  ModelTypes.Linear;
        AIC = AIC;
        BIC = BIC;
        CIC = CIC;
        model = RandFunc;
        res = res;
        parameters = parameters;
        X = X;
        bayestable = ParamHist;
    }
    

let Hill_Repression(B)=    
    let fparam = (50000, 50000, 2000, 2000) in    
    let mparam = Seq.ofList[("b", 0.0,   200.0, 5.0,  0);                   
                            ("K", 1e-15, 1e15,  1e-8, 1); 
                            ("n", 0.0,   5.0,  1.0,  0);                            
                            ("s", 1e-5,  10.0, 1.0,  1)] in         
    let ltotmax = ref Double.MinValue in
    let likefn param =  
        let (b,K,n,s) = Seq.nth 0 param,Seq.nth 1 param,Seq.nth 2 param,Seq.nth 3 param in                        
            let logLk = Seq.fold (fun s1 (conc,vals) ->
                                            let obs = Hill(0.0,b,K,n) conc in 
                                            Seq.fold(fun s2 y -> 
                                                let noise = if (y<0.0) then 0.0 else Math.Sqrt(obs)*s in                                                                                                                                                                                                                
                                                let v =   ModelBase.normal_density(y,obs,noise) in       
                                                let v = if v<=0.0 || Double.IsInfinity(v) then 0.0 else log(v) in
            s2 + v) s1 vals) 0.0 B in
        ltotmax:= if (!ltotmax<logLk) then logLk else !ltotmax;
        logLk 
    in
                            
    let FilzHill = Filzbach(mparam,likefn) in
    let param = FilzHill.Run(fparam) in    
    let ICs = FilzHill.calc_IC(!ltotmax) in
    let ParamHist = FilzHill.GetParamHist() in
    let res = ParamHist.Count in
    let RandFunc(id,v) = 
        let rnd = System.Random() in                
        let param = ParamHist.[id] in
        let (b,K,n) = param.[2], param.[3],param.[4] in
        Hill(0.0,b,K,n) v
    in
    let paramDist = 
        List.mapi(fun i (n,log) -> 
            let dist = Seq.map(fun (y:float[]) -> y.[i+2]) ParamHist |> Array.ofSeq in                                
        (n,log,dist)) [("b",false);("K",true);("n",false); ("s",true)] 
    in
    let parameters = List.mapi(fun i (name,log,dist) -> {pname=name; value= Seq.nth i param; log=log; dist=dist;}) paramDist |> Array.ofList in 
    let (AIC,BIC,CIC) = ICs in
    let X = Seq.map(fun (x,_) -> x) B |> Array.ofSeq in
    {
        modelType =  ModelTypes.Linear;
        AIC = AIC;
        BIC = BIC;
        CIC = CIC;
        model = RandFunc;
        res = res;
        parameters = parameters;
        X = X;
        bayestable = ParamHist;
    }
    

let MM_General(B)=    
    let fparam = (50000, 50000, 2000, 2000) in    
    let mparam = Seq.ofList[("a", 0.0,   5000.0, 5.0,  0);
                            ("b", 0.0,   200.0, 0.0,  0); 
                            ("K", 1e-15, 1e15,  1e-8, 1);       
                            ("s", 1e-5,  10.0, 1.0,  1)] in          
    let ltotmax = ref Double.MinValue in
    let likefn param =  
        let (a,b,K,s) = Seq.nth 0 param,Seq.nth 1 param,Seq.nth 2 param,Seq.nth 3 param in                        
        let logLk = Seq.fold (fun s1 (conc,vals) ->
                                        let obs = Hill(a,b,K,1.0) conc in 
                                        Seq.fold(fun s2 y -> 
                                            let noise = if (y<0.0) then 0.0 else Math.Sqrt(obs)*s in                                                                                                                                                                                                                
                                            let v =   ModelBase.normal_density(y,obs,noise) in       
                                            let v = if v<=0.0 || Double.IsInfinity(v) then 0.0 else log(v) in
                                            s2 + v) s1 vals) 0.0 B in
        ltotmax:= if (!ltotmax<logLk) then logLk else !ltotmax;
        logLk 
    in
                            
    let FilzHill = Filzbach(mparam,likefn) in
    let param = FilzHill.Run(fparam) in    
    let ICs = FilzHill.calc_IC(!ltotmax) in
    let ParamHist = FilzHill.GetParamHist() in
    let res = ParamHist.Count in
    let RandFunc(id,v) = 
        let rnd = System.Random() in                
        let param = ParamHist.[id] in
        let (a,b,K) = param.[2], param.[3],param.[4] in
        Hill(a,b,K,1.0) v
    in
    let paramDist = List.mapi(fun i (n,log) -> 
                                let dist = Seq.map(fun (y:float[]) -> y.[i+2]) ParamHist 
                                            |> Array.ofSeq in                                
                                (n,log,dist)) [("a",false);("b",false);("K",true);("s",true)] in
    let parameters = List.mapi(fun i (name,log,dist) -> {pname=name; value= Seq.nth i param; log=log; dist=dist;}) paramDist |> Array.ofList in 
    let (AIC,BIC,CIC) = ICs in
    let X = Seq.map(fun (x,_) -> x) B |> Array.ofSeq in
    {
        modelType =  ModelTypes.Linear;
        AIC = AIC;
        BIC = BIC;
        CIC = CIC;
        model = RandFunc;
        res = res;
        parameters = parameters;
        X = X;
        bayestable = ParamHist;
    }

let MM_Repression(B)=    
    let fparam = (50000, 50000, 2000, 2000) in    
    let mparam = Seq.ofList[("b", 0.0,   200.0, 5.0,  0);                           
                            ("K", 1e-15, 1e15,  1e-8, 1);       
                            ("s", 1e-5,  10.0, 1.0,  1)] in          
    let ltotmax = ref Double.MinValue in
    let likefn param =  let (b,K,s) = Seq.nth 0 param,Seq.nth 1 param,Seq.nth 2 param in                        
                        let logLk = Seq.fold (fun s1 (conc,vals) ->
                                                            let obs = Hill(0.0,b,K,1.0) conc in 
                                                            Seq.fold(fun s2 y -> 
                                                                let noise = if (y<0.0) then 0.0 else Math.Sqrt(obs)*s in      
                                                                let v =   ModelBase.normal_density(y,obs,noise) in       
                                                                let v = if v<=0.0 || Double.IsInfinity(v) then 
                                                                            0.0 
                                                                        else 
                                                                            log(v) 
                                                                        in
                                                                s2 + v) s1 vals) 0.0 B in
                        ltotmax:= if (!ltotmax<logLk) then logLk else !ltotmax;
                        logLk in
                            
    let FilzHill = Filzbach(mparam,likefn) in
    let param = FilzHill.Run(fparam) in    
    let ICs = FilzHill.calc_IC(!ltotmax) in
    let ParamHist = FilzHill.GetParamHist() in
    let res = ParamHist.Count in
    let RandFunc(id,v) = 
        let rnd = System.Random() in                
        let param = ParamHist.[id] in
        let (b,K) = param.[2], param.[3] in
        Hill(0.0,b,K,1.0) v
    in
    let paramDist = List.mapi(fun i (n,log) -> 
                                let dist = Seq.map(fun (y:float[]) -> y.[i+2]) ParamHist 
                                            |> Array.ofSeq in                                
                                (n,log,dist)) [("b",false);("K",true);("s",true)] in
    let parameters = List.mapi(fun i (name,log,dist) -> {pname=name; value= Seq.nth i param; log=log; dist=dist;}) paramDist |> Array.ofList in 
    let (AIC,BIC,CIC) = ICs in
    let X = Seq.map(fun (x,_) -> x) B |> Array.ofSeq in
    {
        modelType =  ModelTypes.Linear;
        AIC = AIC;
        BIC = BIC;
        CIC = CIC;
        model = RandFunc;
        res = res;
        parameters = parameters;
        X = X;
        bayestable = ParamHist;
    }


let MM_Activation(B)=    
    let fparam = (50000, 50000, 2000, 2000) in    
    let mparam = Seq.ofList[("a", 0.0,   200.0, 5.0,  0);                           
                            ("K", 1e-15, 1e15,  1e-8, 1);       
                            ("s", 1e-5,  10.0, 1.0,  1)] in          
    let ltotmax = ref Double.MinValue in
    let likefn param =  let (a,K,s) = Seq.nth 0 param,Seq.nth 1 param,Seq.nth 2 param in                        
                        let logLk = Seq.fold (fun s1 (conc,vals) ->
                                                            let obs = Hill(a,0.0,K,1.0) conc in 
                                                            Seq.fold(fun s2 y -> 
                                                                let noise = if (y<0.0) then 0.0 else Math.Sqrt(obs)*s in      
                                                                let v =   ModelBase.normal_density(y,obs,noise) in       
                                                                let v = if v<=0.0 || Double.IsInfinity(v) then 0.0 else log(v) in                                                            
                                                                s2 + v) s1 vals) 0.0 B in
                        ltotmax:= if (!ltotmax<logLk) then logLk else !ltotmax;
                        logLk in
                            
    let FilzHill = Filzbach(mparam,likefn) in
    let param = FilzHill.Run(fparam) in    
    let ICs = FilzHill.calc_IC(!ltotmax) in
    let ParamHist = FilzHill.GetParamHist() in
    let res = ParamHist.Count in
    let RandFunc(id,v) = 
        let rnd = System.Random() in                
        let param = ParamHist.[id] in
        let (a,K) = param.[2], param.[3] in
        Hill(a,0.0,K,1.0) v
    in
    let paramDist = List.mapi(fun i (n,log) -> 
                                let dist = Seq.map(fun (y:float[]) -> y.[i+2]) ParamHist 
                                            |> Array.ofSeq in                                
                                (n,log,dist)) [("a",false);("K",true);("s",true)] in
    let parameters = List.mapi(fun i (name,log,dist) -> {pname=name; value= Seq.nth i param; log=log; dist=dist;}) paramDist |> Array.ofList in 
    let (AIC,BIC,CIC) = ICs in
    let X = Seq.map(fun (x,_) -> x) B |> Array.ofSeq in
    {
        modelType =  ModelTypes.Linear;
        AIC = AIC;
        BIC = BIC;
        CIC = CIC;
        model = RandFunc;
        res = res;
        parameters = parameters;
        X = X;
        bayestable = ParamHist;
    }



let Dimer_General(B)=    
    let fparam = (50000, 50000, 2000, 2000) in    
    let mparam = Seq.ofList[("a", 0.0,   200.0, 5.0,  0);
                            ("b", 0.0,   200.0, 0.0,  0); 
                            ("K", 1e-15, 1e15,  1e-8, 1);       
                            ("s", 1e-5,  10.0, 1.0,  1)] in          
    let ltotmax = ref Double.MinValue in
    let likefn param =  let (a,b,K,s) = Seq.nth 0 param,Seq.nth 1 param,Seq.nth 2 param,Seq.nth 3 param in                        
                        let logLk = Seq.fold (fun s1 (conc,vals) ->
                                                            let obs = Hill(a,b,K,2.0) conc in 
                                                            Seq.fold(fun s2 y -> 
                                                                let noise = if (y<0.0) then 0.0 else Math.Sqrt(obs)*s in                                                                                                                                                                                                                
                                                                let v =   ModelBase.normal_density(y,obs,noise) in       
                                                                let v = if v<=0.0 || Double.IsInfinity(v) then 0.0 else log(v) in
                                                                s2 + v) s1 vals) 0.0 B in
                        ltotmax:= if (!ltotmax<logLk) then logLk else !ltotmax;
                        logLk in
                            
    let FilzHill = Filzbach(mparam,likefn) in
    let param = FilzHill.Run(fparam) in    
    let ICs = FilzHill.calc_IC(!ltotmax) in
    let ParamHist = FilzHill.GetParamHist() in
    let res = ParamHist.Count in
    let RandFunc(id,v) = 
        let rnd = System.Random() in                
        let param = ParamHist.[id] in
        let (a,b,K) = param.[2], param.[3],param.[4] in
        Hill(a,b,K,2.0) v
    in
    let paramDist = List.mapi(fun i (n,log) -> 
                                let dist = Seq.map(fun (y:float[]) -> y.[i+2]) ParamHist 
                                            |> Array.ofSeq in                                
                                (n,log,dist)) [("a",false);("b",false);("K",true);("s",true)] in
    let parameters = List.mapi(fun i (name,log,dist) -> {pname=name; value= Seq.nth i param; log=log; dist=dist;}) paramDist |> Array.ofList in 
    let (AIC,BIC,CIC) = ICs in
    let X = Seq.map(fun (x,_) -> x) B |> Array.ofSeq in
    {
        modelType =  ModelTypes.Linear;
        AIC = AIC;
        BIC = BIC;
        CIC = CIC;
        model = RandFunc;
        res = res;
        parameters = parameters;
        X = X;
        bayestable = ParamHist;
    }


let Dimer_Repression(B)=    
    let fparam = (50000, 50000, 2000, 2000) in    
    let mparam = Seq.ofList[("b", 0.0,   200.0, 5.0,  0);                           
                            ("K", 1e-15, 1e15,  1e-8, 1);       
                            ("s", 1e-5,  10.0, 1.0,  1)] in          
    let ltotmax = ref Double.MinValue in
    let likefn param =  let (b,K,s) = Seq.nth 0 param,Seq.nth 1 param,Seq.nth 2 param in                        
                        let logLk = Seq.fold (fun s1 (conc,vals) ->
                                                            let obs = Hill(0.0,b,K,2.0) conc in 
                                                            Seq.fold(fun s2 y -> 
                                                                let noise = if (y<0.0) then 0.0 else Math.Sqrt(obs)*s in      
                                                                let v =   ModelBase.normal_density(y,obs,noise) in       
                                                                let v = if v<=0.0 || Double.IsInfinity(v) then 0.0 else log(v) in
                                                                s2 + v) s1 vals) 0.0 B in
                        ltotmax:= if (!ltotmax<logLk) then logLk else !ltotmax;
                        logLk in
                            
    let FilzHill = Filzbach(mparam,likefn) in
    let param = FilzHill.Run(fparam) in    
    let ICs = FilzHill.calc_IC(!ltotmax) in
    let ParamHist = FilzHill.GetParamHist() in
    let res = ParamHist.Count in
    let RandFunc(id,v) = 
        let rnd = System.Random() in                
        let param = ParamHist.[id] in
        let (b,K) = param.[2], param.[3] in
        Hill(0.0,b,K,2.0) v
    in
    let paramDist = List.mapi(fun i (n,log) -> 
                                let dist = Seq.map(fun (y:float[]) -> y.[i+2]) ParamHist 
                                            |> Array.ofSeq in                                
                                (n,log,dist)) [("b",false);("K",true);("s",true)] in
    let parameters = List.mapi(fun i (name,log,dist) -> {pname=name; value= Seq.nth i param; log=log; dist=dist;}) paramDist |> Array.ofList in 
    let (AIC,BIC,CIC) = ICs in
    let X = Seq.map(fun (x,_) -> x) B |> Array.ofSeq in
    {
        modelType =  ModelTypes.Linear;
        AIC = AIC;
        BIC = BIC;
        CIC = CIC;
        model = RandFunc;
        res = res;
        parameters = parameters;
        X = X;
        bayestable = ParamHist;
    }

let Dimer_Activation(B)=    
    let fparam = (50000, 50000, 2000, 2000) in        
    let mparam = Seq.ofList[("a", 0.0,   200.0, 5.0,  0);                           
                            ("K", 1e-15, 1e15,  1e-8, 1);       
                            ("s", 1e-5,  10.0, 1.0,  1)] in          
    let ltotmax = ref Double.MinValue in
    let likefn param =  let (a,K,s) = Seq.nth 0 param,Seq.nth 1 param,Seq.nth 2 param in                        
                        let logLk = Seq.fold (fun s1 (conc,vals) ->
                                                            let obs = Hill(a,0.0,K,2.0) conc in 
                                                            Seq.fold(fun s2 y -> 
                                                                let noise = if (y<0.0) then 0.0 else Math.Sqrt(obs)*s in      
                                                                let v =   ModelBase.normal_density(y,obs,noise) in       
                                                                let v = if v<=0.0 || Double.IsInfinity(v) then 0.0 else log(v) in
                                                                s2 + v) s1 vals) 0.0 B in
                        ltotmax:= if (!ltotmax<logLk) then logLk else !ltotmax;
                        logLk in
                            
    let FilzHill = Filzbach(mparam,likefn) in
    let param = FilzHill.Run(fparam) in    
    let ICs = FilzHill.calc_IC(!ltotmax) in
    let ParamHist = FilzHill.GetParamHist() in
    let res = ParamHist.Count in
    let RandFunc(id,v) = 
        let rnd = System.Random() in                
        let param = ParamHist.[id] in
        let (a,K) = param.[2], param.[3] in
        Hill(a,0.0,K,2.0) v
    in
    let paramDist = List.mapi(fun i (n,log) -> 
                                let dist = Seq.map(fun (y:float[]) -> y.[i+2]) ParamHist 
                                            |> Array.ofSeq in                                
                                (n,log,dist)) [("a",false);("K",true);("s",true)] in
    let parameters = List.mapi(fun i (name,log,dist) -> {pname=name; value= Seq.nth i param; log=log; dist=dist;}) paramDist |> Array.ofList in 
    let (AIC,BIC,CIC) = ICs in
    let X = Seq.map(fun (x,_) -> x) B |> Array.ofSeq in
    {
        modelType =  ModelTypes.Linear;
        AIC = AIC;
        BIC = BIC;
        CIC = CIC;
        model = RandFunc;
        res = res;
        parameters = parameters;
        X = X;
        bayestable = ParamHist;
    }

let Sequential(B)=    
    let fparam = (100000, 100000, 2000, 2000) in    
    let mparam = Seq.ofList[("a", 0.0,   200.0, 5.0,  0);
                            ("b", 0.0,   200.0, 0.0,  0); 
                            ("c", 0.0,   200.0, 0.0,  0); 
                            ("K1", 1e-15, 1e15,  1e-8, 1); 
                            ("K2", 1e-15, 1e15,  1e-8, 1);                             
                            ("s", 1e-5,  10.0, 1.0,  1)] in      
    let ltotmax = ref Double.MinValue in
    let SeqMech(a,b,c,K1,K2) conc = (a * conc ** 2.0 + b * conc * K1 + c * K2 ** 2.0)/(conc**2.0 + conc*K1 + K2**2.0) in
    let likefn param =  let (a,b,c,K1,K2,s) = Seq.nth 0 param,Seq.nth 1 param,Seq.nth 2 param,Seq.nth 3 param,Seq.nth 4 param,Seq.nth 5 param in                        
                        let logLk = Seq.fold (fun s1 (conc,vals) ->
                                                            let obs = SeqMech(a,b,c,K1,K2) conc in
                                                            Seq.fold(fun s2 y -> 
                                                                let noise = if (y<0.0) then 0.0 else Math.Sqrt(obs)*s in                                                                                                                                                    
                                                                let v =   ModelBase.normal_density(y,obs,noise) in       
                                                                let v = if v<=0.0 || Double.IsInfinity(v) then 0.0 else log(v) in
                                                                s2 + v) s1 vals) 0.0 B in
                        ltotmax:= if (!ltotmax<logLk) then logLk else !ltotmax;
                        logLk in
                            
    let FilzHill = Filzbach(mparam,likefn) in
    let param = FilzHill.Run(fparam) in    
    let ICs = FilzHill.calc_IC(!ltotmax) in
    let ParamHist = FilzHill.GetParamHist() in
    let res = ParamHist.Count in
    let RandFunc(id,v) = 
        let rnd = System.Random() in                
        let param = ParamHist.[id] in
        let (a,b,c,K1,K2) = param.[2], param.[3],param.[4],param.[5],param.[6] in
        SeqMech(a,b,c,K1,K2) v
    in
    let paramDist = List.mapi(fun i (n,log) -> 
                                let dist = Seq.map(fun (y:float[]) -> y.[i+2]) ParamHist 
                                            |> Array.ofSeq in                                
                                (n,log,dist)) [("a",false);("b",false);("c",false);("K1",true);("K2",true); ("s",true)] in
    let parameters = List.mapi(fun i (name,log,dist) -> {pname=name; value= Seq.nth i param; log=log; dist=dist;}) paramDist |> Array.ofList in 
    let (AIC,BIC,CIC) = ICs in
    let X = Seq.map(fun (x,_) -> x) B |> Array.ofSeq in
    {
        modelType =  ModelTypes.Linear;
        AIC = AIC;
        BIC = BIC;
        CIC = CIC;
        model = RandFunc;
        res = res;
        parameters = parameters;
        X = X;
        bayestable = ParamHist;
    }
    
    
let Linear(B)=    
    let fparam = (50000, 50000, 2000, 2000) in    
    let mparam = Seq.ofList[("a",1e-10,1e5, 0.001,  1);                                    
                            ("b", 1e-5,  1e5, 0.001,  1);                                    
                            ("s", 1e-5, 1e10, 1.0,  1)] in         
    let ltotmax = ref Double.MinValue in
    let likefn param =  let (a,b,s) = Seq.nth 0 param, Seq.nth 1 param, Seq.nth 2 param in                        
                        let logLk = Seq.fold (fun s1 (conc,vals) ->
                                                            let obs = a*conc+b in 
                                                            Seq.fold(fun s2 y -> 
                                                                let noise = if (y<0.0) then 0.0 else Math.Sqrt(obs)*s in         
                                                                let v =   ModelBase.normal_density(y,obs,noise) in       
                                                                let v = if v<=0.0 || Double.IsInfinity(v) then 0.0 else log(v) in
                                                                s2 + v) s1 vals) 0.0 B in
                        ltotmax:= if (!ltotmax<logLk) then logLk else !ltotmax;
                        logLk in
                            
    let FilzHill = Filzbach(mparam,likefn) in
    let param = FilzHill.Run(fparam) in    
    let ICs = FilzHill.calc_IC(!ltotmax) in
    let ParamHist = FilzHill.GetParamHist() in
    let res = ParamHist.Count in
    let RandFunc(id,v) = 
        let rnd = System.Random() in                
        let param = ParamHist.[id] in
        let (a,b) = param.[2], param.[3] in
        a*v + b
    in    
    let paramDist = List.mapi(fun i (n,log) -> 
                                let dist = Seq.map(fun (y:float[]) -> y.[i+2]) ParamHist 
                                            |> Array.ofSeq in                                
                                (n,log,dist)) [("a",true);("b",true);("s",true)] in
    let parameters = List.mapi(fun i (name,log,dist) -> {pname=name; value= Seq.nth i param; log=log; dist=dist;}) paramDist |> Array.ofList in 
    let (AIC,BIC,CIC) = ICs in
    let X = Seq.map(fun (x,_) -> x) B |> Array.ofSeq in
    {
        modelType =  ModelTypes.Linear;
        AIC = AIC;
        BIC = BIC;
        CIC = CIC;
        model = RandFunc;
        res = res;
        parameters = parameters;
        X = X;
        bayestable = ParamHist;
    }

//note: currently, this function supports only a single inducible/repressible promoter with a single regulator
let CharacterizeModel(data:seq<double*double[]>, modelType)=        
    match modelType with 
        | ModelTypes.Hill_General -> Hill_General(data)
        | ModelTypes.Hill_Activation -> Hill_Activation(data)
        | ModelTypes.Hill_Repression -> Hill_Repression(data)
        | ModelTypes.Linear -> Linear(data)
        | ModelTypes.Michaelis_Menten_General -> MM_General(data)
        | ModelTypes.Michaelis_Menten_Activation -> MM_Activation(data)
        | ModelTypes.Michaelis_Menten_Repression -> MM_Repression(data)
        | ModelTypes.Dimer_General -> Dimer_General(data)
        | ModelTypes.Dimer_Activation -> Dimer_Activation(data)
        | ModelTypes.Dimer_Repression -> Dimer_Repression(data)
        | ModelTypes.Sequential_Binding -> Sequential(data)       
        | ModelTypes.Constitutive -> 
                { 
                    modelType = ModelTypes.Constitutive;
                    AIC = 0.0;
                    BIC = 0.0;
                    CIC = 0.0;
                    model = (fun (id,x) -> 0.0);
                    res = 0;
                    parameters = [||];
                    X = Seq.map(fun (x,_) -> x) data |> Array.ofSeq;
                    bayestable = new List<double[]>();
                }       
