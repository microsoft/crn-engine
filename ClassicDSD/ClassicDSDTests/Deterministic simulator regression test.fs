module Microsoft.Research.DNA.``Deterministic simulator regression test``

//open System.IO
////open System.Xml
////open System.Xml.XPath
//
//open FsUnit
//open FsUnit.Xunit
//open Xunit
//
//open TestUtils
//
//open Microsoft.Research.DNA
//open Microsoft.Research.CRNEngine
//
//   
//let split (seqin : seq<'a>) =
//    let enum = seqin.GetEnumerator()
//    enum.MoveNext() |> ignore
//    let head = enum.Current
//
//    let seqTail = seq {
//        while enum.MoveNext() do
//            yield enum.Current
//    }
//
//    (head, seqTail)
//
//let read_example resourceName =
//    let csvLines = seq {
//        use stream = System.Reflection.Assembly.GetExecutingAssembly().GetManifestResourceStream(resourceName)
//        use reader = new System.IO.StreamReader(stream)
//
//        while not reader.EndOfStream do
//            yield reader.ReadLine()
//    }
//
//    let headers, rawdata = split csvLines
//
//
//    let data = rawdata |> Seq.map (fun s ->
//                                    s.Split ','
//                                    |> Array.map (fun v -> float v))
//
//
//    (headers, data)
//
//let pointstore_outputter ps (sweep_key: string) =
//  let store =
//    match Hashtable.tryFind ps sweep_key with
//    | None ->
//      let s = ref [] in
//      Hashtable.add ps sweep_key s;
//      s
//    | Some s ->
//(*      System.Console.WriteLine("Warning: Pointstore already exists with key: " ^ sweep_key);*)
//      s in  
//  fun time (_:'species Settings.settings) (_:string) (outputs:'species Output.t list) ->
//    let mean = List.map (fun o -> match o with
//                                  | Output.NonSpatial(_,v) -> v
//                                  | Output.NonSpatialWithStd(_,v,_) -> v
//                                  | Output.Spatial1D (v, _) -> v
//                                  | Output.Spatial1DMultiPlot (_,v, _) -> v
//                                  | Output.Spatial2D (v, _, _) -> v
//                                  | Output.Spatial2DMultiPlot (_,v, _, _) -> v
//                                  //| _ -> failwith "This output is not supported in pointstore outputter"
//                                  ) outputs in
//    let variance = List.choose (fun o -> match o with
//                                  | Output.NonSpatialWithStd(_,_,v) -> Some v
//                                  | _ -> None) outputs in
//    let data = mean @ variance in
//    store := (time::data)::!store
//
//
//let simulationTest mode algo isStiff text resultsFileName expectedHeader =
//   
//    let t = Dsd.parse text  |> Dsd.convert_expand  
//    let t' = {t with settings = 
//                      {t.settings with simulator  = algo;
//                                       simulation = {t.settings.simulation with kinetics = mode}
//                                       deterministic = {t.settings.deterministic with reltolerance = 0.00001;
//                                                                                      stiff = isStiff}}}
//             
//    
//    let pointstores = Hashtable.empty ()
//    let output_pointstore = Simulator.pointstore_outputter pointstores
//    let simulation_output sweep_key = fun time settings env dataList ->
//        output_pointstore sweep_key time settings env dataList
//    
//    let eat_species _ _ _ _ _ = ()
//
//    ignore(Gui.simulate simulation_output eat_species (ref false) g);
//
//    let pointstore = Hashtable.find pointstores Simulator.no_sweep_key in
//    let pointstore = !pointstore |> List.rev |> List.toArray 
//
//    let headers, exampleData = read_example resultsFileName
//    let exampleArray = exampleData |> Seq.toArray
//
//    headers |> should equal expectedHeader
//
//    let findTargetPair sourceVal (target : float list [])  =
//
//        let targetIndex = Array.tryFindIndex
//                            (fun x -> List.head x > sourceVal)
//                            target
//
//        match targetIndex with
//            | None -> None
//            | Some i -> Some(i - 1, i)
//
//    let generalComparison (expected : float [][]) (calculated : float list [])  =
//
//        //Starts and ends are bit awkward
//        for i in {10..(expected.Length-5)} do
//
//            let innerexpected = expected.[i]
//            let sampleValue = innerexpected.[0]
//
//            match findTargetPair sampleValue calculated with
//                | None -> ()
//                | Some(s1, s2) ->
//                    let v1 = calculated.[s1].[0]
//                    let v2 = calculated.[s2].[0]
//
//                    let samplePoint = (v2 - sampleValue) / (v2 - v1)
//
//                    for j in {1..(innerexpected.Length-1)} do
//                        let sampledValue = calculated.[s1].[j] * samplePoint + calculated.[s2].[j] * (1.0 - samplePoint)
//                        Assert.Equal(innerexpected.[j], sampledValue, 2)
//
//    //We throw away the first point as the point store weirdly has an empty list. CDG
//    generalComparison exampleArray pointstore
//    //generalComparison exampleArray pointstore
//
//[<Trait("Category", "CRN")>]
//[<Fact>]
//let ``Non-stiff deterministic simulation: AND Circuit``() =
//
//    let text = programText "AND Circuit"
//
//    let expectedHeader = "Time,<1^ 2>,<3 4^>,<2 3>"
//    let isStiff = false
//    let sett    = Simulation_settings.Deterministic
//    let algo    = Crn_settings.Oslo
//
//    simulationTest sett algo isStiff text "deterministicANDCircuit.csv" expectedHeader
//
//[<Trait("Category", "CRN")>]
//[<Fact>]
//let ``Stiff deterministic simulation: Ultrasensitive``() =
//
//    let text = programText "Ultrasensitive"
//
//    let expectedHeader = "Time,sum(<a.2 tl1p^ l1p>),sum(<b.8 tl2p^ l2p>),sum(<b.14 tl3p^ l3p>)"
//    let isStiff = true
//    let sett    = Simulation_settings.Deterministic
//    let algo    = Crn_settings.Oslo
//
//    simulationTest sett algo isStiff text  "deterministicUltrasensitive.csv" expectedHeader