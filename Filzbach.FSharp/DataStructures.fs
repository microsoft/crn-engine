namespace Microsoft.Research.Filzbach

[<JavaScript>]
module DataStructures =

    type NamesMap(names:string array) = //array based implementation shows high performance in case of small amount of parameters. Can be changed to map based implementation if needed
        member s.Item(name) =
            let len = names.Length //as WebSharper fails to translate array.indexOf and Seq.findIndex. finding index explicitly
            let rec testArrayTail index =
                if names.[index]=name then index
                else if index=len-1 then -1
                else testArrayTail (index+1)
            testArrayTail 0

        member s.Keys =
            names

    type NamedObject<'a>(name:string,data:'a) = //as WebSharper fails translating array of tuples we use type with 2 fields
        member s.Data with get() = data
        member s.Name with get() = name
    
    /// An AssociateArray is basically an Ordered Map type
    type AssociativeArray<'a>(map:NamesMap,objects:'a array) = //as WebSharper translates multiple constructors in the code that does not work
        member s.CopyWithNewValues(objects)=
            AssociativeArray(map,objects)

        member s.Item(idx:int) = //access by internal order number
            objects.[idx]
        
        member s.Item(name:string) = //access by name for scalar parameters
            objects.[map.[name]]

        member s.Names
            with get() = map.Keys
        
        member s.ToArray() = Array.ofSeq objects 
        member s.ToMap() = Array.zip s.Names (s.ToArray()) |> Map.ofArray

        member internal s._dataArray =
            objects

        member internal s._namesMap =
            map

        static member public ofArray<'a>(namedRanges:NamedObject<'a> array) =
            let len = namedRanges.Length
            let objects = Array.init len (fun i -> namedRanges.[i].Data)
            let names = Array.init len (fun i -> namedRanges.[i].Name)
            AssociativeArray<'a>(NamesMap(names),objects)

        static member public ofSeq<'a>(namedRanges:NamedObject<'a> seq) =
            namedRanges |> Array.ofSeq |> AssociativeArray.ofArray

    type BayesTable(data:float [] [],columnNames:NamesMap) =
        member s.GetColumn name =
            data.[columnNames.[name]]
        member s.ColumnCount =
            data.Length
        member s.ColumnNames =
            columnNames.Keys
        member internal s._namesMap = columnNames