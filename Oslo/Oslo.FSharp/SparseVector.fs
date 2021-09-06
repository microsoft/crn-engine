// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Oslo.SparseVector
open System
open Microsoft.FSharp.Core

[<Literal>]
let IncrementSize : int = 16 // Size of chunk for array increments
    
type SparseVector(items : double[], indices : int[], n : int) = 
            
    let n : int = n //number of elements
    let mutable items : double[] = if items = null then failwith "argument is null" else items//The nonzero elements of the sparse vector
    let mutable indices : int[] = if indices = null then failwith "argument is null" else indices  //The indices of the nonzero elements in the sparse vector
    let mutable count : int = if (Array.sumBy (fun x->x*x) items) = 0.0 then 0 else Array.length items //Number of initialized elements
           
    new(n : int) = SparseVector(Array.zeroCreate IncrementSize, Array.zeroCreate IncrementSize, n)

    member this.Items with get() = items
    member this.Indices with get() = indices
    member this.Count with get() = count
    member this.Length with get() = n    
    member this.Item 
      with get(i : int)  = 
        if (i < 0 || i >= n) 
        then failwith "index is out of range"
        else 
          let idx : int = 
            match Array.tryFindIndex (fun x -> x = i) indices.[0..count - 1] with
            | Some x -> x
            | None -> 
              if Array.forall (fun x -> x < i) (indices.[0..(count - 1)]) 
              then ~~~(count) 
              else 
                match (Array.tryFindIndex (fun x -> x > i) (indices.[0..(count - 1)])) with 
                | Some x -> ~~~x 
                | None -> -1
          if idx < 0 then 0.0 else items.[idx]
                            
      and set i value =
        if (i < 0 || i >= n)
        then failwith "index is out of range"
        else
          let idx : int = 
            match Array.tryFindIndex (fun x -> x = i) indices.[0..count - 1] with
            | Some x -> x
            | None -> 
              if Array.forall (fun x -> x < i) (indices.[0..(count - 1)]) 
              then ~~~(count) 
              else 
                match (Array.tryFindIndex (fun x -> x > i) (indices.[0..(count - 1)])) with 
                | Some x -> ~~~x 
                | None -> -1
          if (idx >= 0)
          then Array.set items idx value
          else
            let indexToAdd = ~~~idx
            if (count >= items.Length)
            then
              let delta = Math.Min(IncrementSize, n - items.Length)
              let newIndices : int[] = Array.zeroCreate ((Array.length indices) + delta)
              let newItems : double[] = Array.zeroCreate ((Array.length items) + delta)
              for k in 0..(Array.length indices) - 1 do Array.set newIndices k indices.[k]
              for k in 0..(Array.length items) - 1 do Array.set newItems k items.[k]
                                      
              items <- newItems
              indices <- newIndices                 
                                                  
              for k in 0..count - indexToAdd - 1 do
                indices.[count - k] <- indices.[count - k - 1]
                items.[count - k] <- items.[count - k - 1]
                                                                 
              count <- count + 1
              Array.set indices indexToAdd i
              Array.set items indexToAdd value
            
    member this.binary f (B:SparseVector) =
      //let A = this
      if not (this.Length = B.Length) then failwith "Minus requires two vectors of the same length"
      match (this.Count,B.Count) with
      | (0,_) -> SparseVector(Array.map (f 0.0) B.Items, B.Indices, B.Length)
      | (_,0) -> this
      | _ ->
        let tail (V:SparseVector) cV acc =
          let indices, items = acc |> List.rev |> Array.ofList |> Array.unzip
          SparseVector
            ( Array.append items V.Items.[cV..V.Count-1]
            , Array.append indices V.Indices.[cV..V.Count-1]
            , V.Length )                
        let rec loop cA cB acc =
          let iA = this.Indices.[cA]
          let iB = B.Indices.[cB]
          match iA - iB with
          | n when n < 0 ->
              let new_acc = (iA, f this.Items.[iA] 0.0)::acc
              if cA < this.Count-1 then
                loop (cA+1) (cB) new_acc
              else tail B cB new_acc
          | n when n > 0 ->
              let new_acc = (iB, f 0.0 B.Items.[iB])::acc
              if cB < B.Count-1 then
                loop (cA) (cB+1) new_acc
              else tail this cA new_acc
          | 0 ->
              //let A_inner = A
              //I changed this from  let new_acc = (iA, f A.Items.[iA] B.Items.[iB])::acc
              let new_acc = (iA, f this.Items.[cA] B.Items.[cB])::acc
              if cA < this.Count-1 && cB < B.Count-1 then
                loop (cA+1) (cB+1) new_acc
              elif cA < this.Count-1 then tail this (cA+1) new_acc
              else tail B (cB+1) new_acc

        loop 0 0 []                                         
                                            
    member this.plus (B:SparseVector) = 
        this.binary (+) B

    member this.minus (B:SparseVector) = 
        this.binary (-) B
