[<JavaScript>]
module Microsoft.Research.CRNEngine.Rng

open Microsoft.Research.Filzbach.Lib

//Adapts costom PRNG (LCG for now) to System.Random like interface
//So you can use Rng.Random instead of System.Random across other components

// FP: due to concerns about performance and reproducibility when used with the new SSA trajectories feature, the RNG is no longer thread-safe. A separate RNG instance should be used for each thread.

type public Random(seed:int) =
    let rng = ref (LCGRng(uint32 seed) :> IRng)
#if DEBUG && !JavaScript
    let tid = System.Threading.Thread.CurrentThread.ManagedThreadId
#endif

    new() =
        let seed =  abs (int System.DateTime.UtcNow.Ticks)
        Random(seed)

    with
        member __.Next() =
#if DEBUG && !JavaScript
            if System.Threading.Thread.CurrentThread.ManagedThreadId <> tid then failwith "RNG is not thread-safe" else
#endif
            let next,newRng = (!rng).NextInt32()
            rng := newRng
            int next
        member __.Next(max: int) =
#if DEBUG && !JavaScript
            if System.Threading.Thread.CurrentThread.ManagedThreadId <> tid then failwith "RNG is not thread-safe" else
#endif
            let next,newRng = (!rng).NextDouble()
            rng := newRng                
            let nextCoerced = floor(next*(float max))
            int nextCoerced
        member __.NextConstrained(max: int) = //alias to usу in JavaScript
            __.Next(max)
        member __.NextDouble() =
#if DEBUG && !JavaScript
            if System.Threading.Thread.CurrentThread.ManagedThreadId <> tid then failwith "RNG is not thread-safe" else
#endif
            let next,newRng = (!rng).NextDouble()
            rng := newRng                
            next


let emitTestSequence() =
    let seed = 123
    let count = 10
    let max = 10000

    let rng = new Random(seed)
    Array.init count (fun i -> rng.Next(max))