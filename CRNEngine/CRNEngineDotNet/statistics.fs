// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Microsoft.Research.CRNEngine.Statistics

let rec sample_standardnormal (rng:Rng.Random) =   
  let u = 2.0 * rng.NextDouble() - 1.0
  let v = 2.0 * rng.NextDouble() - 1.0
  let w = u*u + v*v
  if w < 1.0 
  then u * sqrt((-2.0 * log(w)) / w)
  else sample_standardnormal rng

// Define numerically improved log of normal density, replacing need to take logs of extremely small numbers
let log_of_normal_density (x,m,s) = 
  let sqrt2pi = sqrt(2.0*System.Math.PI) in
  let h1 = ((x-m)/s) in
  let p1 = -0.5 * h1 * h1 in
  let p2 = s * sqrt2pi in
  p1 - log(p2)

/// Welford's online algorithm
let online_meanvar mean variance new_value new_n = 
  let new_mean = mean + (new_value-mean)/(float new_n)
  let new_var = variance + ((new_value-mean)*(new_value-new_mean) - variance)/(float new_n)
  new_mean, new_var

/// Pearson correlation matrix for arrays of float values
let correlation_pearson (samples:float[][]) = 
  let num_vars = Array.length samples
  let num_samples = Array.length samples.[0] |> float
  // Pre-compute the mean and standard deviations
  let means, deviations = 
    samples |> Array.map (fun xs -> 
      let xbar = Array.average xs
      let dev = xs |> Array.map (fun x -> (x-xbar)*(x-xbar)) |> Array.sum |> sqrt
      xbar, dev
    )
    |> Array.unzip
  // Compute product-sum and use pre-computed statistics
  let mutable A = Array.init num_vars (fun i -> Array.zeroCreate num_vars)
  for i in 0..num_vars-1 do
    A.[i].[i] <- 1.0
    for j in (i+1)..(num_vars-1) do
      let prod_sum = Array.fold2 (fun acc si sj -> acc + si * sj) 0.0 samples.[i] samples.[j]
      A.[i].[j] <- (prod_sum - num_samples*means.[i]*means.[j]) / deviations.[i] / deviations.[j]
    // Copy the lower triangle from the upper triangle, which have already been computed
    for j in 0..i-1 do
      A.[i].[j] <- A.[j].[i]
  A

/// Pooled variance (assuming biased variance definitions)
let pooled_variance (distributions:(float*float*float) list) = 
  distributions
  |> List.reduce (fun (v1,m1,n1) (v2,m2,n2) ->
    let n0 = n1+n2
    let mpool = (n1*m1 + n2*m2) / n0
    let vpool = ( n1*(v1+m1*m1) + n2*(v2+m2*m2) ) / n0 - mpool*mpool
    vpool, mpool, n0
  )