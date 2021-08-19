namespace Microsoft.Research.CRNEngine
[<JavaScript>]
type Point = 
  { mean:float; stdev:float }
  static member create (mean:float) (stdev:float) = { mean = mean; stdev = stdev}
  static member interpolate (t:float) (t0:float) (t1:float) (x0:Point) (x1:Point) = {
    mean = x0.mean + (x1.mean-x0.mean)*(t-t0)/(t1-t0);
    stdev = x0.stdev + (x1.stdev-x0.stdev)*(t-t0)/(t1-t0); //TODO: is this correct?
  }

