namespace Microsoft.Research.CRNEngine
[<JavaScript>]
type Row<'v> = 
  { time: float; values: 'v [] }
  static member create (time:float) (values:'v list) = 
    { time = time; values = Array.ofList values }
  static member from_array (time:float) (values:'v []) = 
    { time = time; values = values }
  static member from_list (row:float list) =
    match row with
    | [] -> {time = 0.0; values = [||]}
    | v::vs -> {time = v; values = Array.ofList vs}
  static member interpolate_float t t0 t1 x0 x1 : float = x0 + (x1-x0)*(t-t0)/(t1-t0)
  static member zero_order_hold t t0 t1 x0 x1 = x0
  static member interpolate_reverse (interpolate_point:float -> float -> float -> 'v -> 'v -> 'v) (rows:Row<'v> list) (times:float list) =
    let later (r1:Row<'v>) (r2:Row<'v>) = r1.time > r2.time in
      let interpolate (time:float) (r1:Row<'v>) (r2:Row<'v>) = Array.map2 (interpolate_point time r1.time r2.time) r1.values r2.values in
      let rec in_scope (r:Row<'v>) (times:float list) =
        match times with
        | [] -> []
        | point :: more_points ->
          if r.time <= point then times
          else in_scope r more_points
      match rows with
      | [] -> []
      | current_sim_point :: later_sim_points ->
        let times_in_scope:float list = in_scope current_sim_point times
        // CS: leaving the comment below as it might be important for future reference
        // let rec interpolate_times stuff (times_in_scope:float list) =
        //Colin: if the tuple is destructured as function arguments it disables tail call elimination in WebSharper.
        //We might want to get WebSharper to improve this if it's not already in WebSharper 4. (to be tested)
        //   let (output:t<'v> list, current_sim_point:t<'v>, later_sim_points) = stuff in 
        let rec interpolate_times (output, current_sim_point, later_sim_points, times_in_scope) =
          match times_in_scope with
          | []      -> (*List.rev*) output (* ran out of real data *)
          | (t::ts) -> (* Invariant: not later current_simpoint data_point *)
              if t > current_sim_point.time
                then
                  match later_sim_points with
                  | [] -> (*List.rev*) output (* ran out of real data *)
                  | next_sim_point :: further_sim_points ->
                    if next_sim_point.time <= t (* can safely move to next sim point *)
                    then 
                      let ts' = t::ts
                      interpolate_times (output, next_sim_point, further_sim_points, ts')
                    else (* interpolate *)
                      let interp_point = interpolate t current_sim_point next_sim_point
                      let output'      = {time = t; values = interp_point} :: output
                      interpolate_times (output', current_sim_point, later_sim_points, ts)
                else (* sim point at data point *)
                  let output' = current_sim_point::output
                  interpolate_times (output', current_sim_point, later_sim_points, ts)
        interpolate_times ([], current_sim_point, later_sim_points, times_in_scope)
  static member to_string (separator:string) (value_to_string:'v -> string) (r:Row<'v>) = 
     String.concat separator ((string r.time)::(List.ofArray <| Array.map value_to_string r.values))
  

