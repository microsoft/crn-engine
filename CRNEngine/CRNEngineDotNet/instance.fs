namespace Microsoft.Research.CRNEngine
[<JavaScript>]
type Instance<'e> when 'e:equality = 
  { model:string; sweep:string; assignment:string; environment:Environment.t; settings:Simulation_settings<'e>; name:string }
  static member create (model:string, sweep:string, assignment:string, environment:Environment.t, settings:Simulation_settings<'e>, name:string) = 
    { model = model; sweep = sweep; assignment = assignment; environment = environment; settings = settings; name = name }
  static member create (model:string, sweep:string, assignment:string, environment:Environment.t, settings:Simulation_settings<'e>) = 
    Instance<'e>.create(model,sweep,assignment,environment,settings,"")
  static member create (model:string, sweep:string, environment:Environment.t, settings:Simulation_settings<'e>, name:string) = 
    Instance<'e>.create(model,sweep,Environment.to_string environment,environment,settings,name)
  static member create (model:string, sweep:string, environment:Environment.t, settings:Simulation_settings<'e>) = 
    Instance<'e>.create(model,sweep,Environment.to_string environment,environment,settings,"")
  static member create (model:string, sweep:string, assignment:string, environment:Environment.t) = 
    Instance<'e>.create(model,sweep,assignment,environment,Simulation_settings<'e>.defaults)
  static member create (model:string, sweep:string, environment:Environment.t) = 
    Instance<'e>.create(model,sweep,Environment.to_string environment,environment,Simulation_settings<'e>.defaults)
  static member empty = Instance.create("","","",Environment.empty,Simulation_settings<'e>.defaults,"")
  static member same_sweep (i1:Instance<'e>) (i2:Instance<'e>) = (i1.model = i2.model) && (i1.sweep = i2.sweep)
  member i.update_environment e = { i with environment = Map(Seq.concat [ (Map.toSeq i.environment) ; (Map.toSeq e) ]) }