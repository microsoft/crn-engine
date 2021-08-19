namespace Microsoft.Research.CRNEngine 
open WebSharper
[<JavaScript>]
type Ode_settings<'e> when 'e:equality = 
  {
    simulation: Simulation_settings<'e>;
    simulations: Simulation_settings<'e> list;
    deterministic: Deterministic_settings;
    inference: Inference_settings.t; 
    data: Dataset list;
    units: Units;
    parameters: Parameter list;
    rates:Map<string, 'e>;
    sweeps: Sweep list;
  }
  static member create () = {
    simulation = Simulation_settings.defaults;
    simulations = []
    deterministic = Deterministic_settings.defaults;
    inference = Inference_settings.defaults; 
    data = [];
    units = Units.defaults;
    parameters = [];
    rates = Map.empty
    sweeps = [];
  }
  member s.substitute (e:Environment.t) = {s with parameters = Parameters.substitute e s.parameters}
  member s.update_times (times:float list) = {s with simulation = s.simulation.update_times times }
  member s.get_inference_parameters = 
    Parameter.filter_variable //Only keep parameters that are allowed to vary
    <| Parameters.remove (Sweeps.get_variables s.sweeps) s.parameters  //Remove sweep parameters

