[<JavaScript>]
module Microsoft.Research.CRNEngine.Errors

type EngineException(message) =
    inherit System.Exception(message)

type CompilationCancelledException() =
    inherit EngineException("CompilationCancelled exception is for internal use only")

type 'species ConflictingInitialException (sp:'species) =
    inherit EngineException("Initial species " + sp.ToString() + " has conflicting infinite rate reactions")

type SimulatorErrorException (str:string) =
    inherit EngineException("Simulator error: " + str)

(* Raise errors. *)
let compilation_cancelled ()                = raise(new CompilationCancelledException())
let conflicting_initial_error (sp:'species) = raise(new ConflictingInitialException<'species>(sp))
let ode_solver_error str                    = raise(new SimulatorErrorException ("ODE error: " + str))
let negative_simulation ()                  = raise(new SimulatorErrorException ("Cannot evaluate a proportional-error noise model for non-positive simulation values."))

(* Check a cancellation flag to see if it has been triggered by the UI. *)
let check_cancelled (cancel_flag:bool ref) =
  if (!cancel_flag) then compilation_cancelled()