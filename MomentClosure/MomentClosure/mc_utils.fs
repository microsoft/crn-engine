// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

module Microsoft.Research.CRNEngine.MC_Utils

open Microsoft.Research.CRNEngine.Moments
open MathNet.Symbolics

(* renames symbol names in a MathNet.Symbolics.Expression according to the input f *)
let rec mapSymbolics f (e:MathNet.Symbolics.Expression) =
  match e with 
  | Number              _ -> e
  | Approximation       _ -> e
  | Identifier (Symbol x) -> f x |> Symbol |> Identifier
  | Constant            _ -> e
  | Power         (b,exp) -> Power (mapSymbolics f b, mapSymbolics f  exp)
  | Product            es -> es 
                              |> List.map (mapSymbolics f)
                              |> Product
  | Sum                es -> es 
                              |> List.map (mapSymbolics f)
                              |> Sum
  | Function  (fn, arg)  -> Function (fn, arg |> mapSymbolics f)
  | FunctionN (fn, args) -> FunctionN (fn, args |> List.map (mapSymbolics f))
  | ComplexInfinity      -> ComplexInfinity
  | PositiveInfinity     -> PositiveInfinity
  | NegativeInfinity     -> NegativeInfinity
  | Undefined            -> Undefined       


(* Polynomialization: converts all fractions into polynomials in a set of ODEs by adding new variables and ODEs.
   This is an approximation technique from:
   "Abstraction of Elementary Hybrid Systems by Variable Transformation" by J. Liu, N. Zhan, H. Zhao and L. Zou
   (https://arxiv.org/abs/1403.7022) *)
let polynomialize (odes:t) =
  
  (* VT creates a new variable per denominator and per negative power *)
  let algoVT (e:MathNet.Symbolics.Expression) = 
    let f exp = match exp with
                | Number              _ -> exp
                | Approximation       _ -> exp
                | Identifier          _ -> exp
                | Constant            _ -> exp
                | Power         (b,exp) -> failwith "TODO"
                (* these should not occur *)
                | Product            _ -> exp
                | Sum                _ -> exp
                | Function  (fn, arg)  -> failwith "Unexpected input in polynomialization"
                | FunctionN (fn, args) -> failwith "Unexpected input in polynomialization"
                | ComplexInfinity      -> ComplexInfinity
                | PositiveInfinity     -> PositiveInfinity
                | NegativeInfinity     -> NegativeInfinity
                | Undefined            -> Undefined       
    e 
    |> MathNet.Symbolics.Algebraic.summands 
    |> List.map (fun e' -> 
      e' 
      |> MathNet.Symbolics.Algebraic.factors
      |> MathNet.Symbolics.Expression.Product
      |> f
    )
    |> MathNet.Symbolics.Expression.Sum

  (* U approximates each new variable with a "polynomialized" form *)
  let uAlgo e = failwith "TODO"

  let newVarID = 
    let x = ref 0
    fun () -> let id = !x
              x := !x + 1
              id
  odes.equations
    |> List.map (fun (x, e) -> failwith "todo")
    |> ignore
  failwith "todo"

(* simple string format print utilities*)
let printMoments latex e = 
      let formatter = if latex then LaTeX.format else Infix.format
      e 
      |> Algebraic.summands
      |> List.map (fun ei -> 
            let coef, rest = splitCoefficient ei
            let c = match coef with
                    | None -> Expression.One
                    | Some n -> n 
            let cString = 
              if MathNet.Symbolics.Evaluate.evaluate (dict []) c = FloatingPoint.Real (-1.0) (*Expression.MinusOne*) then "-"
              elif c = Expression.One then ""
              else formatter c + (if latex then "" else " * ")
            if c = Expression.Zero
              then "0"
              else  cString + match rest with 
                              | None   -> ""
                              | Some m -> formatter m)
      |> String.concat " + "

let printMoment species (m:int[], e) = sprintf "d%s/dt \t= %s" (toMonomial species m) (printMoments false e)


let to_string (momentSystem:t) = 
  let species = allSpecies momentSystem
  momentSystem |> combine_equations |> List.map (printMoment species) |> String.concat "\n"



(* prints moments in LaTeX *)
let latexMoment species (m:int[], e) = sprintf "\dfrac{d%s}{dt} &= %s" (toMonomial species m) (printMoments true e)
let to_latex (momentSystem:t) = 
  let species = allSpecies momentSystem
  momentSystem
  |> combine_equations 
  |> List.map (latexMoment species) 
  |> String.concat "\\\\ \n" 
  |> fun x -> x.Replace("*", "").Replace("+ -","-")
  |> sprintf "\\begin{align*}\n%s\n\\end{align*}"


(* exports a moments.t system to Matlab *)
let to_matlab (mSystem:t) = 
  (* Get all species in the system *)
  let species = allSpecies mSystem
  
  (* Printing utilities *)
  let newline = Lib.newline
  let tabtab = "\t\t"
  
  let to_matlab_monomial (m : int[]) =
    m 
    |> Array.map (fun i -> 
      let pop = mSystem.populations.index_to_species.[i]
      pop.species.name)

  // prints a moment "<A*B>" as "A.B" for plot names and as "A_B" for Matlab variable names
  let printMonomialAt separator (m:int[]) = 
    m
    |> Array.mapi (fun i pow -> match pow with 
                                | 0 -> ""
                                | _ -> species.[i].name + match pow with 
                                                          | 1 -> ""
                                                          | _ -> pow.ToString()
    )
    |> Array.filter ((<>) "")
    |> String.concat separator
  
  let toSpecies (m:int[]) = 
    m  
    |> printMonomialAt "_"
    |> Species.create

  let printPlotAt i = 
    mSystem.monomials.[i] 
    |> printMonomialAt "."
    |> sprintf "<%s>"

  (* Setup an initial for each moment *)
  let initials = 
    mSystem.monomials
    |> List.map (fun p -> 
      let moment = toSpecies p
      p 
      |> Array.mapi (fun i pow -> mSystem.populations.index_to_species.[i].value**(float pow))
      |> Array.fold (*) 1.0
      |> fun value -> Initial<Species, float>.create(false, value, moment, None, None))

  (* Constants for various names used in the syntax. *)
  let outerFunctionName,innerFunctionName,timeVar,popsVar,innerReturnVar = "crn_export","odes","t","x__","dxdt"
  let initialTimeVar,finalTimeVar,speciesVar,odeSolverName,solutionVar,lengthVar,loopVar = "tstart","tfinal","species","ode15s","sol","n","i"
  let plotsVar = "plots"

  (* String builder for the final Matlab code *)
  let sb = Stringbuilder.empty ()
  let append = Stringbuilder.append sb
  
  
  (* Code for the outer function. *)
  append ("% " + sprintf "Moment closure order: %i" mSystem.mc_settings.order + newline)
  append ("function " + solutionVar + " = " + outerFunctionName + "()" + newline + newline)
  append (initialTimeVar + " = " + (string mSystem.sim_settings.initial) + "; % Initial time" + newline)
  append (finalTimeVar + " = " + (string mSystem.sim_settings.final) + "; % Final time" + newline)
  append (speciesVar + " = {" + (mSystem.monomials |> Lib.string_of_list (printMonomialAt "_" >> sprintf "\'%s\'") ",") + "}; % The list of all moments" + newline)
  
  (* Specify plot names *)
  let havePlots = not (List.isEmpty mSystem.mc_settings.plots)
  if havePlots then
    append (
      plotsVar + " = {" 
        + (mSystem.mc_settings.plots 
           |> Lib.string_of_list (fun (plot,_) -> 
             "\'" + (plot |> Expression.to_string printPlotAt) + "\'") ",") 
              + "};" + newline)
  append (lengthVar + " = length(" + speciesVar + ");" + newline + newline)
  
  (* Assign initial conditions *)
  append ("% Assign initial conditions" + newline)
  append ("x0 = ones(" + lengthVar + ",1);")
  initials
  |> List.iteri (fun i initial -> 
    if initial.value <> 0.0 then append (newline + "x0(" + string (i+1) + ") = " + string initial.value + ";" + tabtab + "% " + initial.species.name)
  )
  append (newline + newline)

  (* Solver the ODEs *)
  append ("% Solve the ODEs" + newline)
  append ("[" + timeVar + "," + popsVar + "] = " 
              + odeSolverName + "(@" 
              + innerFunctionName + ",[" 
              + initialTimeVar + " " 
              + finalTimeVar + "],x0,[]" 
              (*+ paramsVarStr*) + ");" 
              + newline + newline)
    
  append ("% Write out a solution structure to be returned by the function" + newline)
  append ("for " + loopVar + " = 1:" + lengthVar + newline)
  append ("  " + solutionVar + ".(" + speciesVar + "{" + loopVar + "}) = " + popsVar + "(:," + loopVar + ");" + newline)
  append ("end" + newline + newline)

  if havePlots then
      (* Apply plot expressions *)
      append ("% Apply plot expressions" + newline)
      append ("plotsValues = zeros(length(" + popsVar + ")," + string (List.length mSystem.mc_settings.plots) + ");")
      let speciesPrefixed species = "sol.(\'" + (species |> Species.to_string) + "\')"
      mSystem.mc_settings.plots
      |> List.iteri(fun i (plot, _) ->
          //MATLAB's elementwise operators require dot prefixes, we could generalise the Expression.to_string, just patch locally for now
          let asMatlab =
              (Expression.to_string (fun i -> mSystem.monomials.[i] |> printMonomialAt "_" |> sprintf "sol.%s") plot)
                  .Replace("*", ".*")
                  .Replace("/", "./")
                  .Replace("^", ".^")
                
          append (newline + "plotsValues(:," + string (i + 1) + ") = " + asMatlab + ";"))
      append (newline + newline)

  (* Produce a plot *)
  append ("% Produce a plot" + newline)
  append ("figure;" + newline)

  let plotsString, legendsString =
      if havePlots then
          "plotsValues", plotsVar
      else
          popsVar, speciesVar   

  append ("plot(" + timeVar + ", " + plotsString + ")" + newline)
  append ("xlabel('Time')" + newline)
  append ("ylabel('Concentration')" + newline)
  append ("box off" + newline)
  append ("legend(" + legendsString + ",'box','off')")
  append (newline + newline + "return" + newline + newline + "%%%" + newline + newline)


  (* Code for the inner function. *)
  append ("function " + innerReturnVar + " = " + innerFunctionName 
          + "(" + timeVar + "," + popsVar + (*paramsVarStr +*) ")" + newline + newline)

  (* Assign states *)
  append ("% Assign states")
  initials
  |> List.iteri (fun i initial -> append (newline + initial.species.name + " = " + popsVar + "(" + string (i+1) + ")" + ";"))
  append (newline + newline)

  (* Define derivatives *)
  append ("% Assign derivatives");
  let d species = "d" + species
  let deriv_string (eq:Expression) =
    eq 
    |> mapSymbolics 
        (fun symbol -> symbol.Replace("E[","")
                             .Replace("]","")
                             .Replace("*","_")
                             .Replace("^",""))
    |> Infix.format

  mSystem.equations
  |> List.iter (fun (m, eq) -> append (newline + (d <| printMonomialAt "_" m) + " = " + deriv_string eq + ";"))
  append (newline + newline)

  (* Postamble which closes the inner function. *)
  append (innerReturnVar + " = [" + (initials |> Lib.string_of_list (fun init -> d init.species.name) ";") + "];" + newline + newline)
  append ("return")

  sb.ToString()