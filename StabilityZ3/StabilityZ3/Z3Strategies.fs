module Microsoft.Research.Biology.StabilityZ3.Z3Strategies


type z3uint = 
    | UI of uint32
    | Infty
    override this.ToString() = 
        match this with
        | UI x -> sprintf "%i" (int64 x)
        | Infty -> "infty"

type Z3Param = 
    | Z3PBool of bool
    | Z3PSmallInt  of z3uint
    | Z3PMidInt    of z3uint
    | Z3PLargeInt  of z3uint
    static member Randomize (rnd:System.Random) (x:Z3Param) = 
        match x with 
        | Z3PBool  _     -> rnd.Next(2) = 0 |> Z3PBool
        | Z3PSmallInt _  -> 128 |> rnd.Next |> uint32 |> UI |>  Z3PSmallInt
        | Z3PMidInt   _  -> 100000 |> rnd.Next |> uint32 |> UI |> Z3PMidInt
        | Z3PLargeInt _  -> 4294967295.0*rnd.NextDouble() |> uint32 |> UI |> Z3PLargeInt //TODO: Add infinity as a random option?
    
    

// builtin strategy for solving QF_NRA problems using only nlsat.
type qfnra_nlsat = 
    { pars : Map<string, Z3Param>   
    }
    static member Default = 
        [ "arith_lhs"          , false             |> Z3PBool
        ; "cache_all"          , false             |> Z3PBool
        ; "common_patterns"    , true              |> Z3PBool
        ; "complete"           , true              |> Z3PBool
        ; "distributivity"     , true              |> Z3PBool
        ; "distributivity_blowup" , 32u            |> UI |> Z3PSmallInt
        ; "elim_and"           , false             |> Z3PBool   
        ; "elim_root_objects"  , true              |> Z3PBool
        ; "eq2ineq"            , false             |> Z3PBool
        ; "expand_power"       , false             |> Z3PBool
        ; "factor"             , true              |> Z3PBool
        ; "factor_max_prime"   , 31u               |> UI |> Z3PSmallInt
        ; "factor_num_primes"  , 1u                |> UI |> Z3PSmallInt
        ; "factor_search_size" , 5000u             |> UI |> Z3PMidInt
        ; "flat"               , true              |> Z3PBool
        ; "gcd_rounding"       , false             |> Z3PBool
        ; "hoist_cmul"         , false             |> Z3PBool
        ; "hoist_mul"          , false             |> Z3PBool
        ; "ignore_labels"      , false             |> Z3PBool
        ; "lazy"               , 0u                |> UI |> Z3PSmallInt
        ; "local_ctx"          , false             |> Z3PBool
        ; "local_ctx_limit"    , 4294967295u       |> UI |> Z3PLargeInt
        ; "max_args"           , 128u              |> UI |> Z3PSmallInt
        ; "max_conflicts"      , 4294967295u       |> UI |> Z3PLargeInt
        ; "max_degree"         , 64u               |> UI |> Z3PSmallInt
        ; "max_memory"         , 4294967295u       |> UI |> Z3PLargeInt
        ; "max_prime"          , Infty             |> Z3PLargeInt
        ; "max_rounds"         , 2u                |> UI |> Z3PSmallInt
        ; "max_search_size"    , Infty             |> Z3PLargeInt
        ; "max_steps"          , 4294967295u       |> UI |> Z3PLargeInt
        ; "min_mag"            , 16u               |> UI |> Z3PSmallInt
        ; "minimize_conflicts" , false             |> Z3PBool
        ; "mul_to_power"       , false             |> Z3PBool
        ; "num_primes"         , 1u                |> UI |> Z3PSmallInt
        ; "randomize"          , true              |> Z3PBool
        ; "reorder"            , true              |> Z3PBool
        ; "seed"               , 0u                |>  UI |> Z3PLargeInt
        ; "shuffle_vars"       , false             |> Z3PBool
        ; "simplify_conflicts" , true              |> Z3PBool
        ; "skolemize"          , true              |> Z3PBool
        ; "solve_eqs_max_occs" , Infty             |> Z3PLargeInt
        ; "som"                , false             |> Z3PBool
        ; "som_blowup"         , 4294967295u       |> UI |> Z3PLargeInt
        ; "sort_store"         , false             |> Z3PBool
        ; "sort_sums"          , false             |> Z3PBool
        ; "split_concat_eq"    , false             |> Z3PBool
        ; "split_factors"      , true              |> Z3PBool
        ; "theory_solver"      , true              |> Z3PBool
        ; "zero_accuracy"      , 0u                |> UI |> Z3PSmallInt
        ]
        |> Map.ofSeq
        |> fun x -> {pars = x}
    static member Random (rnd:System.Random)=         
        let default_fraction = 10 //1/default_fraction probability to randomize values                    
        {pars = 
            qfnra_nlsat.Default.pars
            |> Map.map(fun _ v -> if rnd.Next(default_fraction) = 0 then Z3Param.Randomize rnd v else v)
        }
    

let MkSolver (z3:Microsoft.Z3.Context) (p:qfnra_nlsat) =     
    let d   = qfnra_nlsat.Default

    
    let z3p = 
        p.pars
        |> Map.toArray
        |> Array.filter(fun (key,v) ->    d.pars.[key]<>v) //keep only seetings that are different from the defaults        
        |> Array.fold(fun  (acc:Microsoft.Z3.Params) (key,v) ->
            match v with 
            | Z3PBool x -> acc.Add(key,x)
            | Z3PSmallInt x 
            | Z3PMidInt x   
            | Z3PLargeInt x ->
                match x with 
                | UI ux -> acc.Add(key,ux)
                | Infty -> acc.Add(key,"infty")      
            ) (z3.MkParams())
    z3.UsingParams(z3.MkTactic("qfnra-nlsat"),z3p)

let MkRndSolver (z3:Microsoft.Z3.Context) = 
    new System.Random()
    |> qfnra_nlsat.Random
    |> MkSolver z3         



    (*
    override this.ToString() = 
         let d = qfnra_nlsat.Default
         [ (if this.arith_lhs              <> d.arith_lhs             then sprintf ":arith_lhs             %A" this.arith_lhs                       else "")
         ; (if this.cache_all              <> d.cache_all             then sprintf ":arith_lhs             %A" this.cache_all                       else "")
         ; (if this.common_patterns        <> d.common_patterns       then sprintf ":common_patterns       %A" this.common_patterns                 else "")                          
         ; (if this.complete               <> d.complete              then sprintf ":complete              %A" this.complete                        else "")   
         ; (if this.distributivity         <> d.distributivity        then sprintf ":distributivity        %A" this.distributivity                  else "")   
         ; (if this.distributivity_blowup  <> d.distributivity_blowup then sprintf ":distributivity_blowup %i" this.distributivity_blowup           else "")   
         ; (if this.elim_and               <> d.elim_and              then sprintf ":elim_and              %A" this.elim_and                        else "")   
         ; (if this.elim_root_objects      <> d.elim_root_objects     then sprintf ":elim_root_objects     %A" this.elim_root_objects               else "")   
         ; (if this.eq2ineq                <> d.eq2ineq               then sprintf ":eq2ineq               %A" this.eq2ineq                         else "")   
         ; (if this.expand_power           <> d.expand_power          then sprintf ":expand_power          %A" this.expand_power                    else "")   
         ; (if this.factor                 <> d.factor                then sprintf ":factor                %A" this.factor                          else "")   
         ; (if this.factor_max_prime       <> d.factor_max_prime      then sprintf ":factor_max_prime      %i" this.factor_max_prime                else "")   
         ; (if this.factor_num_primes      <> d.factor_num_primes     then sprintf ":factor_num_primes     %i" this.factor_num_primes               else "")   
         ; (if this.factor_search_size     <> d.factor_search_size    then sprintf ":factor_search_size    %i" this.factor_search_size              else "")   
         ; (if this.flat                   <> d.flat                  then sprintf ":flat                  %A" this.flat                            else "")   
         ; (if this.gcd_rounding           <> d.gcd_rounding          then sprintf ":gcd_rounding          %A" this.gcd_rounding                    else "")   
         ; (if this.hoist_cmul             <> d.hoist_cmul            then sprintf ":hoist_cmul            %A" this.hoist_cmul                      else "")   
         ; (if this.hoist_mul              <> d.hoist_mul             then sprintf ":hoist_mul             %A" this.hoist_mul                       else "")   
         ; (if this.ignore_labels          <> d.ignore_labels         then sprintf ":ignore_labels         %A" this.ignore_labels                   else "")   
         ; (if this.``lazy``               <> d.``lazy``              then sprintf ":lazy                  %i" this.``lazy``                        else "")   
         ; (if this.local_ctx              <> d.local_ctx             then sprintf ":local_ctx             %A" this.local_ctx                       else "")   
         ; (if this.local_ctx_limit        <> d.local_ctx_limit       then sprintf ":local_ctx_limit       %i" this.local_ctx_limit                 else "")   
         ; (if this.max_args               <> d.max_args              then sprintf ":max_args              %i" this.max_args                        else "")   
         ; (if this.max_conflicts          <> d.max_conflicts         then sprintf ":max_conflicts         %i" this.max_conflicts                   else "")   
         ; (if this.max_degree             <> d.max_degree            then sprintf ":max_degree            %i" this.max_degree                      else "")   
         ; (if this.max_memory             <> d.max_memory            then sprintf ":max_memory            %i" this.max_memory                      else "")   
         ; (if this.max_prime              <> d.max_prime             then sprintf ":max_prime             %s" (this.max_prime.ToString())          else "")   
         ; (if this.max_rounds             <> d.max_rounds            then sprintf ":max_rounds            %i" this.max_rounds                      else "")   
         ; (if this.max_search_size        <> d.max_search_size       then sprintf ":max_search_size       %s" (this.max_search_size.ToString())    else "")   
         ; (if this.max_steps              <> d.max_steps             then sprintf ":max_steps             %i" this.max_steps                       else "")   
         ; (if this.min_mag                <> d.min_mag               then sprintf ":min_mag               %i" this.min_mag                         else "")   
         ; (if this.minimize_conflicts     <> d.minimize_conflicts    then sprintf ":minimize_conflicts    %A" this.minimize_conflicts              else "")   
         ; (if this.mul_to_power           <> d.mul_to_power          then sprintf ":mul_to_power          %A" this.mul_to_power                    else "")   
         ; (if this.num_primes             <> d.num_primes            then sprintf ":num_primes            %i" this.num_primes                      else "")   
         ; (if this.randomize              <> d.randomize             then sprintf ":randomize             %A" this.randomize                       else "")   
         ; (if this.reorder                <> d.reorder               then sprintf ":reorder               %A" this.reorder                         else "")   
         ; (if this.seed                   <> d.seed                  then sprintf ":seed                  %i" this.seed                            else "")   
         ; (if this.shuffle_vars           <> d.shuffle_vars          then sprintf ":shuffle_vars          %A" this.shuffle_vars                    else "")   
         ; (if this.simplify_conflicts     <> d.simplify_conflicts    then sprintf ":simplify_conflicts    %A" this.simplify_conflicts              else "")   
         ; (if this.skolemize              <> d.skolemize             then sprintf ":skolemize             %A" this.skolemize                       else "")   
         ; (if this.solve_eqs_max_occs     <> d.solve_eqs_max_occs    then sprintf ":solve_eqs_max_occs    %s" (this.solve_eqs_max_occs.ToString()) else "")   
         ; (if this.som                    <> d.som                   then sprintf ":som                   %A" this.som                             else "")   
         ; (if this.som_blowup             <> d.som_blowup            then sprintf ":som_blowup            %i" this.som_blowup                      else "")   
         ; (if this.sort_store             <> d.sort_store            then sprintf ":sort_store            %A" this.sort_store                      else "")   
         ; (if this.sort_sums              <> d.sort_sums             then sprintf ":sort_sums             %A" this.sort_sums                       else "")   
         ; (if this.split_concat_eq        <> d.split_concat_eq       then sprintf ":split_concat_eq       %A" this.split_concat_eq                 else "")   
         ; (if this.split_factors          <> d.split_factors         then sprintf ":split_factors         %A" this.split_factors                   else "")   
         ; (if this.theory_solver          <> d.theory_solver         then sprintf ":theory_solver         %A" this.theory_solver                   else "")   
         ; (if this.zero_accuracy          <> d.zero_accuracy         then sprintf ":zero_accuracy         %i" this.zero_accuracy                   else "")
         ]
         |> String.concat " "        
         |> sprintf "(! qfnra-nlsat %s)"
        
    static member PatchFile (smt:string) (solver:qfnra_nlsat)=         
        let solver_string = sprintf "(check-sat-using %s)" (solver.ToString())
        smt.Replace("(check-sat)", solver_string)

    static member PatchFileWithRandom (smt:string) = 
        new System.Random()
        |> qfnra_nlsat.Random
        |> qfnra_nlsat.PatchFile smt        

    static member Randomize (n:int) (smt:string) = 
        let rnd = new System.Random()
        Array.init (100*n) (fun _ -> qfnra_nlsat.Random rnd) 
        |> Array.distinct
        |> Array.take n
        |> Array.map (qfnra_nlsat.PatchFile smt)
*)

(*
    ; elim_rem           : bool // replace (rem x y) with (ite (>= y 0) (mod x y) (- (mod x y))). (default: false)
    ; elim_inverses   : bool // (default: true) eliminate inverse trigonometric functions (asin, acos, atan).
    algebraic_number_evaluator (bool) simplify/evaluate expressions containing (algebraic) irrational numbers. (default: true)    
    bit2bool (bool) try to convert bit-vector terms of size 1 into Boolean terms (default: true)
    blast_distinct (bool) expand a distinct predicate into a quadratic number of disequalities (default: false)
    blast_distinct_threshold (unsigned int) when blast_distinct is true, only distinct expressions with less than this number of arguments are blasted (default: 4294967295)
    blast_eq_value (bool) blast (some) Bit-vector equalities into bits (default: false)
    bv_sort_ac (bool) sort the arguments of all AC operators (default: false)
    bvnot2arith (bool) replace (bvnot x) with (bvsub -1 x) (default: false)                        
    elim_sign_ext (bool) expand sign-ext operator using concat and extract (default: true)
    elim_to_real (bool) eliminate to_real from arithmetic predicates that contain only integers. (default: false)    
    expand_select_store (bool) replace a (select (store ...) ...) term by an if-then-else term (default: false)
    expand_store_eq (bool) reduce (store ...) = (store ...) with a common base into selects (default: false)
    expand_tan (bool) replace (tan x) with (/ (sin x) (cos x)). (default: false)        
    hi_div0 (bool) use the 'hardware interpretation' for division by zero (for bit-vector terms) (default: true)    
    ite_chaing (bool) (default: true) minimize the number of auxiliary variables during CNF encoding by identifing if-then-else chains
    ite_extra (bool) (default: true) add redundant clauses (that improve unit propagation) when encoding if-then-else formulas
    ite_extra_rules (bool) extra ite simplifications, these additional simplifications may reduce size locally but increase globally (default: false)
    ite_solver (bool) (default: true) use if-then-else solver.    
    mul2concat (bool) replace multiplication by a power of two into a concatenation (default: false)    
    pull_cheap_ite (bool) pull if-then-else terms when cheap. (default: false)
    push_ite_arith (bool) push if-then-else over arithmetic terms. (default: false)
    push_ite_bv (bool) push if-then-else over bit-vector terms. (default: false)
    push_to_real (bool) distribute to_real over * and +. (default: true)        
    sk_hack (bool) hack for VCC (default: false)    
    udiv2mul (bool) convert constant udiv to mul (default: false)    
*)