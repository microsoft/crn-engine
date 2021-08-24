// Copyright (c) Microsoft Corporation.
// Licensed under the MIT License.

[<JavaScript>]
module Microsoft.Research.CliLibrary.Export

open Microsoft.Research.CRNEngine

(*let export_dot term namer file profile =
    let t_prev = Sys.time() in
    let dot = Export.to_dot term namer in
    let t_current = Sys.time() in
    System.IO.File.WriteAllText (file^".dot", dot);
    printfn "Wrote DOT file: %s.dot" file;
    if profile then Io.println ("Producing DOT representation took " + (Lib.display_float (t_current - t_prev)) + "s.")

let export_sbml term namer file profile =
      let t_prev = Sys.time() in
      let sbml = Export.to_sbml term namer namer in
      let t_current = Sys.time() in
      System.IO.File.WriteAllText (file^".xml", sbml);
      printfn "Wrote SBML file: %s.xml" file;
      if profile then Io.println ("Producing SBML representation took " + (Lib.display_float (t_current - t_prev)) + "s.")

let export_crn term namer (file : string) profile =
    let t_prev = Sys.time() in
    let text = Term.to_string namer term in
    let t_current = Sys.time() in
    System.IO.File.WriteAllText (   (if( file.EndsWith(".lbs") ) then file else file^".crn"), text);
    printfn "Wrote text file: %s" (file^".crn");
    if profile then Io.println ("Producing CRN representation took " + (Lib.display_float (t_current - t_prev)) + "s.")

let export_states term file profile =
    printfn "Starting state space computation...";
    let t_prev = Sys.time() in
    let settings = Term.get_settings term in
    let env = Settings.get_param_env settings in
    let g = Term.set_settings (settings |> Settings.set_sim_mode (SettingsConstants.sim_mode.Stochastic { algorithm = SettingsConstants.JIT })) term in
    let ss = StatespaceJIT.create_state_space g in
    let ss = StatespaceJIT.explore (ss, settings, term.calculus) (ref false) in
    let ss_info = StatespaceJIT.get_state_list ss in
    //let ss_text = StatespaceJIT.get_initial_state_key ss |> StatespaceJIT.string_of_state_key (fun s -> s.ToString()) ss in
    let tra_text, lab_text = StatespaceJIT.get_MRMC ss env in
    let (numStates, numTerminalStates, numTransitions) = StatespaceJIT.get_state_space_statistics ss_info in
    let t_current = Sys.time() in
    printfn "Number of terminal states: %d" numTerminalStates;
    printfn "Number of states: %d" numStates;
    printfn "Number of transitions: %d" numTransitions;
    printfn "Initial state: ";
    Io.write_file (file^".tra") tra_text;
    Io.write_file (file^".lab") lab_text;
    printfn "Wrote MRMC files: %s.tra and %s.lab" file file;
    let prism, _ = Export.to_prism g (Some ss) Lib.id Lib.id in
    System.IO.File.WriteAllText (file^".sm", prism);
    printfn  "Wrote PRISM file: %s.sm" file;
    if profile then Io.println ("State space computation took " + (Lib.display_float (t_current - t_prev)) + "s.");
    printfn "State space computation finished!"*)

let log_process c args =
    printfn "running %s %s" c args
    let p = System.Diagnostics.Process.Start (c, args)
    p.WaitForExit()
    p.ExitCode |> printfn "Exit code: %i\n"

let export_reaction_graph_dot crn file =
    let rs = (Crn.group_reactions crn).reactions

    let all_ss = rs |> List.collect (fun r -> r.allSpecies) |> List.map (fun (s:Species) -> crn.attributes.[s.name])

    let init_ss =
        crn.initials
        |> List.choose (fun i -> if i.value <> Expression.Float 0.0 then Some i.species.name else None)
        |> Set.ofList

    let r_att = "[penwidth=2.0,dir=none]"
    let style = ""
    let init_style = ",penwidth=\"3.0\""
    let sid (s : Species) = s.name |> sprintf "\"%s\""
    let said (s : Attributes) = s.name |> sprintf "\"%s\""
    let rate (r:Rate<Expression.t<string>,Expression.t<Key<Species>>>) = r.to_string (Expression.to_string id) Functional2.to_string
    let rr (r: Reaction<Species,Value,Expression.t<Key<Species>>>(*: Crn.reaction*)) =
        match r.reverse with
        | None -> r.rate |> rate
        | Some rev_rate -> sprintf "%s\n%s" (rate r.rate) (rate rev_rate)
    let r_to_n rid r = sprintf "r%d [label=\"%s\"];" rid (rr r)

    let r_to_e rid (r: Reaction<Species,Value,Expression.t<Key<Species>>> (*: Crn.reaction*)) =
        let rev_att =
            match r.reverse with
            | None -> "[dir=none]"
            | Some _ -> "[arrowtail=normal,dir=back]"
        let react_str =
            r.reactants
            |> List.map (fun entry -> entry.element)
            |> List.map sid
            |> String.concat " "
            |> sprintf "{%s}"
        let prod_str =
            r.products
            |> List.map (fun entry -> entry.element)
            |> List.map sid
            |> String.concat " "
            |> sprintf "{%s}"
        [ sprintf "%s -> r%d%s" react_str rid rev_att
        ; sprintf "r%d -> %s[arrowhead=onormal]" rid prod_str]

    let images = ref Set.empty<string>
    let to_image path svg =
        if Set.contains path !images then
            ()
        else
            images := Set.add path !images
            System.IO.File.WriteAllText (path, svg)

    let out_dir = System.IO.Path.GetDirectoryName file
    let out_file_name = System.IO.Path.GetFileNameWithoutExtension file |> sprintf "%s_rg"
    
    let species (s : Attributes) =
        match s.svg with
        | "" -> s.name |> sprintf "label=\"%s\""
        | svg ->
            let path = System.IO.Path.Combine (out_dir, sprintf "%s_%s.svg" out_file_name s.name)
            to_image path svg
            System.IO.Path.ChangeExtension (path, ".png") |> sprintf "image=\"%s\",label=\"\""

    let dot =
        seq {
            yield "digraph reaction_graph {"
            yield! all_ss |> Seq.map (fun s -> sprintf "%s [%s,shape=\"box\",style=\"rounded\"%s];" (said s) (species s) (if Set.contains s.name init_ss then init_style else style))
            yield "node [shape=box];"
            yield! rs |> Seq.mapi r_to_n
            yield! rs |> Seq.mapi r_to_e |> Seq.concat
            yield "}"
        } |> String.concat "\n"

    let dot_file = out_file_name |> sprintf "%s.dot" // System.IO.Path.ChangeExtension (file, ".dot")
    System.IO.File.WriteAllText (dot_file, dot)

    !images
    |> Set.iter (fun path -> log_process "inkscape" (sprintf "-z -e %s %s" (System.IO.Path.ChangeExtension (path, ".png")) path))
    //|> Set.iter (fun path -> log_process "inkscape" (sprintf "--file=%s --export-area-drawing --without-gui --export-pdf=%s" path (System.IO.Path.ChangeExtension (path, ".pdf"))))

    let pdf_file = out_file_name |> sprintf "%s.pdf" // System.IO.Path.ChangeExtension (file, ".svg")
    log_process "dot" (sprintf "-Tpdf %s -o %s" dot_file pdf_file)

let export_states_dot (crn:Crn) file =
    let ctmc_result = crn.to_ctmc()

    let state_ids =
        Dictionary.keys ctmc_result.ctmc.graph
        |> Seq.mapi (fun i k -> k, sprintf "state_%i" i)
        |> Map.ofSeq

    let images = ref Set.empty<string>
    let to_image path svg =
        if Set.contains path !images then
            ()
        else
            images := Set.add path !images
            System.IO.File.WriteAllText (path, svg)

    let out_dir = System.IO.Path.GetDirectoryName file

    let species i =
        let s = ctmc_result.to_species.[i]
        let sa = crn.attributes.[s.name]
        match sa.svg with
        | "" -> s.name
        | svg ->
            let path = System.IO.Path.Combine (out_dir, sprintf "%s.svg" s.name)
            to_image path svg
            System.IO.Path.ChangeExtension (path, ".png") |> sprintf "<img src=\"%s\"/>"

    let start_state = Dictionary.keys ctmc_result.ctmc.graph |> Seq.head

    let state st =
        seq {
            yield sprintf "%s [label=<" state_ids.[st];
            yield "<table border=\"0\" cellborder=\"0\" cellspacing=\"0\" cellpadding =\"0\">"
            yield! st |> Map.toSeq |> Seq.map (fun (s,n) -> sprintf "<tr>\n  <td align=\"right\">%d </td>\n  <td align=\"left\">%s</td>\n</tr>" n (species s))
            yield sprintf "</table>>%s];" (if st = start_state then ",penwidth=\"3.0\"" else "")
        } |> String.concat "\n"

    let rate = Expression.simplify >> Expression.to_string id

    let transition s (t :Transition) =
        sprintf "%s -> %s [label=\" %s \"];" state_ids.[s] state_ids.[t.target] (rate t.propensity)

    let dot =
        seq {
            yield "digraph state_space {";
            yield "node [shape=box];"
            yield! ctmc_result.ctmc.graph |> Dictionary.toSeq |> Seq.collect (fun (s,t) -> (state s)::(List.map (transition s) t))
            yield "}"
        } |> String.concat "\n"

    let file_name = System.IO.Path.GetFileNameWithoutExtension file |> sprintf "%s_ss.dot"

    let out_file = System.IO.Path.Combine (out_dir, file_name)
    System.IO.File.WriteAllText (out_file, dot)

    !images
    |> Set.iter (fun path -> log_process "inkscape" (sprintf "-z -e %s %s" (System.IO.Path.ChangeExtension (path, ".png")) path))

    log_process "dot" (sprintf "-Tsvg %s -O" out_file)