[<JavaScript>]
module Microsoft.Research.GEC.TransCrn

open Microsoft.Research.GEC.Trans
open Microsoft.Research.CRNEngine
open Api

open Parser

let separator = "_"

let spReplace (str:string) = 
    str.Replace("::","_")

let private newStr = // ref (fun () -> "Fixme")
    let count = ref 0
    (fun () -> count := !count + 1; string !count)

let rec newVar = 
    "sp" + newStr() + "_" 

let indexInitial (initial:Initial<Species,Expression.t<_>>) (str:string)=
    //let newSpeciesName = newVar + ((spReplace initial.species.name) + separator + (spReplace str))
    let newSpeciesName = ((spReplace initial.species.name) + separator + (spReplace str))
    let newSpecies = Species.create(newSpeciesName)
    let newInitial = {initial with species = newSpecies} 
    newInitial

let indexReaction (reaction:Reaction<Species,_,_>) (str:string)=
    let ncatalysts = reaction.catalysts |> Mset.map (fun (x) -> 
        //let newSpName = newVar + ((spReplace x.name) + separator + (spReplace str))
        let newSpName = ((spReplace x.name) + separator + (spReplace str))
        Species.create(newSpName))
    let nreactants = reaction.reactants |> Mset.map (fun (x) ->
        //let newSpName = newVar + ((spReplace x.name) + separator + (spReplace str))
        let newSpName = ((spReplace x.name) + separator + (spReplace str))
        Species.create(newSpName))
    let nproducts = reaction.products |> Mset.map (fun (x) -> 
        //let newSpName = newVar + ((spReplace x.name) + separator + (spReplace str))
        let newSpName = ((spReplace x.name) + separator + (spReplace str))
        Species.create(newSpName))
    let nreaction = {reaction with catalysts = ncatalysts; reactants = nreactants; products=nproducts}
    nreaction

let compInitial (initial:Initial<Species,Expression.t<_>>) (str:string) =
    //let newSpName = newVar + ((spReplace str) + separator + (spReplace initial.species.name))
    let newSpName = ((spReplace str) + separator + (spReplace initial.species.name))
    let newSpecies = Species.create(newSpName)
    let newInitial = {initial with species = newSpecies} 
    newInitial

let compReaction (reaction:Reaction<Species,_,_>) (str:string)=
    let ncatalysts = reaction.catalysts |> Mset.map (fun (x) -> 
        //let newSpName = newVar + ((spReplace str) + separator + (spReplace x.name))
        let newSpName = ((spReplace str) + separator + (spReplace x.name))
        Species.create(newSpName))
    let nreactants = reaction.reactants |> Mset.map (fun (x) -> 
        //let newSpName = newVar + ((spReplace str) + separator + (spReplace x.name))
        let newSpName = ((spReplace str) + separator + (spReplace x.name))
        Species.create(newSpName))
    let nproducts = reaction.products |> Mset.map (fun (x) -> 
        //let newSpName = newVar + ((spReplace str) + separator + (spReplace x.name))
        let newSpName = ((spReplace str) + separator + (spReplace x.name))
        Species.create(newSpName))

    let nreaction = {reaction with catalysts = ncatalysts; reactants = nreactants; products=nproducts}
    nreaction




let createComplexSpecies (sp:string list) = 
    if sp.IsEmpty then
        Species.create("")
    else 
        //let species = sp |> List.reduce (fun a b -> (a + "-" + b)) |> Species.create
        let species = sp |> List.reduce (fun a b -> 
            //newVar + ((spReplace a) + separator + (spReplace b))) |> Species.create
            ((spReplace a) + separator + (spReplace b))) |> Species.create
        species

let createComplexSpeciesWithEmptyCheck (sp:string list) = 
    if sp.IsEmpty then
        None
    else 
        //let species = sp |> List.reduce (fun a b -> (newVar + ((spReplace a) + separator + (spReplace b)))) |> Species.create
        let species = sp |> List.reduce (fun a b -> (((spReplace a) + separator + (spReplace b)))) |> Species.create
        Some(species)

let createComplexSpeciesList (sp:string list list) =
    if sp.IsEmpty then
        []
    else 
        sp 
        |> List.map (fun(x) -> createComplexSpeciesWithEmptyCheck(x)) 
        |> List.filter (fun x -> 
            match x with 
            | None -> false
            | Some(_) -> true)
        |> List.map (fun x -> x.Value)

let createComplexCompSpecies (sp:string list) (comp:string)= 
    //let spName = sp |> List.reduce (fun a b -> (a + "-" + b))
    let spName = sp |> List.reduce (fun a b -> ((spReplace a) + separator + (spReplace b)))
    //let species = Species.create(comp + "[" + spName + "]")
    //let newSpName = newVar + ((spReplace comp) + separator + spName)
    let newSpName = ((spReplace comp) + separator + spName)
    let species = Species.create(newSpName)
    species

let rec createCrnReactions (lbsProg:tLBSProg) (reactions:Reaction<_,_,_> list) (initials:Initial<_,_> list) =
    match lbsProg with 
    | LBSPar(prog1,prog2) -> 
        let (reactions1,initials1) = createCrnReactions prog1 [] []
        let (reactions2,initials2) = createCrnReactions prog2 [] []
        ((reactions1@reactions2@reactions),(initials1@initials2@initials))
    | LBSInitPop(initVal,value) -> 
        //let species = initVal |> List.reduce (fun a b -> (newVar + (spReplace a) + separator + (spReplace b))) |> Species.create
        let species = initVal |> List.reduce (fun a b -> ((spReplace a) + separator + (spReplace b))) |> Species.create
        let initial = Initial.create(false, (Expression.Float value), species, None, None)
        (reactions,initial::initials)
    | LBSCopy(index,prog) -> 
        let (reactions1,initials1) = createCrnReactions prog [] []
        let rinitials = initials1 |> List.map (fun(x) -> indexInitial x (index.ToString()))
        let rreactions = reactions1 |> List.map (fun(x) -> indexReaction x (index.ToString()))
        (rreactions@reactions,rinitials@initials)
    | LBSComp(comp,prog) -> 
        let (reactions1,initials1) = createCrnReactions prog [] []
        let rinitials = initials1 |> List.map (fun(x) -> compInitial x comp)
        let rreactions = reactions1 |> List.map (fun(x) -> compReaction x comp)
        (rreactions@reactions,rinitials@initials)
    | LBSCompDec(comp,prog) -> 
        let (reactions1,initials1) = createCrnReactions prog [] []
        let rinitials = initials1 |> List.map (fun(x) -> compInitial x comp)
        let rreactions = reactions1 |> List.map (fun(x) -> compReaction x comp)
        (rreactions@reactions,rinitials@initials)
    | LBSReac(enzymes,reactants,products,rate,massAction) ->
        let rreactants = createComplexSpeciesList(reactants) //|> Mset.from_list
        let renzymes = createComplexSpeciesList(enzymes) //|> Mset.from_list
        let rproducts = createComplexSpeciesList(products)  //|> Mset.from_list
        if massAction then
            //check if anything is empty
            let reaction = Reaction.create(renzymes, rreactants, !-> (Expression.Key rate ), rproducts)
            (reaction:: reactions,initials)
        else 
            (*let baseIdParserNoSpaces = (Parser.many1Satisfy Parser.isLetter .>>. Parser.manySatisfy (fun c -> Parser.isLetter c || Parser.isDigit c || c = '_'|| c = '-' || c = '\'') |>> fun (a,b) -> a + b) <?> "an identifier"
            let baseIdParser = baseIdParserNoSpaces .>> Parser.spaces
            let speciesparser = baseIdParser |>> fun (x) -> (Species.create(x))
            let speciesExpParser = Expression.parse speciesparser
            let exp_from_string (s:string) = Parser.from_string speciesExpParser s
            let exp = exp_from_string rate
            let reaction = Reaction.create_functional renzymes rreactants exp None rproducts
            (reaction:: reactions,initials)*)
            (reactions,initials)
    | LBSDegReac(reactants,rate) -> 
        let rreactants = createComplexSpeciesList(reactants)
        let reaction = Reaction.create([], rreactants, !-> (Expression.Key rate), [])
        (reaction::reactions,initials)
    | LBSTrans(complex1,complex2,compartment,rate,direction) -> 
        if direction = Ast.In then
            let reaction = Reaction.create([], [(createComplexSpecies complex1)], !-> (Expression.Key rate), [(createComplexCompSpecies complex2 compartment)])
            (reaction::reactions,initials)
        else 
            let reaction = Reaction.create([], [(createComplexCompSpecies complex1 compartment)], !-> (Expression.Key rate), [(createComplexSpecies complex2)])
            (reaction::reactions,initials)
    | LBSReacAbstraction((enzymes,reactants,products,rate,massAction),prog1) -> 
        let rreactants = createComplexSpeciesList(reactants) //|> Mset.from_list
        let renzymes = createComplexSpeciesList(enzymes) //|> Mset.from_list
        let rproducts = createComplexSpeciesList(products)  //|> Mset.from_list
        let (reactions1,initials1) = createCrnReactions prog1 [] []
        if massAction then
            let reaction = Reaction.create(renzymes, rreactants, !-> (Expression.Key rate), rproducts)
            ((reaction::reactions1)@reactions,initials1@initials)
        else 
            (reactions1@reactions,initials1@initials)
        
    | _ -> ([],[]) //This is basically LBSNil, 
    



let create (lbsProg:tLBSProg) = 
    let (reactions,initials) = createCrnReactions lbsProg List.empty List.empty
    let (crn:Crn) = {Crn.empty with reactions = reactions; initials = initials}
    let gcrn = Crn.group_reactions crn
    gcrn
