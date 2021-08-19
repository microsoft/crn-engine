[<JavaScript>]
module Microsoft.Research.CRNEngine.Queue

//Okasaki, Chris. Purely functional data structures. p53
//modified to use options

type t<'a> = {
    front: list<'a>;
    back: list<'a>;
}

let empty<'a> : t<'a> = { front=[]; back=[] }

let enqueue queue item = { queue with back = item :: queue.back}

let start item = { front=[item]; back=[] }

let dequeue queue =
    match queue.front with
    | [] ->
        begin
            match List.rev queue.back with
            | [] -> (None, empty)
            | front :: back -> (Some front, {front = back; back = [] })
        end
    | front :: back -> (Some front, {front = back; back = queue.back})