module List

// val fold: 
//    folder: 'State -> 'T -> 'State ->
//    state : 'State  ->
//    list  : 'T list 
//         -> 'State
let fold folder state list =
    let rec loop s = function
        | [] -> s
        | head :: tail -> loop (folder s head) tail
    loop state list

let x = [1;2;34;455] |> fold (fun state item -> state + item) 0
let y = [1;2;34;455] |> fold (+) 10

let z = 1::2::34::455::[]

// val foldBack: 
//    folder: 'T -> 'State -> 'State ->
//    list  : 'T list ->
//    state : 'State  
//         -> 'State
let foldBack folder list state =
    let rec loop s = function
        | [] -> s
        | head :: tail -> folder (loop s tail) head
    loop state list
let yy = foldBack (+) [1;2;34;455] 10
let s1 = foldBack (sprintf "(%s,%s)") ["1";"2";"34";"455"] "init"
let s2 = ["1";"2";"34";"455"] |> fold (sprintf "(%s,%s)") "init"

// val reduce: 
//    reduction: 'T -> 'T -> 'T ->
//    list     : 'T list        
//            -> 'T
let reduce f list = 
    match list with
    | head::tail -> List.fold f head tail
    | [] -> failwith "The list was empty!"

let reduceElmStyle reducer defaultValue = function
    | [] -> defaultValue
    | head::tail -> List.fold reducer head tail
let s3 = ["1";"2";"34";"455"] |> reduceElmStyle (sprintf "%s,%s") "default"
let s4 = ["1";"2";"34";"455"] |> reduce (sprintf "%s,%s")
let s5 = [] |> reduceElmStyle (sprintf "(%s,%s)") "default"