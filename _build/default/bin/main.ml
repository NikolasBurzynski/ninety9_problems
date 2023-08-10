let rec printList (list: string list) = 
  match list with
  | [x] -> 
    print_string x;
    print_string "\n"
  | x::rest ->
    print_string x;
    print_string " ";
    printList rest
  | [] -> ()


(*Q1*)
let rec last (lst : 'a list) : 'a option =
  match lst with
  | [x] -> Some x
  | [] -> None
  | _ :: tail -> last tail

(*Q2*)
let rec last_two (lst : 'a list) : ('a * 'b) option =
  match lst with
  | [x ; y] -> Some (x , y)
  | [] -> None
  | [_] -> None
  | _ :: rest -> last_two rest

(*Q3*)
let rec nth (lst : 'a list) (idx : int) : 'a = 
  match (lst, idx) with
  | x::_ , 0 -> x
  | [], _ -> raise (Failure "nth")
  | _::list, idx -> nth list (idx-1)

(*Q4*)
let rec length_aux (list : 'a list) (acc : int) : int =
  match list with
  | [] -> acc
  | _::rest -> length_aux rest (acc+1)

let length (list : 'a list) : int = length_aux list 0

(*Q5*)
let rec reverse_aux (list: 'a list) (acc : 'a list) : 'a list =
  match list with
  | [] -> acc
  | x::rest -> reverse_aux rest (x::acc)

let reverse (list: 'a list) : 'a list = reverse_aux list []

(*Q6*)
let palindrome (list: 'a list) : bool = list = reverse list

(*Q7*)
type 'a node =
  | One of 'a 
  | Many of 'a node list

let rec flatten_aux (list: 'a node list) (acc: 'a list) : 'a list =
  match list with
  | [] -> acc
  | (One x) :: rest -> flatten_aux rest (x::acc)
  | Many inside::rest -> flatten_aux rest (flatten_aux inside acc) 

let flatten (list: 'a node list) : 'a list = reverse (flatten_aux list [])



let () = 
  let _ = last ["a" ; "b" ; "c"] in 
  let _ = last_two ["a" ; "b" ; "c"] in 
  let _ = nth ["a" ; "b" ; "c"] 2 in 
  let _ = length [] in
  let _ = reverse ["a" ; "b" ; "c"] in
  let _ = palindrome ["a", "b", "a"] in
  let flat = flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]] in
    printList flat
