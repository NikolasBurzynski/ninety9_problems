let rec _printList (list: string list) = 
  match list with
  | [x] -> 
    print_string x;
    print_string "\n"
  | x::rest ->
    print_string x;
    print_string " ";
    _printList rest
  | [] -> ()

  let rec printList (list: int list) = 
    match list with
    | [x] -> 
      print_int x;
      print_string "\n"
    | x::rest ->
      print_int x;
      print_string " ";
      printList rest
    | [] -> ()

  let rec printListList (list: 'a list list) = 
    match list with
    | [x] -> 
      printList x
    | x::rest -> 
      printList x;
      printListList rest
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

(*Q8*)
let rec compress (list: 'a list) : 'a list = 
  match list with
  | [] -> []
  | [singular] -> [singular]
  | x::(y::_ as t) -> if x = y then compress t else x::compress t 

(*Q9*)
(*This one is a bit tricky. The aux function has two secondary args. A list to hold the current inner list which will collect like values
   and a acc list of list. We will traverse the input and if our current list is empty we are looking at a new letter, we should append that new letter
    to the current list and then recurse on the rest of the list. If we have a current list with elements, we need to check if the element
      we are at is the same as the elements in the current list, if it is, we append another copy of it to the current list and continue
        recursing. If it isn't, then we have run out of the current element, so we need to append the current list to the acc and then recurse
  with a cleared out current list.*)
let rec pack_aux (list: 'a list) (current: 'a list) (acc: 'a list list) : 'a list list =
  match list, current with 
  | [], [] -> []
  | [], something -> something :: acc
  | x::rest, y::_ -> if x = y then pack_aux rest (x::current) acc else pack_aux rest [x] (current::acc)
  | x::rest, [] -> pack_aux rest [x] acc

let pack (list: 'a list) : 'a list list = reverse (pack_aux list [] [])

(* Q10 *)
let res encode_aux (list: 'a list) (current: 'a list) (cumulator: (int * 'a) list) (count: int) : (int * 'a) list = 
  match list, current with
  | [], [] -> []
  | [], x::_ -> (count, x)::cumulator
  | x::rest, [] -> encode_aux rest [x]cumulator 1
  | x::rest, y::_ -> if x = y then encode_aux rest [x] cumulator (count+1) else encode_aux rest [x] ((count, y)::cumulator) 0

let encode (list: 'a list): (int * 'a) list = reverse (encode_aux list [] [] 0) 

(*Bing bong*)

let () = 
  let _ = last ["a" ; "b" ; "c"] in 
  let _ = last_two ["a" ; "b" ; "c"] in 
  let _ = nth ["a" ; "b" ; "c"] 2 in 
  let _ = length [] in
  let _ = reverse ["a" ; "b" ; "c"] in
  let _ = palindrome ["a", "b", "a"] in
  let _ = flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]] in
  let _ = compress [1;1;1;1;1;2;3;4;5;6;6;6;7;7] in
  let packed = pack [1;1;1;1;1;2;3;4;5;6;6;6;7;7] in
    printListList packed
