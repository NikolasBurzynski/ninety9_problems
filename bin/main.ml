let rec _printList (list: string list) = 
  match list with
  | [x] -> 
    print_string x;
    print_string "\n"
  | x::rest ->
    print_string x;
    print_string ":";
    _printList rest
  | [] -> ()

let rec _printMap (list: (int * int) list) =
  match list with
  | [(count, value)] ->
    print_int count;
    print_string ":";
    print_int value;
    print_string "\n" 
  | (count, value)::rest ->
    print_int count;
    print_string ":";
    print_int value;
    print_string " ";
    _printMap rest
  | [] -> ()

  let rec _printMap (list: (int * string) list) =
    match list with
    | [(count, value)] ->
      print_int count;
      print_string ":";
      print_string value;
      print_string "\n" 
    | (count, value)::rest ->
      print_int count;
      print_string ":";
      print_string value;
      print_string " ";
      _printMap rest
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

  let rec _printListList (list: 'a list list) = 
    match list with
    | [x] -> 
      printList x
    | x::rest -> 
      printList x;
      _printListList rest
    | [] -> ()


(* Q1 *)
let rec last (lst : 'a list) : 'a option =
  match lst with
  | [x] -> Some x
  | [] -> None
  | _ :: tail -> last tail

(* Q2 *)
let rec last_two (lst : 'a list) : ('a * 'b) option =
  match lst with
  | [x ; y] -> Some (x , y)
  | [] -> None
  | [_] -> None
  | _ :: rest -> last_two rest

(* Q3 *)
let rec nth (lst : 'a list) (idx : int) : 'a = 
  match (lst, idx) with
  | x::_ , 0 -> x
  | [], _ -> raise (Failure "nth")
  | _::list, idx -> nth list (idx-1)

(* Q4 *)
let rec length_aux (list : 'a list) (acc : int) : int =
  match list with
  | [] -> acc
  | _::rest -> length_aux rest (acc+1)

let length (list : 'a list) : int = length_aux list 0

(* Q5 *)
let rec reverse_aux (list: 'a list) (acc : 'a list) : 'a list =
  match list with
  | [] -> acc
  | x::rest -> reverse_aux rest (x::acc)

let reverse (list: 'a list) : 'a list = reverse_aux list []

(* Q6 *)
let palindrome (list: 'a list) : bool = list = reverse list

(* Q7 *)
type 'a node =
  | One of 'a 
  | Many of 'a node list

let rec flatten_aux (list: 'a node list) (acc: 'a list) : 'a list =
  match list with
  | [] -> acc
  | (One x) :: rest -> flatten_aux rest (x::acc)
  | Many inside::rest -> flatten_aux rest (flatten_aux inside acc) 

let flatten (list: 'a node list) : 'a list = reverse (flatten_aux list [])

(* Q8 *)
let rec compress (list: 'a list) : 'a list = 
  match list with
  | [] -> []
  | [singular] -> [singular]
  | x::(y::_ as t) -> if x = y then compress t else x::compress t 

(* Q9 *)
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
let rec encode_aux (list: 'a list) (cumulator: (int * 'a) list) (count: int) : (int * 'a) list = 
  match list with
  | [] -> cumulator
  | [x] -> (count, x)::cumulator
  | x::(y::_ as t) -> if x = y then encode_aux t cumulator (count+1) else encode_aux t ((count, x)::cumulator) 1 

let encode (list: 'a list): (int * 'a) list = reverse (encode_aux list [] 1)

(* Q11 *)
type 'a rle =
  | One of 'a
  | Many of int * 'a

let rec encode_2_aux list cumulator count  =
  match list with
  | [] -> cumulator
  | [x] -> if count == 1 then One x::cumulator else Many (count, x)::cumulator 
  | x::(y::_ as t) -> 
      if x = y then 
          encode_2_aux t cumulator (count+1) 
      else
        if count = 1 then 
          encode_2_aux t (One x::cumulator) 1 
        else 
          encode_2_aux t (Many (count, x)::cumulator) 1

let encode_2 (list: 'a list): 'a rle list = reverse(encode_2_aux list [] 1)


(* Q12 *)
let rec append_many (char: 'a) (cumulator: 'a list) (count: int): 'a list =
  if count = 0 then cumulator else append_many char (char::cumulator) (count-1)

let rec decode_aux (list: 'a rle list) (acc: 'a list): 'a list = 
  match list with
  | [] -> acc
  | Many (n, char) :: rest -> decode_aux rest (append_many char acc n)
  | One char :: rest -> decode_aux rest (char::acc)

let decode (list: 'a rle list): 'a list = reverse (decode_aux list [])

(* Q13 *)
let duplicate (list: 'a list): 'a list = 
  let rec dup_aux (list: 'a list) (acc: 'a list) : 'a list = 
    match list with
    | [] -> acc
    | x::rest -> dup_aux rest (x::x::acc) in
  reverse (dup_aux list [])

(* Q14 *)
let rec rep_aux list acc times = 
  match list with
  | [] -> acc
  | x::rest -> rep_aux rest (append_many x acc times) times 
let replicate (list: 'a list) (times: int): 'a list = 
  match list with 
  | [] -> []
  | _::_ -> reverse @@ rep_aux list [] times

(* Q15 *)
(* This version is not tail recursive, it can be made to be though... *)
let drop (list: 'a list) (num: int): 'a list = 
  let rec drop_aux list idx = 
    match list with
    | [] -> []
    | x::rest -> if idx = num then drop_aux rest 1 else x::(drop_aux rest (idx+1))
  in drop_aux list 1

(* Q16 *)
let split (list: 'a list) (indx: int) : 'a list * 'a list = 
  let rec aux list acc i = 
    match list with
    | [] -> (reverse acc, [])
    | x::rest -> if i = 0 then (reverse (acc), x::rest) else aux rest (x::acc) (i-1)
  in 
  aux list [] indx

(* Q17 *)
let slice (list: 'a list) (start_i: int) (end_i: int) : 'a list =
  let rec aux (list: 'a list) (indx: int) (acc: 'a list) =
    match list with
    | [] -> acc
    | x::rest -> if indx <= end_i && indx >= start_i then 
                  aux rest (indx+1) (x::acc)
                 else if indx > end_i then acc else aux rest (indx+1) acc
    in reverse (aux list 0 [])

(* Q18 *)
let rotate (list: 'a list) (places: int): 'a list = 
  let shifts = if length list = 0 then 0 else places mod (length list) in
    if shifts = 0 then list else
      let left, right = split list shifts  in
        right @ left

(* Q19 *)
let remove_at (indx: int) (list: 'a list): 'a list = 
  let rec aux (list: 'a list) (num: int): 'a list =
    match list with
    | [] -> []
    | x::rest -> if num = indx then aux rest (num+1) else x::aux rest (num+1)in 
  aux list 0 

(* Q20 *)
let insert_at (ele: 'a) (indx: int) (list: 'a list): 'a list = 
  


let () = 
  let _ = last ["a" ; "b" ; "c"] in 
  let _ = last_two ["a" ; "b" ; "c"] in 
  let _ = nth ["a" ; "b" ; "c"] 2 in 
  let _ = length [] in
  let _ = reverse ["a" ; "b" ; "c"] in
  let _ = palindrome ["a", "b", "a"] in
  let _ = flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]] in
  let _ = compress [1;1;1;1;1;2;3;4;5;6;6;6;7;7] in
  let _ = pack [1;1;1;1;1;2;3;4;5;6;6;6;7;7] in
  let _ = encode_2 [1;1;1;1;1;2;3;4;5;6;6;6;7;7] in
  let _ = encode [1;1;1;1;1;2;3;4;5;6;6;6;7;7] in
  let _ = decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")] in
  let _ = duplicate ["a" ; "b" ; "c" ; "d"] in
  let _ = replicate ["a" ; "b" ; "c" ; "d"] 5 in
  let _ = drop ["a" ; "b" ; "c" ; "d" ; "a" ; "b" ; "c" ; "d" ; "a" ; "b" ; "c" ; "d" ; "a" ; "b" ; "c" ; "d"] 6 in
  let _ = split ["a" ; "b" ; "c" ; "d" ; "a" ; "b" ; "c" ; "d" ; "a" ; "b" ; "c" ; "d" ; "a" ; "b" ; "c" ; "d"] 1 in
  let _ = slice [1;1;1;1;1;2;3;4;5;6;6;6;7;7] 3 7 in 
  let _ = rotate [1;1;1;1;1;2;3;4;5;6;6;6;7;7] 22 in 
  let res = remove_at 6 [1;1;1;1;1;2;3;4;5;6;6;6;7;7] in 
      printList res

