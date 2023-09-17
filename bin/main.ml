(* 1. Tail of a List *)
let rec last list =
  match list with
  | [] -> None
  | [ item ] -> Some item
  | _ :: tail -> last tail
;;

let _ = last [ "a"; "b"; "c"; "d" ]
let _ = last []

(* 2. Last Two Elements of a List *)
let rec last_two list =
  match list with
  | [] -> None
  | [ x; y ] -> Some (x, y)
  | _ :: tail -> last_two tail
;;

let _ = last_two [ "a"; "b"; "c"; "d" ]
let _ = last_two [ "a" ]

(* 3. N'th Element of a List *)
let rec nth list index =
  match list with
  | [] -> None
  | head :: tail -> if index = 0 then Some head else nth tail (index - 1)
;;

let _ = nth [ "a"; "b"; "c"; "d"; "e" ] 2
let _ = nth [ "a" ] 2

(* 4. Length of a List *)
let length list =
  let rec length list result =
    match list with
    | [] -> result
    | _ :: tail -> length tail (result + 1)
  in
  length list 0
;;

let _ = length [ "a"; "b"; "c" ]
let _ = length []

(* 5. Reverse a List *)
let rev list =
  let rec rev list result =
    match list with
    | [] -> result
    | head :: tail -> rev tail (head :: result)
  in
  rev list []
;;

let _ = rev [ "a"; "b"; "c" ]

(* 6. Palindrome *)
let is_palindrome list = list = rev list
let _ = is_palindrome [ "x"; "a"; "m"; "a"; "x" ]
let _ = not (is_palindrome [ "a"; "b" ])

(* 7. Flatten a List *)
type 'a node =
  | One of 'a
  | Many of 'a node list

let flatten list =
  let rec flatten list result =
    match list with
    | [] -> result
    | One head :: tail -> flatten tail (head :: result)
    | Many head :: tail -> flatten tail (flatten head result)
  in
  let result = flatten list [] in
  rev result
;;

let _ =
  flatten [ One "a"; Many [ One "b"; Many [ One "c"; One "d" ]; One "e" ] ]
;;

(* 8. Eliminate Duplicates *)
let compress list =
  let rec compress list result =
    match list with
    | [] -> result
    | [ last ] -> last :: result
    | head :: (second :: _ as tail) ->
      if head = second
      then compress tail result
      else compress tail (head :: result)
  in
  let result = compress list [] in
  rev result
;;

let _ =
  compress
    [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
;;

(* 9. Pack Consecutive Duplicates *)
let pack list =
  let rec pack list outer inner =
    match list with
    | [] -> outer
    | [ last ] -> (last :: inner) :: outer
    | head :: (second :: _ as tail) ->
      if head = second
      then pack tail outer (head :: inner)
      else pack tail ((head :: inner) :: outer) []
  in
  let result = pack list [] [] in
  rev result
;;

let _ =
  pack [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e" ]
;;

(* 10. Run-Length Encoding *)
let encode list =
  let rec encode list result count =
    match list with
    | [] -> result
    | [ last ] -> (count + 1, last) :: result
    | head :: (second :: _ as tail) ->
      if head = second
      then encode tail result (count + 1)
      else encode tail ((count + 1, head) :: result) 0
  in
  let result = encode list [] 0 in
  rev result
;;

let _ =
  encode
    [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
;;

(* 11. Modified Run-Length Encoding *)
type 'a rle =
  | One of 'a
  | Many of int * 'a

let mod_encode list =
  let create_rle count item =
    if count = 1 then One item else Many (count, item)
  in
  let rec mod_encode list result count =
    match list with
    | [] -> result
    | [ last ] -> create_rle (count + 1) last :: result
    | head :: (second :: _ as tail) ->
      if head = second
      then mod_encode tail result (count + 1)
      else mod_encode tail (create_rle (count + 1) head :: result) 0
  in
  let result = mod_encode list [] 0 in
  rev result
;;

let _ =
  mod_encode
    [ "a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e" ]
;;
