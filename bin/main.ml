(* Tail of a List *)
let rec last list =
  match list with
  | [] -> None
  | [ item ] -> Some item
  | _ :: tail -> last tail
;;

let _ = last [ "a"; "b"; "c"; "d" ]
let _ = last []

(* Last Two Elements of a List *)
let rec last_two list =
  match list with
  | [] -> None
  | [ x; y ] -> Some (x, y)
  | _ :: tail -> last_two tail
;;

let _ = last_two [ "a"; "b"; "c"; "d" ]
let _ = last_two [ "a" ]

(* N'th Element of a List *)
let rec nth list index =
  match list with
  | [] -> None
  | head :: tail -> if index = 0 then Some head else nth tail (index - 1)
;;

let _ = nth [ "a"; "b"; "c"; "d"; "e" ] 2
let _ = nth [ "a" ] 2

(* Length of a List *)
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

(* Reverse a List *)
let rev list =
  let rec rev list result =
    match list with
    | [] -> result
    | head :: tail -> rev tail (head :: result)
  in
  rev list []
;;

let _ = rev [ "a"; "b"; "c" ]

(* Palindrome *)
let is_palindrome list = list = rev list
let _ = is_palindrome [ "x"; "a"; "m"; "a"; "x" ]
let _ = not (is_palindrome [ "a"; "b" ])

(* Flatten a List *)
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
