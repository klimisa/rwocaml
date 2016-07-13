let languages = "OCaml,Perl,C++,C";;

let dashed_languages =
  let languages = String.split languages ~on:',' in
    String.concat ~sep:"-" languages
;;

let area_of_ring inner_radius outer_radius =
  let pi = acos (-1.) in
  let area_of_circle r = pi *. r *. r in
  area_of_circle outer_radius -. area_of_circle inner_radius
;;

let (ints,strings) = List.unzip [(1,"one"); (2,"two"); (3,"three")];;

let upcase_first_entry line =
  match String.split  ~on:',' line with
  | [] -> assert false (* String.split returns at least one element *)
  | first::rest ->
    String.concat ~sep:"," (String.uppercase first :: rest)
;;

(fun x -> x + 1);;

List.map ~f:(fun x -> x + 1) [1;2;3];;

let increments = [(fun x -> x + 1);(fun x -> x + 2)];;
List.map ~f:(fun g -> g 5) increments;;

let plusone = (fun x -> x + 1);;

let abs_diff x y = abs (x - y);;

let dist_from_3 = abs_diff 3;;

let abs_diff = (fun x y -> abs (x - y));;

let abs_diff (x,y) = abs (x - y);;

let rec find_first_stutter list =
  match list with
  | [] | [_]  -> None
  | x :: x' :: xs ->
    if (x = x') then Some x
    else find_first_stutter (x' :: xs)
;;

let rec is_even x =
  if x = 0 then true else is_odd (x - 1)
and is_odd x =
  if x = 0 then false else is_even (x - 1)
;;

List.map ~f:((+) 2) [1;2;3;4];;

let (+!) (x1,x2) (y1,y2) = (x1+x2,y1+y2);;

let ( *** ) x y = (x ** y) ** y;;

let (|>) x f = f x;;
let (^>) x f = f x;;

let path = "/usr/bin:/usr/local/bin:/bin:/sbin";;
String.split ~on:':' (Sys.getenv_exn "PATH")
|> List.dedup ~compare:String.compare
|> List.iter ~f:print_endline
;;

(((String.split ~on:':' "/usr/bin:/usr/local/bin:/bin:/sbin")
  |> List.dedup ~compare:String.compare )
    |> List.iter ~f:print_endline)
;;

String.split ~on:':' "/usr/bin:/usr/local/bin:/bin:/sbin"
|> (List.dedup ~compare:String.compare
      |> List.iter ~f:print_endline)
;;

let some_or_zero = function
  | Some x -> x
  | None -> 0
;;

let some_or_default default = function
| Some x -> x
| None -> default
;;

let ratio num ~demon = float num /. float demon;;


let concat ?sep x y =
  let sep = match sep with None -> "" | Some x -> x in
  x ^ sep ^ y
;;

let concat ?(sep="") x y =   x ^ sep ^ y;;

let uppercase_concat ?sep a b =
  concat sep (String.uppercase a) b
;;

let colon_concat = concat ~sep:":";;
colon_concat "a" "b";;

let prepend_pound = concat "# ";;

let concat x ?(sep="") y = x ^ sep ^ y;;
