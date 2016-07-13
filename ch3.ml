let l = 1 :: 2 :: 3 :: [];;

let m = 0 :: l;;

let rec sum l =
  match l with
  | [] -> 0
  | x :: xs -> x + sum xs
;;

let rec drop_value l to_drop =
  match l with
  | [] -> []
  | x :: xs ->
    if x = to_drop then drop_value xs to_drop
    else x :: (drop_value xs to_drop)
;;

let rec drop_zero l =
  match l with
  | [] -> []
  | 0  :: tl -> drop_zero tl
  | hd :: tl -> hd :: drop_zero tl
;;

let plus_one_match x =
  match x with
  | 0 -> 1
  | 1 -> 2
  | 2 -> 3
  | _ -> x + 1
;;

let plus_one_if x =
  if      x = 0 then 1
  else if x = 1 then 2
  else if x = 1 then 3
  else x + 1
;;

#require "core_bench";;
open Core_bench.Std;;

let main () =
  let x = 10 in
  Command.run (Bench.make_command [
    Bench.Test.create ~name:"plus_one_match"
    (fun () -> ignore (plus_one_match x));
    Bench.Test.create ~name:"plus_one_if"
    (fun () -> ignore (plus_one_if x))
  ]);;

  let rec sum_if l =
  if List.is_empty l then 0
  else List.hd_exn l + sum_if (List.tl_exn l)
  ;;

let bench_list () =
  let l = (List.range 0 1000) in
  Command.run (Bench.make_command [
    Bench.Test.create ~name:"sum"
      (fun () -> ignore (sum l));
    Bench.Test.create ~name:"sum_if"
      (fun () -> ignore (sum_if l));
  ]);;

let () = main ();;

printf "%s\n"
  (render_table
      ["language";"architect";"first release"]
      [ ["Lisp" ;"John McCarthy" ;"1958"] ;
        ["C" ;"Dennis Ritchie";"1969"] ;
        ["ML" ;"Robin Milner" ;"1973"] ;
        ["OCaml";"Xavier Leroy" ;"1996"] ;
      ]);;

List.map ~f:String.length ["Hello"; "World!"];;

List.map2_exn ~f:Int.max [1;2;3] [3;2;1];;

let l = [1;2;3;4];;

List.fold ~init:0 ~f:(+) l;;

List.fold ~init:[] ~f:(fun accum x -> x :: accum) l;;

let max_width header rows =
  let lengths l = List.map ~f:String.length l in
  List.fold rows
    ~init:(lengths header)
    ~f:(fun acc row -> List.map2_exn ~f:Int.max acc (lengths row))
;;

let widths = max_width
    ["language";"architect";"first release"]
    [ ["Lisp" ;"John McCarthy" ;"1958"] ;
      ["C" ;"Dennis Ritchie";"1969"] ;
      ["ML" ;"Robin Milner" ;"1973"] ;
      ["OCaml";"Xavier Leroy" ;"1996"] ;
    ]
;;

let render_seperator widths =
  let pieces = List.map ~f:(fun w -> String.make (w + 2) '-') widths in
  "|" ^ String.concat ~sep:"+" pieces ^ "|"
;;

let pad s length =
  " " ^ s ^ (String.make (length - String.length s + 1) ' ')
;;

let render_row row widths =
  let padded = List.map2_exn row widths ~f:pad in
  "|" ^ (String.concat ~sep:"|" padded) ^ "|"
;;

let render_table header rows =
  let widths = max_width header rows in
  String.concat ~sep:"\n"
      (render_row header widths
        :: render_seperator widths
        :: List.map rows ~f:(fun row -> render_row row widths)
      )
;;

render_table
      ["language";"architect";"first release"]
      [ ["Lisp" ;"John McCarthy" ;"1958"] ;
        ["C" ;"Dennis Ritchie";"1969"] ;
        ["ML" ;"Robin Milner" ;"1973"] ;
        ["OCaml";"Xavier Leroy" ;"1996"] ;
      ];;


List.filter_map (Sys.ls_dir "/var/log") ~f:(fun fname ->
  match String.rsplit2 ~on:'.' fname with
  | None | Some ("",_) -> None
  | Some (_, ext) ->
    Some ext)
|> List.dedup;;


let is_ocaml_source s =
  match String.rsplit2 s ~on:'.' with
  | Some (_,("ml"|"mli")) -> true
  | _ -> false
;;

let (ml_files,other_files) =
  List.partition_tf (Sys.ls_dir "/home/klimisa/Development/rw-ocaml") ~f:is_ocaml_source;;

List.append [1;2;3] [4;5;6];;

[1;2;3] @ [4;5;6];;

List.concat [[1;2;3];[4;5];[6]];;

let rec ls_rec s =
  if Sys.is_file_exn ~follow_symlinks:true s
  then [s]
  else
    Sys.ls_dir s
    |> List.map ~f:(fun sub -> ls_rec (s ^/ sub))
    |> List.concat;;

let rec ls_rec s =
  if Sys.is_file_exn ~follow_symlinks:true s
  then [s]
  else
    Sys.ls_dir s
    |> List.concat_map ~f:(fun sub -> ls_rec (s ^/ sub))
;;

let rec length = function
| [] -> 0
| _ :: tl -> 1 + length tl
;;

let make_list n = List.init n ~f:((fun  x -> x));;

let l1 = make_list 10;;
let l1 = make_list 10_000_000;;

let rec length_plus_n l n =
  match l with
  | [] -> n
  | _ :: tl -> length_plus_n tl (n + 1);;

let length l = length_plus_n l 0;;

let rec destutter = function
  | [] | [_]  as l -> l
  | hd :: (hd' :: _ as tl) when (hd = hd') -> destutter tl
  | hd :: tl -> hd :: destutter tl;;
