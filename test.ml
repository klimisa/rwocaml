let x = 1;;
let ratio x y =
  Float.of_int x /. Float.of_int y;;

let even x =
  x mod 2 = 0;;

let sum_if_true (test : int -> bool) (first : int) (second : int) =
    (if test first then first else 0)
    + (if test second then second else 0)
;;

let first_if_true test x y =
  if test x then x else y;;

let long_string s = String.length s > 6;;
let big_number x = x > 3;;

let is_a_multiple x y =
  x mod y = 0;;

let a_tuple = (3,"three");;

let (x,y) = a_tuple;;

let distance (x1,y1) (x2,y2) =
  sqrt ((x1 -. x2) ** 2. +. (y1 -. y2) ** 2.);;

let languages = ["OCaml";"Perl";"C"];;

let numbers = [1;"two";2];;

List.map ~f:String.length languages;;

let my_favorite_language (my_favorite :: the_rest) =
  my_favorite
;;

let my_favorite_language languages =
  match languages with
  | x::xs -> x
  | [] -> "OCaml" (* A good example *)
;;

let rec sum l =
  match l with
  | [] -> 0
  | x::xs -> x + sum xs
;;

let rec destutter list =
  match list with
  | [] -> []
  | [x] -> [x]
  | x1 :: x2 :: xs ->
    if x1 = x2 then destutter (x2 :: xs)
    else x1 :: destutter (x2 :: xs)
;;

let log_entry maybe_time message =
  let time =
    match maybe_time with
    | None -> Time.now ()
    | Some x -> x
  in
  Time.to_sec_string time Time.Zone.local ^ "--" ^ message
;;

let x = 7 in
  x + x
;;

let x = 7 in
let y = x * x in
  x + y
;;

type point2d = {
  x : float; y : float
};;

let magnitude { x; y } =
  sqrt (x ** 2. +. y ** 2.)
;;

let distance v1 v2 =
  magnitude {x = v1.x -. v2.x; y = v2.y -. v2.y};;

type circle_desc = { center: point2d; radius: float }
type rect_desc = { lower_left: point2d; width: float; height: float }
type segment_desc = { endpoint1: point2d; endpoint2: point2d } ;;

type scene_elements =
  | Circle  of circle_desc
  | Rect    of rect_desc
  | Segment of segment_desc
;;

let is_inside_scene_element point scene_element =
  match scene_element with
  | Circle { center; radius } ->
    distance center point < radius
  | Rect { lower_left; width; height } ->
    point.x > lower_left.x && point.x < lower_left.x +. width
    && point.y > lower_left.y && point.y < lower_left.y +. height
  | Segment { endpoint1; endpoint2 } -> false
;;

let is_inside_scene point scene =
  List.exists scene
    ~f:(fun el -> is_inside_scene_element point el)
;;

is_inside_scene {x=3.;y=7.}
  [ Circle {center = {x=4.;y= 4.}; radius = 0.5 } ];;

is_inside_scene {x=3.;y=7.}
  [ Circle {center = {x=4.;y= 4.}; radius = 5. } ];;

let numbers = [|1;2;3;4|];;
numbers.(2) <- 4;;

type running_sum =
  { mutable sum: float;
    mutable sum_sq: float;
    mutable samples: int;
  }
;;

let mean rsum = rsum.sum /. float rsum.samples
let stdev rsum =
    sqrt ((rsum.sum_sq) /. float rsum.samples)
          -. ((rsum.sum /. float rsum.samples) ** 2.)
;;


let create () = { sum = 0.; sum_sq = 0. ; samples = 0 };;

let update rsum x =
  rsum.samples  <- rsum.samples + 1;
  rsum.sum      <- rsum.sum     +. x;
  rsum.sum_sq   <- rsum.sum_sq  +. x *. x
;;

let sum list =
  let sum = ref 0 in
  List.iter list ~f:(fun x -> sum := !sum + x);
  !sum
;;

let permute array =
  let length = Array.length array in
  for i = 0 to length - 2 do
    let j = i + Random.int (length - i) in
    let tmp = array.(i) in
    array.(i) <- array.(j);
    array.(j) <- tmp
  done
;;
