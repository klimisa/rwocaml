open Core.Std

(** A collection of string frequency counts *)
type t

(** The empty set of frequency counts *)
val empty : t

(** Bump the frequency count of a given string *)
val touch : t -> string -> t

(** Converts the set of frequency to an associative list.
A string shows up at the most once, and the counts are >=1 *)
val to_list : t -> (string * int) list
