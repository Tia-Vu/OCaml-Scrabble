type t = { mutable letters : char list }

exception DrawFromEmpty

(*TODO: should we get rid of formatting...?*)

(** Reference from https://scrabble.hasbro.com/en-us/faq*)
let init_pool () =
  let _ = Random.self_init () in
  {
    letters =
      Yojson.Basic.from_file "pool.json"
      |> Yojson.Basic.Util.to_list
      |> List.map (fun x -> (Yojson.Basic.Util.to_string x).[0]);
  }

(* (** [remove_nth_tr] is the tail recursion version of [remove_nth]*)
   let rec remove_nth_tr front (h :: back) = function | 0 -> List.rev
   front @ back | n -> if back = [] then failwith "n out of bounds" else
   remove_nth_tr (h :: front) back (n - 1)

   let remove_nth n lst = remove_nth_tr [] lst n*)

(** [remove_nth n lst] gives a list with the [n]th element removed.*)
let remove_nth n lst =
  let rec remove_nth_aux front back = function
    | 0 -> (
        match back with
        | [] -> List.rev front
        | h :: t -> List.rev front @ t )
    | x -> (
        match back with
        | [] -> failwith "n out of bounds"
        | h :: t -> remove_nth_aux (h :: front) t (x - 1) )
  in
  remove_nth_aux [] lst n

let size pool = List.length pool.letters

let is_empty pool = size pool = 0

let draw_letter pool =
  if is_empty pool then raise DrawFromEmpty else ();
  let i = Random.int (List.length pool.letters) in
  let drawn_letter = List.nth pool.letters i in
  pool.letters <- remove_nth i pool.letters;
  drawn_letter

let to_string pool = string_of_int (size pool)
