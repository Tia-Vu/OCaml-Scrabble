type t = { mutable letters : char list }

exception DrawFromEmpty

let init_pool () =
  let _ = Random.self_init () in
  {
    letters =
      Yojson.Basic.from_file "pool.json"
      |> Yojson.Basic.Util.to_list
      |> List.map (fun x -> (Yojson.Basic.Util.to_string x).[0]);
  }

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

let to_string pool = string_of_int (size pool) [@@coverage off]
