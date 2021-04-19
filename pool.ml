type t = { mutable letters : char list }

exception DrawFromEmpty

(*TODO: should we get rid of formatting...?*)

(** Reference from https://scrabble.hasbro.com/en-us/faq*)
let init_pool () =
  {
    letters =
      [
        'a';
        'a';
        'a';
        'a';
        'a';
        'a';
        'a';
        'a';
        'a';
        'b';
        'b';
        'c';
        'c';
        'd';
        'd';
        'd';
        'd';
        'e';
        'e';
        'e';
        'e';
        'e';
        'e';
        'e';
        'e';
        'e';
        'e';
        'e';
        'e';
        'f';
        'f';
        'g';
        'g';
        'g';
        'h';
        'h';
        'i';
        'i';
        'i';
        'i';
        'i';
        'i';
        'i';
        'i';
        'i';
        'j';
        'k';
        'l';
        'l';
        'l';
        'l';
        'm';
        'm';
        'n';
        'n';
        'n';
        'n';
        'n';
        'n';
        'o';
        'o';
        'o';
        'o';
        'o';
        'o';
        'o';
        'o';
        'p';
        'p';
        'q';
        'r';
        'r';
        'r';
        'r';
        'r';
        'r';
        's';
        's';
        's';
        's';
        't';
        't';
        't';
        't';
        't';
        't';
        'u';
        'u';
        'u';
        'u';
        'v';
        'v';
        'w';
        'w';
        'x';
        'y';
        'y';
        'z';
      ];
  }

(** [remove_nth_tr] is the tail recursion version of [remove_nth]*)
let rec remove_nth_tr front (h :: back) = function
  | 0 -> List.rev front @ back
  | n -> remove_nth_tr (h :: front) back (n - 1)

(** [remove_nth n lst] gives a list with the [n]th element removed.*)
let remove_nth n lst = remove_nth_tr [] lst n

let size pool = List.length pool.letters

let is_empty pool = size pool = 0

let draw_letter pool =
  if is_empty pool then raise DrawFromEmpty else ();
  let i = Random.int (List.length pool.letters) in
  let drawn_letter = List.nth pool.letters i in
  pool.letters <- remove_nth i pool.letters;
  drawn_letter
