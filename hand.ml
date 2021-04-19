type t = char list

let rec draw_nletters pool n hand =
  match n with
  | 0 -> hand
  | n ->
      if Pool.is_empty pool then hand
      else
        let letter = Pool.draw_letter pool in
        draw_nletters pool (n - 1) (letter :: hand)

let has_word = failwith "unimplemented"

let spend_word = failwith "unimplemented"
