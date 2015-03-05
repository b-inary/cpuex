
(* fundamental functions *)
let (<<) f g x = f (g x)
let (>>) f g x = g (f x)
let id x = x
let const x y = x

(* exceptions *)
let invalid_argf fmt = Printf.ksprintf invalid_arg fmt
let failwithf fmt = Printf.ksprintf failwith fmt

(* option module *)
module Option = struct
  let is_none x = x = None
  let is_some x = not (is_none x)
  let get = function
    | None -> invalid_arg "Option.get"
    | Some x -> x
  let map f = function
    | None -> None
    | Some x -> Some (f x)
  let bind = function
    | None -> const None
    | Some x -> fun f -> f x
  let default x = function
    | None -> x
    | Some y -> y
  let map_default f x = function
    | None -> x
    | Some y -> f y
end

(* extended list module *)
module List = struct
  include List
  let rec take = function
    | 0 -> const []
    | n -> function
      | [] -> invalid_arg "List.take"
      | x :: xs -> x :: take (n - 1) xs
  let rec drop = function
    | 0 -> id
    | n -> function
      | [] -> invalid_arg "List.drop"
      | _ :: xs -> drop (n - 1) xs
  let sum = fold_left (+) 0
  let min = function
    | [] -> invalid_arg "List.min"
    | x :: xs -> fold_left min x xs
  let max = function
    | [] -> invalid_arg "List.max"
    | x :: xs -> fold_left max x xs
end

let rec range x y =
  if x >= y then [] else
  x :: range (x + 1) y
