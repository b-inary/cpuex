
(* Exceptions *)
let invalid_argf fmt = Printf.ksprintf invalid_arg fmt
let failwithf fmt = Printf.ksprintf failwith fmt

(* Fundamental functions *)
let compose f g x = f (g x)
let const x y = x

(* Input library [required: OCaml 4.02.0] *)
let input_all ic =
  let buf_size = 8192 in
  let rec loop acc size buf ofs =
    let n = input ic buf ofs (buf_size - ofs) in
    if n = 0 then (* end of input *)
      let res = Bytes.create size in
      let blit i buf = Bytes.blit buf 0 res (i * buf_size) buf_size in
      List.iteri blit (List.rev acc);
      Bytes.blit buf 0 res (size - ofs) ofs;
      Bytes.to_string res
    else if ofs + n = buf_size then
      loop (buf :: acc) (size + n) (Bytes.create buf_size) 0
    else
      loop acc (size + n) buf (ofs + n) in
  loop [] 0 (Bytes.create buf_size) 0

(* String map *)
module M = struct
  include Map.Make (String)
  let add_list keys data t =
    List.fold_left2 (fun t key datum -> add key datum t) t keys data
end

