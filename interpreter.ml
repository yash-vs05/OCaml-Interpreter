type types =
|StringC of string
|Error of string
|Bool of string
|Int of int
|Name of string
|Unit of string

let valid_name (c : char) : bool =
((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || (c = '_'))

let fst_valid (c : char) : bool =
  ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c = '_'))


let valid_int (c : char) : bool =
  (c >= '0' && c <= '9')


let fst_valid_int (c : char) : bool =
  ((c >= '0' && c <= '9') || (c = '-'))


let split (line : string) : (string * string) =
  let idx = String.index_from_opt line 0 ' ' in
  match idx with
  |None -> line,""
  |Some x -> (String.sub line 0 x),(String.sub line (x+1) ((String.length line) - 1 - x))

let arg_det (arg : string) : (types) =
  match arg with
  | "" -> Error ":error:"
  |":true:" -> Bool arg
  |":false:" -> Bool arg
  |":unit:" -> Unit arg
  |":error:" -> Error arg
  | _ ->
    if ((String.starts_with ~prefix:"\"" arg) && (String.ends_with ~suffix:"\"" arg)) then
      StringC (String.sub arg 1 (String.length arg - 2))
    else
      let num = int_of_string_opt arg in
      begin match num with
      |Some x ->
        if (fst_valid_int (String.get arg 0)) && (String.for_all valid_int (String.sub arg 1 (String.length arg - 1))) then
          Int (x)
        else
          Error ":error:"
      |None ->
        begin if (not(arg = "_")) then
         begin if ((fst_valid (String.get arg 0)) && (String.for_all valid_name arg)) then
          Name arg
         else
          Error ":error:"
         end
        else
           Error ":error:"
        end
      end

let push (arg : types) (stack : types list) : (types list) =
  arg :: stack

let pop (stack : types list) : (types list) =
match stack with
| [] -> Error ":error:" :: stack
| h::t -> t

let arithmetic (f : int->int->int) (zero : bool) (stack : types list) : (types list) =
match stack with
| [] -> Error ":error:" :: stack
| e1::e2::t ->
  begin match e1 with
  |Int x ->
    begin match e2 with
    |Int y ->
      if zero && x = 0 then
        Error ":error:" :: stack
    else
        (Int (f y x)) :: t
    |_ -> Error ":error:" :: stack
    end
  |_ -> Error ":error:" :: stack
  end
| _ -> Error ":error:" :: stack

let neg (stack : types list) : (types list) =
match stack with
| [] -> Error ":error:" :: stack
| h::t ->
  match h with
  |Int x -> Int (-x) :: t
  |_ -> Error ":error:" :: stack

let swap (stack : types list) : (types list) =
match stack with
| [] -> Error ":error:" :: stack
| e1::e2::t -> e2::e1::t
| _ -> Error ":error:" :: stack
let toString (stack : types list) : (types list) =
match stack with
| [] -> Error ":error:" :: stack
| h::t ->
  match h with
  |Int x -> StringC (string_of_int x) :: t
  |Bool x |StringC x|Name x |Unit x |Error x -> StringC (x) :: t

let println (stack : types list) (output : string list) : (string list * types list) =
match stack with
| [] -> output, Error ":error:" :: stack
| h::t ->
  match h with
  |StringC x -> x::output, t
  |_ -> output, Error ":error:" :: stack

let com_det (line : string) (output : string list) (stack : types list) : (string list * types list) =
let com_lst = split line in
match com_lst with
| (a,b) ->
  match String.trim a with
  | "quit" -> output, pop stack
  | "push" -> output, (push (arg_det(String.trim b)) stack)
  | "pop" -> output, pop stack
  | "add" -> output, arithmetic ( + ) false stack
  | "sub" -> output, arithmetic ( - ) false stack
  | "mul" -> output, arithmetic ( * ) false stack
  | "div" -> output, arithmetic ( / ) false stack
  | "rem" -> output, arithmetic ( mod ) false stack
  | "neg" -> output, neg stack
  | "swap" -> output, swap stack
  | "toString" -> output, toString stack
  | "println" -> println stack output
  | _ -> output, stack

let rec interpreter_begin (ls_str : string list) (output : string list) (stack : types list) : (string list) =
match ls_str with
| [] -> output
| h :: t ->
  match (com_det h output stack) with
  |(nout, nstack) -> interpreter_begin t nout nstack

let interpreter (in_file : string) (out_file : string) : unit =
  let input_lines ic =
    let rec loop acc =
      match input_line ic with
      | line ->
          loop (line :: acc)
      | exception End_of_file ->
          List.rev acc
    in
    loop []
  in
  let ls_str = In_channel.with_open_text in_file input_lines in
  let results = interpreter_begin ls_str [] [] in
  let output_strings ls oc = List.iter (Printf.fprintf oc "%s\n") ls in
  Out_channel.with_open_text out_file (output_strings (List.rev results))

  (* let _ = interpreter "input.txt" "output.txt" *)