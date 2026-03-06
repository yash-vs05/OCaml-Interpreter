type types =
  |StringC of string
  |Error of string
  |Bool of string
  |Int of int
  |Name of string
  |Unit of string

type ar_flag =
  |Div
  |Log
  |NDiv

let resolve (arg : types) (env_var : (string * types) list) : (types) =
  match arg with
  |Name n ->
    begin match (List.assoc_opt n env_var) with
    | None -> arg
    | Some v -> v
  end
  |_ -> arg

let split (line : string) : (string * string) =
  let idx = String.index_from_opt line 0 ' ' in
  match idx with
  |None -> line,""
  |Some x -> (String.sub line 0 x),(String.sub line (x+1) ((String.length line) - 1 - x))
let valid_name (c : char) : bool =
  ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || (c = '_'))

let fst_valid (c : char) : bool =
  ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c = '_'))

let arg_det (arg : string) : (types) =
  match arg with
  |":true:" -> Bool arg
  |":false:" -> Bool arg
  |":unit:" -> Unit arg
  |":error:" -> Error arg
  | _ ->
    if (String.starts_with ~prefix:"\"" arg) then
      StringC (String.sub arg 1 (String.length arg - 2))
    else
      begin match int_of_string_opt arg with
      |Some x ->
        Int (int_of_string arg)
      |None ->
         if ((fst_valid (String.get arg 0)) && (String.for_all valid_name arg)) then
          Name arg
        else
          Error ":error:"
        end

let rec push (arg : string) (stack : types list) : (types list) =
  if arg = "" then
    Error ":error:" :: stack
  else
    (arg_det (String.trim arg)) :: stack

let pop (stack : types list) : (types list) =
  match stack with
  | [] -> Error ":error:" :: stack
  | h::t -> t

let arithmetic (f : int->int->int) (flag : ar_flag) (stack : types list) (env_var : (string * types) list) : (types list) =
  match stack with
  | [] -> Error ":error:" :: stack
  | e1::e2::t ->
    let e1 = resolve e1 env_var in
    let e2 = resolve e2 env_var in
    begin match e1 with
    |Int x ->
      begin match e2 with
      |Int y ->
        begin match flag with
        |Div ->
          if x = 0 then
            Error ":error:" :: stack
        else
          (Int (f y x)) :: t
        |Log ->
          begin match (f y x) with
          | 0 -> Bool ":false:" :: t
          | 1 -> Bool ":true:" :: t
          | _ -> Error ":error:" :: stack
        end
        | _ -> (Int (f y x)) :: t
        end
      |_ -> Error ":error:" :: stack
      end
    |_ -> Error ":error:" :: stack
    end
  | _ -> Error ":error:" :: stack

let neg (stack : types list) (env_var : (string * types) list) : (types list) =
  match stack with
  | [] -> Error ":error:" :: stack
  | h::t ->
    let h = resolve h env_var in
    match h with
    |Int x -> Int (-x) :: t
    |_ -> Error ":error:" :: stack

let t_str_to_bool (s:string) : bool =
  (bool_of_string(String.sub s 1 (String.length s - 2)))

let t_bool_to_str (b:bool) : string =
  ":"^(string_of_bool(b))^":"

let not (stack : types list) (env_var : (string * types) list) : (types list) =
  match stack with
  | [] -> Error ":error:" :: stack
  | h::t ->
    let h = resolve h env_var in
    match h with
    |Bool x -> (Bool (t_bool_to_str(not(t_str_to_bool x)))) :: t
    |_ -> Error ":error:" :: stack

let bool_op (f : bool->bool->bool) (stack : types list) (env_var : (string * types) list) : (types list) =
  match stack with
  | [] -> Error ":error:" :: stack
  | e1::e2::t ->
    let e1 = resolve e1 env_var in
    let e2 = resolve e2 env_var in
    begin match e1 with
    |Bool x ->
      begin match e2 with
      |Bool y -> (Bool (t_bool_to_str(f (t_str_to_bool y) (t_str_to_bool x)))) :: t
      |_ -> Error ":error:" :: stack
    end
    |_ -> Error ":error:" :: stack
  end
  |_ -> Error ":error:" :: stack

let str_op (f : string->string->string) (stack : types list) (env_var : (string * types) list) : (types list) =
  match stack with
  | [] -> Error ":error:" :: stack
  | e1::e2::t ->
    let e1 = resolve e1 env_var in
    let e2 = resolve e2 env_var in
    begin match e1 with
    |StringC x ->
      begin match e2 with
      |StringC y -> StringC (f y x) :: t
      |_ -> Error ":error:" :: stack
    end
    |_ -> Error ":error:" :: stack
  end
  |_ -> Error ":error:" :: stack

let swap (stack : types list) : (types list) =
  match stack with
  | [] -> Error ":error:" :: stack
  | e1::e2::t -> e2::e1::t
  | _ -> Error ":error:" :: stack

let toString (stack : types list) (env_var : (string * types) list) : (types list) =
  match stack with
  | [] -> Error ":error:" :: stack
  | h::t ->
    (* let h = resolve h env_var in *)
    match h with
    |Int x -> StringC (string_of_int x) :: t
    |Bool x -> StringC (x) :: t
    |StringC x -> (StringC x) :: t
    |Name x -> StringC (x) :: t
    |Unit x -> StringC (x) :: t
    |Error x -> StringC (x) :: t

let println (stack : types list) (output : string list) (env_var : (string * types) list) : (string list * types list * (string * types) list) =
  match stack with
  | [] -> output, Error ":error:" :: stack, env_var
  | h::t ->
    (* let h = resolve h env_var in *)
    match h with
    |StringC x -> x::output, t, env_var
    |_ -> output, Error ":error:" :: stack, env_var

let logical (f : int -> int -> bool) (a : int) (b : int) : (int) =
  if f a b then 1 else 0

let if_op (stack : types list) (env_var : (string * types) list) : (types list) =
  match stack with
  | [] -> Error ":error:" :: stack
  | e1::e2::e3::t ->
    let e1 = resolve e1 env_var in
    let e2 = resolve e2 env_var in
    let e3 = resolve e3 env_var in
    begin match e3 with
    |Bool x ->
      begin match x with
      |":true:" -> e1 :: t
      |":false:" -> e2 :: t
      |_ -> Error ":error:" :: stack
    end
    |_ -> Error ":error:" :: stack
  end
  |_ -> Error ":error:" :: stack

let bind (stack : types list) (output : string list) (env_var : (string * types) list) : (string list * types list * (string * types) list) =
  match stack with
  | [] -> output, Error ":error:" :: stack, env_var
  | e1::e2::t ->
    let e1 = resolve e1 env_var in
    begin match e2 with
    |Name n ->
      begin match e1 with
      |StringC _ |Int _ |Unit _ |Bool _ -> output, Unit ":unit:" :: t, (n, e1) :: env_var
      |_ -> output, Error ":error:" :: stack, env_var
    end
    |_ -> output, Error ":error:" :: stack, env_var
  end
  | _ -> output, Error ":error:" :: stack, env_var

let com_det (line : string) (output : string list) (stack : types list) (env_var : (string * types) list) : (string list * types list * (string * types)list) =
  let com_lst = split line in
  match com_lst with
  | (a,b) ->
    match String.trim a with
    | "quit" -> output, pop stack, env_var
    | "push" -> output, push b stack, env_var
    | "pop" -> output, pop stack, env_var
    | "add" -> output, arithmetic ( + ) NDiv stack env_var, env_var
    | "sub" -> output, arithmetic ( - ) NDiv stack env_var, env_var
    | "mul" -> output, arithmetic ( * ) NDiv stack env_var, env_var
    | "div" -> output, arithmetic ( / ) Div stack env_var, env_var
    | "rem" -> output, arithmetic ( mod ) Div stack env_var, env_var
    | "neg" -> output, neg stack env_var, env_var
    | "swap" -> output, swap stack, env_var
    | "toString" -> output, toString stack env_var, env_var
    | "println" -> println stack output env_var
    | "lessThan" -> output, arithmetic (logical ( < )) Log stack env_var, env_var
    | "equal" -> output, arithmetic (logical ( = )) Log stack env_var, env_var
    | "and" -> output, bool_op ( && ) stack env_var, env_var
    | "or" -> output, bool_op ( || ) stack env_var, env_var
    | "not" -> output, not stack env_var, env_var
    | "cat" -> output, str_op ( ^ ) stack env_var, env_var
    | "if" -> output, if_op stack env_var, env_var
    | "bind" -> bind stack output env_var
    | _ -> output, stack, env_var

let rec interpreter_begin (ls_str : string list) (output : string list) (stack : types list) (env_var : (string * types) list) : (string list) =
  match ls_str with
  | [] -> output
  | h :: t ->
    match (com_det h output stack env_var) with
    |(nout, nstack, nenv_var) -> interpreter_begin t nout nstack nenv_var

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
  let results = interpreter_begin ls_str [] [] [] in
  let output_bools ls oc = List.iter (Printf.fprintf oc "%s\n") ls in
  Out_channel.with_open_text out_file (output_bools (List.rev results))