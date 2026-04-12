exception No_Environment
exception StackException
exception FunException

type types =
  |StringC of string
  |Error of string
  |Bool of string
  |Int of int
  |Name of string
  |Unit of string
  |Fun of (string * ((string * types) list) * string list)

type out = string list
type stack = types list
type stacks = stack list
type env = ((string * types) list) list
type info = (stacks * env * out * string list)
type closure = (string * (string * types) list * string list)

(* FLAG for arithmetic options: Div: Division, NDiv: No Division, Log: Integer Logical (lessThan, equal) *)
type ar_flag =
  |Div
  |NDiv
  |Log

(* HELPER for checking if there is atleast one character in a name *)
let one_char (c : char) : bool =
  ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z'))

(* HELPER for checking if character is valid for a name (ignore specifics for the first character) *)
let valid_char (c : char) : bool =
  ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9') || (c = '_'))

(* HELPER for checking if first character is valid for a name *)
let fst_valid (c : char) : bool =
  ((c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c = '_'))

(* HELPER for checking if string is valid for it to become an integer (ignores first character) *)
let valid_int (c : char) : bool =
  (c >= '0' && c <= '9')

(* HELPER for checking if first character of string is valid for it to become an integer *)
let fst_valid_int (c : char) : bool =
  ((c >= '0' && c <= '9') || (c = '-'))

(* HELPER for Arithmetic-Bool operations *)
let logical (f : int -> int -> bool) (a : int) (b : int) : (int) =
  if f a b then 1 else 0

(* HELPER for converting Bool argument strings to bool *)
let t_str_to_bool (s: string) : bool =
  (bool_of_string(String.sub s 1 (String.length s - 2)))

(* HELPER for converting bool to Bool argument strings *)
let t_bool_to_str (b: bool) : string =
  ":"^(string_of_bool(b))^":"

(* HELPER for resolving names *)
let resolve (arg : types) (env_var : env) : (types) =
  match arg with
  |Name n ->
    begin match env_var with
    | h :: t ->
      begin match (List.assoc_opt n h) with
      | None -> arg
      | Some v -> v
    end
    | _ -> raise No_Environment
  end
  |_ -> arg

(* HELPER for splitting command tags *)
let split (line : string) : (string * string) =
  let idx = String.index_from_opt line 0 ' ' in
  match idx with
  |None -> line,""
  |Some x -> (String.sub line 0 x),(String.sub line (x+1) ((String.length line) - 1 - x))

(* HELPER for determining argument from string *)
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
         begin if ((String.exists one_char arg) && (fst_valid (String.get arg 0)) && (String.for_all valid_char arg)) then
          Name arg
         else
          Error ":error:"
         end
        else
           Error ":error:"
        end
      end

(* Takes in an argument and puts it on top of the stack *)
let push (arg : types) (stack : stack) : (stack) =
  arg :: stack

(* HELPER for pushing errors onto the stack *)
let _error_ (stack : stack) = push (Error ":error:") stack

(* Removes the top of the stack *)
let pop (stack : stack) : (stack) =
  match stack with
  | [] -> push (Error ":error:") stack
  | h::t -> t

(* Performs arithmetic and integer logical functions like +, -, *, /, mod, lessThan, equal (only integer operands) *)
let arithmetic (f : int->int->int) (flag : ar_flag) (stack : stack) (env_var : env) : (stack) =
  match stack with
  | [] -> _error_ stack
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
            _error_ stack
        else
          (Int (f y x)) :: t
        |Log ->
          begin match (f y x) with
          | 0 -> push (Bool ":false:") t
          | 1 -> push (Bool ":true:") t
          | _ -> push (Error ":error:") stack
        end
        | _ -> (Int (f y x)) :: t
        end
      |_ -> _error_ stack
      end
    |_ -> _error_ stack
    end
  | _ -> _error_ stack

(* Negates the top of the stack (only integer operands) *)
let neg (stack : stack) (env_var : env) : (stack) =
  match stack with
  | [] -> _error_ stack
  | h::t ->
    let h = resolve h env_var in
    match h with
    |Int x -> push (Int (-x)) t
    |_ -> _error_ stack

(* Inverts the top of the stack (only bool operands) *)
let not_op (stack : stack) (env_var : env) : (stack) =
  match stack with
  | [] -> _error_ stack
  | h::t ->
    let h = resolve h env_var in
    match h with
    |Bool x -> push (Bool (t_bool_to_str(not(t_str_to_bool x)))) t
    |_ -> _error_ stack

(* Performs boolean operations like AND, OR (only bool operands) *)
let bool_op (f : bool->bool->bool) (stack : stack) (env_var : env) : (stack) =
  match stack with
  | [] -> _error_ stack
  | e1::e2::t ->
    let e1 = resolve e1 env_var in
    let e2 = resolve e2 env_var in
    begin match e1 with
    |Bool x ->
      begin match e2 with
      |Bool y -> push (Bool (t_bool_to_str(f (t_str_to_bool y) (t_str_to_bool x)))) t
      |_ -> _error_ stack
    end
    |_ -> _error_ stack
  end
  |_ -> _error_ stack

(* Performs string operations like concatenation (only string operands) *)
let str_op (f : string->string->string) (stack : stack) (env_var : env) : (stack) =
  match stack with
  | [] -> _error_ stack
  | e1::e2::t ->
    let e1 = resolve e1 env_var in
    let e2 = resolve e2 env_var in
    begin match e1 with
    |StringC x ->
      begin match e2 with
      |StringC y -> push (StringC (f y x)) t
      |_ -> _error_ stack
    end
    |_ -> _error_ stack
  end
  |_ -> _error_ stack

(* Swaps the top two elements of the stack *)
let swap (stack : stack) : (stack) =
  match stack with
  | [] -> _error_ stack
  | e1::e2::t -> push e2 (push e1 t)
  | _ -> _error_ stack

(* Converts arguments to printable strings *)
let toString (stack : stack) (env_var : env) : (stack) =
  match stack with
  | [] -> _error_ stack
  | h::t ->
    match h with
    |Int x -> push (StringC (string_of_int x)) t
    |Bool x |StringC x|Name x |Unit x |Error x -> push (StringC x) t
    |_ -> _error_ stack


(* Adds the top string to output to be printed *)
let println ((stacks, env_var, output, program) : info) : (info) =
  match stacks with
  | stack :: rest ->
    begin match stack with
    | [] -> (_error_ stack :: rest), env_var, output, program
    | h::t ->
      begin match h with
      |StringC x -> (t :: rest), env_var, x::output, program
      |_ -> (_error_ stack :: rest), env_var, output, program
    end
  end
  |_ -> raise StackException

(* If third top is true, top is left, else second top is left *)
let if_op (stack : stack) (env_var : env) : (stack) =
  match stack with
  | [] -> _error_ stack
  | e1::e2::e3::t ->
    let e3 = resolve e3 env_var in
    begin match e3 with
    |Bool x ->
      begin match x with
      |":true:" -> push e1 t
      |":false:" -> push e2 t
      |_ -> _error_ stack
    end
    |_ -> _error_ stack
  end
  |_ -> _error_ stack

(* Binds the value (Int, Bool, String, Unit or Value of Bound Name) present on top to a name present on second top *)
let bind (stack : stack) ((stacks, env_var, output, program) : info) : (info) =
  match stack with
  | [] -> _error_ stack :: stacks, env_var, output, program
  | e1::e2::t1 ->
    let e1 = resolve e1 env_var in
    begin match e2 with
    |Name n ->
      begin match e1 with
      |StringC _ |Int _ |Unit _ |Bool _ ->
        begin match env_var with
        | h2 :: t2 -> (push (Unit ":unit:") t1) :: stacks, ((n, e1) :: h2) :: t2, output, program
        | _ -> raise No_Environment
      end
      |_ ->  _error_ stack :: stacks, env_var, output, program
    end
    |_ ->  _error_ stack :: stacks, env_var, output, program
  end
  | _ ->  _error_ stack :: stacks, env_var, output, program

(* Starts a new scope, that is, create a new environment with previous bindings present and a new stack *)
let let_op ((stacks, env_var, output, program) : info) : (info) =
  match env_var with
  | h :: t ->
    let nenv_var = h :: h :: t in
    ([]::stacks, nenv_var, output, program)
  | _ -> raise No_Environment


(* Ends a scope, that is, deletes the latest environment and returns from the latest stack to the previous *)
let end_op ((stacks, env_var, output, program) : info) : (info) =
  match env_var with
  | h :: t ->
    let nenv_var = t in
    begin match stacks with
    | stack :: old :: rest ->
      begin match stack with
      | top :: els ->
        let nstack = (top :: old) in
        let nstacks = nstack :: rest in
        (nstacks, nenv_var, output, program)
      | _ -> raise StackException
      end
    | _ -> raise StackException
    end
  | _ -> raise No_Environment

(* Returns the last item on function stack frame to the caller stack frame *)
let return ((stacks, env_var, output, program) : info) : (info) =
  match env_var with
  | h :: t ->
    let nenv_var = t in
    begin match stacks with
    | stack :: old :: rest ->
      begin match stack with
      | top :: els ->
        let nstack = ((resolve top env_var) :: old) in
        let nstacks = nstack :: rest in
        (nstacks, nenv_var, output, program)
      | _ -> raise StackException
      end
    | _ -> raise StackException
    end
  | _ -> raise No_Environment

(* Helper function to check for successful function call *)
let func_checks ((stacks, env_var, output, program) : info) : (bool * (string * types)) =
  match stacks with
  | stack :: rest ->
    begin match stack with
    | arg :: fname :: els ->
      begin match fname with
      | Name fvar ->
        let fname_clear = List.mem_assoc fvar (List.hd env_var) in
        begin match arg with
        | Name pvar -> ((List.mem_assoc pvar (List.hd env_var) && fname_clear),(fvar,arg))
        | Int x -> ((true && fname_clear),(fvar,arg))
        | StringC x | Bool x | Unit x -> ((true && fname_clear),(fvar,arg))
        | _ -> (false, ("garbage",Error ":error:"))
      end
      | _ -> (false, ("garbage",Error ":error:"))
    end
    | _ -> raise StackException
    end
  | _ -> raise StackException

(*
Handles function calling:
Stack Updation: failed call -> error, successful call -> empty stack
Environment Updation: failed call -> no change, successful call -> function env with parameter-argument binding
Program Updation: failed call -> no change, successful call -> function body joined
 *)
let call ((stacks, env_var, output, program) : info) : (info) =
  let (clear, func_items) = func_checks (stacks, env_var, output, program) in
  begin match clear with
  | false ->
    begin match stacks with
    | stack :: rest ->
      let nstacks = ((_error_ stack) :: rest) in
      (nstacks, env_var, output, program)
    | _ -> raise StackException
    end
  | true ->
    let (fname, arg_val) = func_items in
    begin match List.assoc fname (List.hd env_var) with
    | Fun closure ->
      let (pname, fun_env, body) = closure in
      let fun_arg = resolve arg_val env_var in
      let nfun_env = (pname, fun_arg) :: fun_env in
      begin match env_var with
      | h :: t ->
        let nenv_var = nfun_env :: h :: t in
        begin match stacks with
        | stack :: rest ->
          begin match stack with
          | rem1 :: rem2 :: els ->
            let nstacks = ([] :: (els :: rest)) in
            let nprogram = body @ program in
            (nstacks, nenv_var, output, nprogram)
          | _ -> raise StackException
          end
        | _ -> raise StackException
        end
      | _ -> raise No_Environment
      end
    | _ -> raise FunException
    end
  end

(* Obtains the body of a function for the closure *)
let rec obtain_body (program : string list) (body : string list) (ctr : int) : (string list * string list) =
  match program with
  | line :: t ->
    let com_lst = split line in
    begin match com_lst with
    |(a,b) ->
      begin match String.trim a with
      | "fun" ->
        obtain_body t (line :: body) (ctr+1)
      | "funEnd" ->
        begin match ctr with
        | 0 -> (List.rev body, t)
        | _ ->
          obtain_body t (line :: body) (ctr-1)
        end
      | _ -> obtain_body t (line :: body) ctr
    end
  end
  |_ -> raise FunException

(* Handles function definations: creates a closure and binds it to the function name in the current environment *)
(*
A closure is mutually recursive with the current environment, that is, the closure stores
the current environment and the closure includes the current environment. To implement such
a structure the keyword "and" was used. Documentation: https://ocaml.org/docs/loops-recursion
*)
let fun_def ((stacks, env_var, output, prog) : info) (post_fun : string): (info) =
  let (fname,pname) = split post_fun in
  let fname = String.trim fname in
  let pname = String.trim pname in
  let body = obtain_body prog [] 0 in
  let fn_check = ((String.exists one_char fname) && (fst_valid (String.get fname 0)) && (String.for_all valid_char fname)) in
  let pn_check = ((String.exists one_char pname) && (fst_valid (String.get pname 0)) && (String.for_all valid_char pname)) in
  let same_check = (String.equal fname pname) in
  if(fn_check && pn_check && not(same_check)) then
    match env_var with
    | curr :: t ->
      let rec closure = Fun (pname, nenv, (fst body)) and nenv = (fname, closure) :: curr in
      let nenv_var = nenv :: t in
      begin match stacks with
      | stack :: rest ->
        let nstack = push (Unit ":unit:") stack in
        let nstacks = nstack :: rest in
        (nstacks, nenv_var, output, snd body)
      | _ -> raise StackException
      end
    | _ -> raise No_Environment
  else
    begin match stacks with
    | stack :: rest ->
      let nstack = push (Error ":error:") stack in
      let nstacks = nstack :: rest in
      (nstacks, env_var, output, snd body)
    | _ -> raise StackException
    end

(* Detects and executes the command passed in *)
let com_det (line : string) ((stacks, env_var, output, program) : info) : (info) =
  let com_lst = split line in
  match com_lst with
  | (a,b) ->
    begin match stacks with
    | stack :: rest ->
      begin match String.trim a with
      | "quit" -> ((pop stack)::rest, env_var, output, program)
      | "push" -> ((push (arg_det(String.trim b)) stack)::rest, env_var, output, program)
      | "pop" -> ((pop stack)::rest, env_var, output, program)
      | "add" -> ((arithmetic ( + ) NDiv stack env_var)::rest, env_var, output, program)
      | "sub" -> ((arithmetic ( - ) NDiv stack env_var)::rest, env_var, output, program)
      | "mul" -> ((arithmetic ( * ) NDiv stack env_var)::rest, env_var, output, program)
      | "div" -> ((arithmetic ( / ) Div stack env_var)::rest, env_var, output, program)
      | "rem" -> ((arithmetic ( mod ) Div stack env_var)::rest, env_var, output, program)
      | "neg" -> ((neg stack env_var)::rest, env_var, output, program)
      | "swap" -> ((swap stack)::rest, env_var, output, program)
      | "toString" -> ((toString stack env_var)::rest, env_var, output, program)
      | "println" -> println (stacks, env_var, output, program)
      | "lessThan" -> ((arithmetic (logical ( < )) Log stack env_var)::rest, env_var, output, program)
      | "equal" -> ((arithmetic (logical ( = )) Log stack env_var)::rest, env_var, output, program)
      | "and" -> ((bool_op ( && ) stack env_var)::rest, env_var, output, program)
      | "or" -> ((bool_op ( || ) stack env_var)::rest, env_var, output, program)
      | "not" -> ((not_op stack env_var)::rest, env_var, output, program)
      | "cat" -> ((str_op ( ^ ) stack env_var)::rest, env_var, output, program)
      | "if" -> ((if_op stack env_var)::rest, env_var, output, program)
      | "bind" -> bind stack (rest, env_var, output, program)
      | "let" -> let_op (stacks, env_var, output, program)
      | "end" -> end_op (stacks, env_var, output, program)
      | "fun" -> fun_def (stacks, env_var, output, program) b
      | "call" -> call (stacks, env_var, output, program)
      | "return" -> return (stacks, env_var, output, program)
      | _ -> (stacks, env_var, output, program)
    end
    | _ -> raise StackException
  end

(* Begins execution of the commands *)
let rec interpreter_begin ((stacks, env_var, output, program) : info) : (out) =
  match program with
  | [] -> output
  | h :: t ->
    match (com_det h (stacks, env_var, output, t)) with
    |(nstack, nenv_var, nout, nprogram) -> interpreter_begin (nstack, nenv_var, nout, nprogram)

(* Handles file handling *)
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
  let results = interpreter_begin ([[]], [[]], [], ls_str) in
  let output_bools ls oc = List.iter (Printf.fprintf oc "%s\n") ls in
  Out_channel.with_open_text out_file (output_bools (List.rev results))

  (* #use "interpreter.ml";; *)
  (* interpreter "part3/input6.txt" "output.txt";; *)
  (* ocamlc -o run interpreter.ml main.ml *)

  (* rewrite call to work with functions that are already resolved because functions can be returned. *)