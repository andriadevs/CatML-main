open Lang

let test_string = "Let x:int = 5 In x = 10" (* Test string can be set here to be used with CatML test *)
let return_message = "Returned: " (* String printed to the terminal before printing the return value of the expression that was run. *)


(* Rudimentary error handler *)

let handle_parse_error(e, l, s : exn * Lexing.lexbuf * string) =
  let prev = String.sub s 0 l.lex_curr_pos in
  let line_num = List.length (String.split_on_char '\n' prev) in
  print_string("Parse Error: '" ^ (Printexc.to_string e) ^ "' at line " ^ string_of_int(line_num) ^ "\n")

let handle_type_error(full_e, err_e: expr * expr) = 
  print_string("Type Error: Expression type cannot be resolved.\n");
  print_string("Type unresolvable at: " ^ string_of_expr(err_e) ^ "\n");
  begin match err_e with (* Error hints *)
  | Not(_) -> print_string("! can only be used with booleans!\n")
  | And(_,_) -> print_string("& can only be used with booleans!\n")
  | Or(_,_) -> print_string("| can only be used with booleans!\n")
  | Plus(_,_) -> print_string("+ can only be used with integers!\n")
  | Mult(_,_) -> print_string("* can only be used with integers!\n")
  | If(e1,_,_) -> 
    if type_of_expr(empty_context,e1) == Type BOOL
    then print_string("Then and Else must resolve to the same type!\n")
    else print_string("If statement requires a boolean!\n")
  | _ -> ()
  end

let handle_reduce_error(e, trace : expr * string) =
  print_string("Reduce Error: Expression could not be reduced to a value.\n");
  print_string("Expression stuck at: " ^ string_of_expr(e) ^ "\n");
  print_string("Trace: " ^ trace)

(* Helper functions *)

let rec read_all(file : in_channel) : string =
  try
    let line = input_line file in
    line ^ "\n" ^ read_all(file)
  with e -> close_in file; ""
;;

let parse (s : string) : expr option =
  let lexbuf = Lexing.from_string s in
  try let ast = Parser.prog Lexer.read lexbuf in Some ast
  with e -> handle_parse_error(e, lexbuf, s); None

let eval_with_errors (e : expr) : expr option * string =
  begin match type_of_expr(empty_context, e) with
    | NoType e' -> handle_type_error(e, e'); (None, "")
    | Type _ ->
      let result, trace = trace_and_reduce e false in
      if (is_val result) then (Some result, trace)
      else (handle_reduce_error(result, trace); (None, trace))
  end

(* Command-line interface functionality *)

let do_help() = (* Called when help is used *)
  print_string(" - CatML help : shows this message\n");
  print_string(" - CatML test : evaluates a test expression defined in main.ml\n");
  print_string(" - CatML run [filename] : evaluates the given .catml file\n");
  print_string(" - CatML trace [filename] : evaluates the given .catml file and prints the trace\n")
;;

let do_test() = (* Called when test is used *)
  print_string("Test String: '" ^ test_string ^ "'\n");
  begin match parse(test_string) with
    | Some e ->
    begin match eval_with_errors(e) with
      | Some e, _ -> print_string(return_message ^ string_of_expr(e)  ^ "\n")
      | None, _ -> ()
    end
    | None -> ()
  end
;;

let do_run() = (* Called when run [filename] is used *)
  if (Array.length Sys.argv > 3) then print_string("Error: Too many arguments provided! Use CatML run [filename].")
  else if (Array.length Sys.argv < 3) then print_string("Error: No filename provided! Use CatML run [filename].")
  else
    let file = open_in Sys.argv.(2) in
    let file_string : string = read_all(file) in
    begin match parse(file_string) with
      | Some e ->
      begin match eval_with_errors(e) with
        | Some e, _ -> print_string(return_message ^ string_of_expr(e)  ^ "\n")
        | None, _ -> ()
      end
      | None -> ()
    end

let do_trace() = (* Called when trace [filename] is used *)
  if (Array.length Sys.argv > 3) then print_string("Error: Too many arguments provided! Use CatML run [filename].")
  else if (Array.length Sys.argv < 3) then print_string("Error: No filename provided! Use CatML run [filename].")
  else
    let file = open_in Sys.argv.(2) in
    let file_string : string = read_all(file) in
    begin match parse(file_string) with
      | Some e ->
        begin match eval_with_errors(e) with
        | Some _, trace -> print_string("Trace:\n" ^ trace)
        | None, _ -> ()
        end
      | None -> ()
    end

let () =
  if Array.length Sys.argv == 1 then Printf.printf "Usage: CatML [OPTION]\nType CatML help to see a list of options.\n"
  else
    begin match Sys.argv.(1) with
      | "help" -> do_help()
      | "test" -> do_test()
      | "run" -> do_run()
      | "trace" -> do_trace()
      | _ -> print_string("'" ^ Sys.argv.(1) ^ "' is not a valid option.\nType CatML help to see a list of options.\n")
    end
;;