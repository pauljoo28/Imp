open Impl

let program_file : string option ref = ref None
let set_program_file (arg : string) : unit = 
    match !program_file with
    | None -> program_file := Some arg
    | Some _ -> ()  (* Don't overwrite program_file *)

let spec_list = []
let usage_msg = "Provide a single imp program file to run this interpreter on"

let parse_prog f : ImpAST.prog =
    let ch =
        try open_in f
        with Sys_error s -> failwith ("Cannot open file: " ^ s) in
    let prog : ImpAST.prog =
        let lexbuf = Lexing.from_channel ch in
        try
            Parser.main Lexer.read lexbuf
        with 
            | _ ->
                begin
                    close_in ch;
                let pos = lexbuf.Lexing.lex_curr_p in
                let tok = (Lexing.lexeme lexbuf) in
                (* let line = pos.Lexing.pos_lnum in *)
                let cnum = pos.Lexing.pos_cnum - pos.Lexing.pos_bol in
                failwith ("Parsing error at token '" ^ tok ^ "', line "
                     ^ (string_of_int pos.Lexing.pos_lnum) ^ ", column " ^ string_of_int cnum)
                end in
    close_in ch; prog

let _ =
    Arg.parse spec_list set_program_file usage_msg;
    match !program_file with
    None -> print_string (Arg.usage_string spec_list usage_msg) | Some f ->
    let prog = parse_prog f in
    Interp.eval_prog prog
