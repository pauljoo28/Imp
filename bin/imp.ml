open Impl

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
    print_endline "Hello world!"
