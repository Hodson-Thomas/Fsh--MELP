let rec main _ =
  let _ = printfn "Enter expression (enter <exit> to exit the program): ";
  let str = System.Console.ReadLine() in 
  if str = "exit" then () else 
  let _ = 
    match Lexer.Tokens.string_to_tokens str with
    | Error e -> printfn "%s" (Utils.Errors.error_to_string e)
    | Ok tokens -> (
      printfn "RES : %s" (Lexer.Tokens.tokens_to_string tokens)
      match Lexer.Ast.tokens_to_ast tokens with
      | Error e -> printfn "%s" (Utils.Errors.error_to_string e)
      | Ok ast ->  (
        printfn "%s" (Lexer.Ast.expr_to_string ast);
        match Lexer.Parser.parse_ast ast with
        | Error e -> printfn "%s" (Utils.Errors.error_to_string e)
        | Ok t -> printfn "%s" (Lexer.Parser.tree_to_string t)
      )
    )
  in main ()



let _ = main ()