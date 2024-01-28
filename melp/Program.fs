
open GUI.Plotting


let convert_to_expr (str: string) : Option<list<Lexer.Tokens.Token>> = 
  match Lexer.Tokens.string_to_tokens str with
  | Error e -> 
    let _ = Utilities.Printer.print Utilities.Printer.Error (Utils.Errors.error_to_string e) in 
    None
  | Ok tokens -> 
    let _ = Utilities.Printer.print Utilities.Printer.Info (Lexer.Tokens.tokens_to_string tokens) in
    Some tokens


let convert_to_ast (tokens: list<Lexer.Tokens.Token>) : Option<Lexer.Ast.Expr> = 
  match Lexer.Ast.tokens_to_ast tokens with
  | Error e -> 
    let _ = Utilities.Printer.print Utilities.Printer.Error (Utils.Errors.error_to_string e) in 
    None
  | Ok ast -> 
    let _ = Utilities.Printer.print Utilities.Printer.Info (Lexer.Ast.expr_to_string ast) in 
    Some ast


let convert_to_tree (ast: Lexer.Ast.Expr) : Option<Lexer.Parser.Tree> = 
  match Lexer.Parser.parse_ast ast with
  | Error e -> 
    let _ = Utilities.Printer.print Utilities.Printer.Error (Utils.Errors.error_to_string e) in 
    None
  | Ok tree -> Some tree


let ask_for_double_value (name: string) : Option<double> = 
  let _ = Utilities.Printer.print Utilities.Printer.Normal (sprintf "Enter value for %s : " name) in
  match System.Console.ReadLine() |> System.Double.TryParse with
  | true,v -> Some v
  | _ -> None

let ask_for_boolean_value (name: string) : Option<bool> = 
  let _ = Utilities.Printer.print Utilities.Printer.Normal "Turn on NON-CONTINUE-VALUE ('y' = yes, 'n' = no) :" in
  match System.Console.ReadLine() with
  | "y" -> Some true
  | "n" -> Some false
  | _ -> None


let ask_for_target (variables: list<string>) : Option<string * (double * double * double) * bool> = 
  let var = 
    match variables.Length with
    | 0 -> None 
    | 1 -> Some variables.Head
    | _ ->
      let vars = List.fold (fun a b ->  a + " " + b) "" variables in
      let _ = Utilities.Printer.print Utilities.Printer.Normal (sprintf "Select the targeted id (choices :%s):" vars) in
      let target = System.Console.ReadLine() in
      if List.contains target variables then Some target else None
  in
  if var.IsNone then 
    let _ = Utilities.Printer.print Utilities.Printer.Error (
      if variables.Length = 0 
        then "No variable in expression" 
        else "Invalid selected variable" 
      |> sprintf "%s"
    ) in None 
  else 
    let min = ask_for_double_value (var.Value + "-MIN") in
    if min.IsNone then
      let _ = Utilities.Printer.print Utilities.Printer.Error "Invalid minimum bound given" in None
    else
    let max = ask_for_double_value (var.Value + "-MAX") in
    if max.IsNone then 
      let _ = Utilities.Printer.print Utilities.Printer.Error "Invalid maximum bound given" in None
    else 
    let step = ask_for_double_value "STEP" in
    if step.IsNone then 
      let _ = Utilities.Printer.print Utilities.Printer.Error "Invalid step given" in None
    else
    let ancv = ask_for_boolean_value "Non continue values" in 
    if ancv.IsNone then 
      let _ = Utilities.Printer.print Utilities.Printer.Error "Invalid value given for 'Non continue values'" in None
    else 
    Some (var.Value, (min.Value, max.Value, step.Value), ancv.Value)



let ask_for_values (variables: list<string>) : Option<list<string * double>> = 
  let rec aux (variables': list<string>) (output: list<string * double>) : Option<list<string * double>> = 
    match variables' with
    | [] -> Some output
    | h :: t -> 
      match ask_for_double_value h with
      | None -> 
        let _ = Utilities.Printer.print Utilities.Printer.Error (sprintf "Invalid value given for %s" h) in None
      | Some v -> aux t ((h, v) :: output)
  in aux variables []


let rec loop _ = 
  let _ = Utilities.Printer.print Utilities.Printer.Normal "Enter math expression (enter <exit> to exit the program) :" in 
  let str = System.Console.ReadLine() in 
  if str = "exit" then () else 
  let expr = convert_to_expr str in
  if expr.IsNone then loop () else
  let ast = convert_to_ast expr.Value in
  if ast.IsNone then loop () else 
  let tree = convert_to_tree ast.Value in 
  if tree.IsNone then loop () else
  let ids = Lexer.Parser.get_list_id tree.Value in
  match ids with
  | Error e -> 
    let _ = Utilities.Printer.print Utilities.Printer.Error "Can not extract identifiers" in loop ()
  | Ok ids' -> 
    let target = ask_for_target ids' in 
    if target.IsNone then loop () else
    let target_name, (xmin, xmax, step), ancv = target.Value in 
    let values = ask_for_values [for id in ids' do if id <> target_name then yield id] in
    if values.IsNone then loop () else
    let config = { tree=tree.Value; target=target_name; vals=values.Value; xmin=xmin; xmax=xmax; step=step; allowNonContinue=ancv; title=str} in
    let _ = plot_function config in 
    loop ()



let _ = loop ()