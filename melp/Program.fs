
/// <summary>
/// This is the main module.
/// </summary>
module Program = 

  open GUI.Plotting


  /// <summary>
  /// Converts a string to a list of token.
  /// </summary>
  /// 
  /// <remarks>
  /// Returns None if the string is invalid.
  /// </remarks>
  /// 
  /// <param name="str">The string.</param>
  /// 
  /// <returns>
  /// A list of token or None if the string is invalid.
  /// </returns>
  /// 
  /// <example>
  ///   <code>
  ///     let str = "..." in
  ///     match convert_to_expr str with
  ///     | None -> printfn "The string can not be converted to tokens."
  ///     | Some list -> printfn "The string is converted to %i tokens." list.Length
  ///   </code>
  /// </example>
  let convert_to_expr (str: string) : Option<list<Lexer.Tokens.Token>> = 
    match Lexer.Tokens.string_to_tokens str with
    | Error e -> 
      let _ = Utilities.Printer.print Utilities.Printer.Error (Utils.Errors.error_to_string e) in 
      None
    | Ok tokens -> 
      let _ = Utilities.Printer.print Utilities.Printer.Info (Lexer.Tokens.tokens_to_string tokens) in
      Some tokens


  /// <summary>
  /// Converts a list of tokens to an Abstract Syntax Tree.
  /// </summary>
  /// 
  /// <remarks>
  /// Returns None if the list of tokens is invalid.
  /// </remarks>
  /// 
  /// <param name="tokens">The list of tokens.</param>
  /// 
  /// <returns>
  /// Returns an AST or None if the list of tokens is invalid.
  /// </returns>
  /// 
  /// <example>
  ///   <code>
  ///     let tokens = [ ... ] in 
  ///     match convert_to_ast tokens with
  ///     | None -> printfn "The tokens can not be parsed to an AST."
  ///     | Some _ -> printfn "The tokens are parsed !"
  ///   </code>
  /// </example>
  let convert_to_ast (tokens: list<Lexer.Tokens.Token>) : Option<Lexer.Ast.Expr> = 
    match Lexer.Ast.tokens_to_ast tokens with
    | Error e -> 
      let _ = Utilities.Printer.print Utilities.Printer.Error (Utils.Errors.error_to_string e) in 
      None
    | Ok ast -> 
      let _ = Utilities.Printer.print Utilities.Printer.Info (Lexer.Ast.expr_to_string ast) in 
      Some ast


  /// <summary>
  /// Converts an AST to Tree.
  /// </summary>
  /// 
  /// <remarks>
  /// Returns None if the AST is invalid.
  /// </remarks>
  /// 
  /// <param name="ast">The AST.</param>
  /// 
  /// <returns>
  /// Returns a Tree or None if the list AST is invalid.
  /// </returns>
  /// 
  /// <example>
  ///   <code>
  ///     let ast = ... in 
  ///     match convert_to_tree ast with
  ///     | None -> printfn "The AST can not be parsed to a Tree."
  ///     | Some _ -> printfn "The AST is parsed !"
  ///   </code>
  /// </example>
  let convert_to_tree (ast: Lexer.Ast.Expr) : Option<Lexer.Parser.Tree> = 
    match Lexer.Parser.parse_ast ast with
    | Error e -> 
      let _ = Utilities.Printer.print Utilities.Printer.Error (Utils.Errors.error_to_string e) in 
      None
    | Ok tree -> Some tree


  /// <summary>
  /// Ask the user for a double value in the console.
  /// </summary>
  /// 
  /// <remarks>
  /// If the user's value is not a double, the function returns None. 
  /// </remarks>
  /// 
  /// <param name="name">The name of the asked value</param>
  /// 
  /// <returns>
  /// A double or None if the user's value is can not be parsed as a double.
  /// </returns> 
  /// 
  /// <example>
  ///   <code>
  ///     let name = "..." in
  ///     match ask_for_double_value name with
  ///     | None -> printfn "Invalid value selected by the user."
  ///     | Some d -> printfn "The value for '%s' is %d" name d 
  ///   </code>
  /// </example>
  let ask_for_double_value (name: string) : Option<double> = 
    let _ = Utilities.Printer.print Utilities.Printer.Normal (sprintf "Enter value for %s : " name) in
    match System.Console.ReadLine() |> System.Double.TryParse with
    | true,v -> Some v
    | _ -> None


  /// <summary>
  /// Ask the user for a boolean value in the console.
  /// </summary>
  /// 
  /// <remarks>
  /// If the user's value is invalid, the function returns None. 
  /// </remarks>
  /// 
  /// <param name="name">The name of the asked value</param>
  /// 
  /// <returns>
  /// A boolean or None if the user's value is invalid.
  /// </returns> 
  /// 
  /// <example>
  ///   <code>
  ///     let name = "..." in
  ///     match ask_for_boolean_value name with
  ///     | None -> printfn "Invalid value selected by the user."
  ///     | Some b -> printfn "The value for '%s' is %O" name b
  ///   </code>
  /// </example>
  let ask_for_boolean_value (name: string) : Option<bool> = 
    let _ = Utilities.Printer.print Utilities.Printer.Normal (sprintf "Turn on %s ('y' = yes, 'n' = no) :" name) in
    match System.Console.ReadLine() with
    | "y" -> Some true
    | "n" -> Some false
    | _ -> None


  /// <summary>
  /// The function asks the user for :
  /// <list>
  ///   <item>The identifier to vary (i.e. the variable).</item>
  ///   <item>The variable's minimum value.</item>
  ///   <item>The variable's maximum value.</item>
  ///   <item>The variable's step.</item>
  /// </list>
  /// </summary>
  /// 
  /// <remarks>
  /// If any of the values given by the user is invalid, the function will return None.
  /// </remarks>
  /// 
  /// <param name="variables">The list of identifiers.</param>
  /// 
  /// <returns>
  /// The name of the variable and its parameters (mininmum, maximum, step) or None if any of the values given by the user is invalid.
  /// </returns>
  /// 
  /// <example>
  ///   <code>
  ///     let variables = [ ... ] in
  ///     match ask_for_target variables with
  ///     | None -> printfn "An invalid value has been selected by the user."
  ///     | Some (var, (min', max', step)) -> 
  ///         printfn "The variable '%s' will vary from %d to %d with a step of %d." var min' max' step
  ///   </code>
  /// </example>
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


  /// <summary>
  /// Asks the user for fixed values for all the identifiers.
  /// </summary>
  /// 
  /// <remarks>
  /// If any value given by the user is invalid, the function returns None.
  /// </remarks>
  /// 
  /// <param name="variables">List of all identifiers.</param>
  /// 
  /// <returns>
  /// A list of identifiers (string) with their values (double) or None if any value given by the user is invalid.
  /// </returns>
  /// 
  /// <example>
  ///   <code>
  ///     let variables = [ ... ] in 
  ///     match ask_for_values variables with
  ///     | None -> printfn printfn "An invalid value has been selected by the user."
  ///     | Some _ -> printfn "All values for variables have been selected."
  ///   </code>
  /// </example>
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


  /// <summary>
  /// The main loop of the application. The function : 
  /// <ol>
  ///   <li>Asks the user for an expression.</li>
  ///   <li>Parses the expression.</li>
  ///   <li>Asks the user for the plotting data.</li>
  /// </ol>
  /// </summary>
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


  /// <summary>
  /// The main function.
  /// </summary>
  let [<EntryPoint>] main _ = 
    let _ = loop () in 0