namespace Lexer


/// <summary>
/// This module contains all the code to process an expression to an abstract-syntax-tree (AST).
/// </summary>
/// 
/// <remarks>
/// All the logic is nested in a private sub-module.
/// </remarks>
module Ast =


  /// <summary>
  /// The AST type.
  /// </summary>
  type Expr = 
  | Token of Tokens.Token
  | Block of List<Expr>


  /// <summary>
  /// This module contains element for debugging purposes.
  /// </summary>
  module private Debug = 


    /// <summary>
    /// Generate a <c>n</c> tabs string.
    /// </summary>
    /// 
    /// <param name="n">The amount of tabs.</param>
    /// 
    /// <returns>A string.</returns>
    /// 
    /// <example>
    ///   <code>
    ///     let tabs = generate_tabs 4
    ///     // tabs = "        "
    ///   </code>
    /// </example>
    let generate_tabs (n: int) : string = 
      let rec aux (m: int) (s: string) : string = 
        if m <= 0 then s else aux (m - 1) (s + "  ") 
      in aux n ""
      

    /// <summary>
    /// Converts an AST to a string.
    /// </summary>
    /// 
    /// <remarks>
    /// The functions returns a tabulated string representation of the expression.
    /// </remarks>
    /// 
    /// <param name="expression">The expression to be converted.</param>
    /// <param name="depth">The current depth (used for tabs).</param>
    /// 
    /// <returns>A string.</returns>
    /// 
    /// <example>
    ///   <code>
    ///     let expr = ... in
    ///     let str = expr_to_string expr 0 in 
    ///     printfn "Expression : %s" str
    ///   </code>
    /// </example>
    let rec expr_to_string (expression: Expr) (depth: int) : string = 
      match expression with
      | Token token -> (generate_tabs depth) + (Tokens.token_to_string token) + "\n" 
      | Block block -> 
        List.map (fun expr -> expr_to_string expr (depth + 1)) block 
        |> List.fold (fun s s' -> s + s') ""
    

    /// <summary>
    /// Converts an AST to a string.
    /// </summary>
    /// 
    /// <remarks>
    /// The functions returns a flat string representation of the expression.
    /// </remarks>
    /// 
    /// <param name="expression">The expression to be converted.</param>
    /// 
    /// <returns>A string.</returns>
    /// 
    /// <example>
    ///   <code>
    ///     let expr = ... in
    ///     let str = expr_to_string_flat expr in 
    ///     printfn "Expression : %s" str
    ///   </code>
    /// </example>
    let rec expr_to_string_flat (expression: Expr) : string = 
      match expression with
      | Token token -> sprintf "~ %s ~" (Tokens.token_to_string token)
      | Block block -> 
        let str = 
          List.map (fun expr -> expr_to_string_flat expr) block 
          |> List.fold (fun s  s' -> s + s') ""
        in "[ " + str + "]"


  /// <summary>
  /// This module contains element for comparing expressions and process some custom conditions.
  /// </summary>
  module public Validators = 


    /// <summary>
    /// Checks if the given expression is an operator token.
    /// </summary>
    /// 
    /// <param name="expr">The expression.</param>
    /// 
    /// <returns>A boolean</returns>
    /// 
    /// <example>
    ///   <code>
    ///     let expr = ... in
    ///     if is_operator expr 
    ///       then printfn "The expression is an operator."
    ///       else "The expression is not an operator."
    ///   </code>
    /// </example>
    let is_operator (expr: Expr) : bool = 
      match expr with
      | Token t -> Tokens.Validators.is_operator t
      | _ -> false


    /// <summary>
    /// Checks if the given expression is a value.
    /// </summary>
    /// 
    /// <param name="expr">The expression.</param>
    /// 
    /// <returns>A boolean</returns>
    /// 
    /// <example>
    ///   <code>
    ///     let expr = ... in
    ///     if is_value expr 
    ///       then printfn "The expression is a value."
    ///       else "The expression is not a value."
    ///   </code>
    /// </example>
    let is_value (expr: Expr) : bool = 
      match expr with
      | Token t -> Tokens.Validators.is_value t
      | _ -> true


    /// <summary>
    /// Checks if the given expressions matches to a double operand pattern.
    /// </summary>
    /// 
    /// <remarks>
    /// A double operand pattern is defined as follow: (Value) (Operator) (Value).
    /// </remarks>
    /// 
    /// <param name="e1">The first expression. It will be processed as the left oprand.</param>
    /// <param name="e2">The second expression. It will be processed as the operator.</param>
    /// <param name="e3">The third expression. It will be processed as the right oprand.</param>
    /// 
    /// <returns>A boolean</returns>
    /// 
    /// <example>
    ///   <code>
    ///     let expression1 = ... in
    ///     let expression2 = ... in
    ///     let expression3 = ... in
    ///     if is_double_operand_pattern_valid expression1 expression2 expression3 
    ///       then printfn "Double operand pattern found."
    ///       else printfn "Double operand pattern nor found."
    ///   </code>
    /// </example>
    let is_double_operand_pattern_valid (e1: Expr) (e2: Expr) (e3: Expr) : bool = 
      (is_value e1) && (is_operator e2) && (is_value e3)


    /// <summary>
    /// Checks if the given expressions matches to a single operand pattern.
    /// </summary>
    /// 
    /// <remarks>
    /// A single operand pattern is defined as follow: (Operator) (Value).
    /// </remarks>
    /// 
    /// <param name="e1">The thirst expression. It will be processed as the operator.</param>
    /// <param name="e2">The second expression. It will be processed as the oprand.</param>
    /// 
    /// <returns>A boolean</returns>
    /// 
    /// <example>
    ///   <code>
    ///     let expression1 = ... in
    ///     let expression2 = ... in
    ///     if is_single_operand_operation expression1 expression2 
    ///       then printfn "Single operand pattern found."
    ///       else printfn "Single operand pattern not found."
    ///   </code>
    /// </example>
    let is_single_operand_operation (e1: Expr) (e2: Expr) : bool = 
      (is_operator e1) && (is_value e2)


    /// <summary>
    /// Checks if the given expression matches to the given token.
    /// </summary>
    /// 
    /// <param name="expr">The expression.</param>
    /// <param name="token">The token.</param>
    /// 
    /// <returns>A boolean</returns>
    /// 
    /// <example>
    ///   <code>
    ///     let expression = ... in
    ///     let token = ... in
    ///     if is_expr_token expression token
    ///       then printfn "The expression matches to the token."
    ///       else printfn "The expression does not match the token."
    ///   </code>
    /// </example>
    let is_expr_token (expr: Expr) (token: Tokens.Token) : bool = 
      match expr with 
      | Block _ -> false
      | Token t -> 
        match t with
        | t when t = token -> true
        | _ -> false     


  /// <summary>
  /// This module contains all the logic.
  /// </summary>  
  module private Logic = 

    open Utils.Errors


    /// <summary>
    /// Extracts into seperate blocks the tokens inside parenthesis.
    /// </summary>
    /// 
    /// <remarks>
    /// This function ensures that paranthesis matches.
    /// </remarks>
    /// 
    /// <param name="tokens">The token list to process.</param>
    /// 
    /// <returns>
    /// If a paranthesis doesn't match it returns an Error else it returns an Expr.
    /// </returns>
    /// 
    /// <example>
    ///   <code>
    ///     open Utilities.Errors
    ///     
    ///     let tokens = [ ... ] in 
    ///     match extract_paranthesis tokens with
    ///     | Error e -> printfn "Something went wrong : %s" (error_to_string e)
    ///     | Ok _ -> printfn "Paranthesis extracted !" 
    ///   </code>
    /// </example>
    let extract_paranthesis (tokens: list<Tokens.Token>) : Result<Expr, Error> =
      let rec extract_parenthesis_aux (tokens: list<Tokens.Token>) (buff: list<Expr>) : Result<Expr * list<Tokens.Token>, Error> = 
        match tokens, buff with 
        | [], [] -> Error EmptyInput
        | [], _ -> Ok (Block (List.rev buff), [])
        | h :: t, _ ->
          match h with 
          | Tokens.LPar -> 
            match extract_parenthesis_aux t [] with
            | Error _ -> Error (MissingParanthesis)
            | Ok (e, t') -> extract_parenthesis_aux t' (e :: buff)
          | Tokens.RPar -> Ok (Block (List.rev buff), t) 
          | _ -> extract_parenthesis_aux t ((Token h) :: buff)
      in 
      match extract_parenthesis_aux tokens [] with
      | Ok (tree, _) -> Ok tree
      | Error e -> Error e


    /// <summary>
    /// Converts a list of result to a list of expr.
    /// </summary>
    /// 
    /// <remarks>
    /// If one of the result is an error variant, the program crashes. 
    /// </remarks>
    /// 
    /// <param name="list">The list of result.</param>
    /// 
    /// <returns>
    /// A list of Expr.
    /// </returns>
    /// 
    /// <example>
    ///   <code>
    ///     let list = [ ... ] |> extract_ok
    ///   </code>
    /// </example>
    let extract_ok (list: list<Result<Expr, Error>>) : list<Expr> = 
      List.map (fun (n: Result<Expr, Error>) ->  match n with | Ok n' -> n' | _ -> failwith "Found Error variant") list


    /// <summary>
    /// Nest every negate pattern inside a block in the given expression. 
    /// </summary>
    /// 
    /// <remarks>
    /// This function ensures that all negates pattern are valid.
    /// </remarks>
    /// 
    /// <remarks>
    /// If the given expression is an error variant, it immediately returns the error.
    /// </remarks>
    /// 
    /// <param name="expr">The expression to process.</param>
    /// 
    /// <returns>
    /// If a negate pattern is invalid it returns an Error else it returns an Expr.
    /// </returns>
    /// 
    /// <example>
    ///   <code>
    ///     open Utilities.Errors
    /// 
    ///     let expr = ... |> Ok in 
    ///     match extract_negate expr with
    ///     | Error e -> printfn "Something went wrong : %s" (error_to_string e)
    ///     | Ok _ -> printfn "Negate patterns extracted !"
    ///   </code>
    /// </example>
    let extract_negate (expr: Result<Expr, Error>) : Result<Expr, Error> =
      let rec aux (expression: Expr) : Result<Expr, Error> =
        match expression with
        | Token _ -> Ok expression
        | Block b ->
          match b with
          | [] -> Error (EmptyInput)
          | _ :: [] -> Ok expression
          | e1 :: e2 :: t -> 
            let t' = List.map (fun n -> aux n) t in 
            if Utils.Utilities.any_err t' then Error ExtractNegateFailed  else
            let t'' = extract_ok t' in
            match e1, e2 with
            | Block b1, Block b2 -> 
              let b1' = List.map (fun n -> aux n) b1 in 
              let b2' = List.map (fun n -> aux n) b2 in
              if (Utils.Utilities.any_err b1') ||  (Utils.Utilities.any_err b2') then Error ExtractNegateFailed else 
              let b1'' = extract_ok b1' |> Block in
              let b2'' = extract_ok b2' |> Block in
              Ok (Block (b1'' :: b2'' :: t''))
            | Block b, Token _ -> 
              let b' = List.map (fun n -> aux n) b in 
              if Utils.Utilities.any_err b' then Error ExtractNegateFailed else
              let b'' = extract_ok b' |> Block in
              Ok (Block ( b'' :: e2 :: t''))
            | Token _, Block b -> 
              let b' = List.map (fun n -> aux n) b in 
              if Utils.Utilities.any_err b' then Error ExtractNegateFailed else 
              let b'' = extract_ok b' |> Block in
              if (Validators.is_single_operand_operation e1 b'') && (Validators.is_expr_token e1 Tokens.Minus) 
                then Ok (Block ((Block [e1; b'']) :: t''))
                else Ok (Block (e1 :: b'' :: t''))
            | Token _, Token _ -> 
             if (Validators.is_single_operand_operation e1 e2) && (Validators.is_expr_token e1 Tokens.Minus) 
              then Ok (Block ((Block [e1; e2]) :: t''))
              else Ok (Block (e1 :: e2 :: t''))
      in
      match expr with
      | Error e -> Error e
      | Ok n -> aux n


    /// <summary>
    /// Nest every double operand pattern matching the given operator inside a block in the given expression. 
    /// </summary>
    /// 
    /// <remarks>
    /// This function ensures that all double operand pattern matching the given operator are valid.
    /// </remarks>
    /// 
    /// <remarks>
    /// If the given expression is an error variant, it immediately returns the error.
    /// </remarks>
    /// 
    /// <param name="expr">The expression to process.</param>
    /// <param name="operator">The operator.</param>
    /// 
    /// <returns>
    /// If a double operand pattern is invalid it returns an Error else it returns an Expr.
    /// </returns>
    /// 
    /// <example>
    ///   <code>
    ///     open Utilities.Errors
    ///     
    ///     let expression = ... |> Ok in
    ///     let operator = ... in 
    ///     match extract_double_operand_operation expression operator with
    ///     | Error e -> printfn "Something went wrong : %s" (error_to_string e)
    ///     | Ok _ -> printfn "Double operand patterns extracted !"
    ///   </code>
    /// </example>
    let rec extract_double_operand_operation (expr: Result<Expr, Error>) (operator: Tokens.Token) : Result<Expr, Error> = 
      let rec aux (expressions: list<Expr>) (buff: list<Expr>) (left: Option<Expr>) : Result<Expr, Error> = 
        match expressions with
        | [] -> if left.IsNone then Ok (Block (buff |> List.rev)) else Ok (Block ((left.Value :: buff) |> List.rev)) 
        | e :: [] -> 
          let e' = match e with | Block b -> aux b [] None | Token _ -> Ok e in 
          match e', left with 
          | Error e, _ -> Error e
          | Ok e'', None -> 
            if Validators.is_expr_token e'' operator 
              then Error (InvalidPattern "Isloated token found")
              else aux [] buff (Some e'')
          | Ok e'', Some left' -> 
            if Validators.is_expr_token e'' operator 
              then Error (InvalidPattern "Isolated token found")
              else aux [] (left' :: buff) (Some e'')
        | e1 :: e2 :: t ->
          let e1' = match e1 with | Block b -> aux b [] None | Token _ -> Ok e1 in
          let e2' = match e2 with | Block b -> aux b [] None | Token _ -> Ok e2 in
          match e1', e2', left with
          | Error e , _, _ | _, Error e, _ -> Error e 
          | Ok e1'', Ok e2'', None -> aux (e2'' :: t) buff (Some e1'')
          | Ok e1'', Ok e2'', Some left' -> 
            if Validators.is_double_operand_pattern_valid left' e1'' e2'' then
              if Validators.is_expr_token e1'' operator
                then aux t buff (Some (Block [left'; e1''; e2'']))
              else aux (e2'' :: t) (left' :: buff) (Some e1'')
            else aux (e2'' :: t) (left' :: buff) (Some e1'')
      in
      match expr with
      | Ok expr' ->
        match expr' with
        | Token _ -> expr
        | Block b -> aux b [] None 
      | _ -> expr
      

    /// <summary>
    /// Nest in a block all times pattern.
    /// </summary>
    /// 
    /// <remarks>
    /// This function ensures that all times pattern are valid.
    /// </remarks>
    /// 
    /// <remarks>
    /// If the given expression is an error variant, it immediately returns the error.
    /// </remarks>
    /// 
    /// <param name="expr">The expression to process.</param>
    /// 
    /// <returns>
    /// If a times pattern is invalid it returns an Error else it returns an Expr.
    /// </returns>
    /// 
    /// <example>
    ///   <code>
    ///     open Utilities.Errors
    /// 
    ///     let expression = ... |> Ok in 
    ///     match extract_times expression with
    ///     | Error e -> printfn "Something went wrong : %s" (error_to_string e)
    ///     | Ok _ -> printfn "Times patterns extracted !"
    ///   </code>
    /// </example>
    let extract_times (expr: Result<Expr, Utils.Errors.Error>) : Result<Expr, Utils.Errors.Error> = 
      extract_double_operand_operation expr Tokens.Times


    /// <summary>
    /// Nest in a block all divide pattern.
    /// </summary>
    /// 
    /// <remarks>
    /// This function ensures that all divide pattern are valid.
    /// </remarks>
    /// 
    /// <remarks>
    /// If the given expression is an error variant, it immediately returns the error.
    /// </remarks>
    /// 
    /// <param name="expr">The expression to process.</param>
    /// 
    /// <returns>
    /// If a divide pattern is invalid it returns an Error else it returns an Expr.
    /// </returns>
    /// 
    /// <example>
    ///   <code>
    ///     open Utilities.Errors
    /// 
    ///     let expression = ... |> Ok in 
    ///     match extract_divide expression with
    ///     | Error e -> printfn "Something went wrong : %s" (error_to_string e)
    ///     | Ok _ -> printfn "Divide patterns extracted !"
    ///   </code>
    /// </example>
    let extract_divide (expr: Result<Expr, Utils.Errors.Error>) : Result<Expr, Utils.Errors.Error> = 
      extract_double_operand_operation expr Tokens.Divide


    /// <summary>
    /// Nest in a block all add pattern.
    /// </summary>
    /// 
    /// <remarks>
    /// This function ensures that all add pattern are valid.
    /// </remarks>
    /// 
    /// <remarks>
    /// If the given expression is an error variant, it immediately returns the error.
    /// </remarks>
    /// 
    /// <param name="expr">The expression to process.</param>
    /// 
    /// <returns>
    /// If a add pattern is invalid it returns an Error else it returns an Expr.
    /// </returns>
    /// 
    /// <example>
    ///   <code>
    ///     open Utilities.Errors
    /// 
    ///     let expression = ... |> Ok in 
    ///     match extract_add expression with
    ///     | Error e -> printfn "Something went wrong : %s" (error_to_string e)
    ///     | Ok _ -> printfn "Add patterns extracted !"
    ///   </code>
    /// </example>
    let extract_add (expr: Result<Expr, Utils.Errors.Error>) : Result<Expr, Utils.Errors.Error> = 
      extract_double_operand_operation expr Tokens.Plus


    /// <summary>
    /// Nest in a block all substract pattern.
    /// </summary>
    /// 
    /// <remarks>
    /// This function ensures that all substract pattern are valid.
    /// </remarks>
    /// 
    /// <remarks>
    /// If the given expression is an error variant, it immediately returns the error.
    /// </remarks>
    /// 
    /// <param name="expr">The expression to process.</param>
    /// 
    /// <returns>
    /// If a substract pattern is invalid it returns an Error else it returns an Expr.
    /// </returns>
    /// 
    /// <example>
    ///   <code>
    ///     open Utilities.Errors
    /// 
    ///     let expression = ... |> Ok in 
    ///     match extract_minus expression with
    ///     | Error e -> printfn "Something went wrong : %s" (error_to_string e)
    ///     | Ok _ -> printfn "Minus patterns extracted !"
    ///   </code>
    /// </example>
    let extract_minus (expr: Result<Expr, Utils.Errors.Error>) : Result<Expr, Utils.Errors.Error> = 
      extract_double_operand_operation expr Tokens.Minus


  /// <summary>
  /// Attempts to convert a list of tokens to an AST.
  /// </summary>
  /// 
  /// <remarks>
  /// This functions ensure that the expression doesn't contains any structural error.
  /// </remarks>
  /// 
  /// <param name="tokens">The list of tokens.</param>
  /// 
  /// <returns>
  /// An error if any step returns an error else it returns an expression.
  /// </returns>
  /// 
  /// <example>
  ///   <code>
  ///     open Utilities.Errors
  /// 
  ///     let tokens = [ ... ] in 
  ///     match tokens_to_ast tokens with
  ///     | Error e -> printfn "Something went wrong : %s" (error_to_string e)
  ///     | Ok _ -> printfn "Tokens parsed to AST"
  ///   </code>
  /// </example>
  let tokens_to_ast (tokens: list<Tokens.Token>) : Result<Expr, Utils.Errors.Error> =
    tokens 
    |> Logic.extract_paranthesis
    |> Logic.extract_negate
    |> Logic.extract_times
    |> Logic.extract_divide
    |> Logic.extract_add
    |> Logic.extract_minus
    

  /// <summary>
  /// Converts an expression to a string.
  /// </summary>
  /// 
  /// <param name="expression">The expression.</param>
  /// 
  /// <returns>A string.</returns>
  /// 
  /// <example>
  ///   <code>
  ///     let expression = ... in
  ///     let str = expr_to_string expression in
  ///     printfn "Expression : %s" str
  ///   </code>
  /// </example>
  let expr_to_string (expression: Expr) : string = 
    Debug.expr_to_string expression 0
