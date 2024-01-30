namespace Lexer


/// <summary>
/// This module contains all the code to process an AST to an evaluable expression (Tree).
/// </summary>
/// 
/// <remarks>
/// All the logic is nested in a private sub-module.
/// </remarks>
module Parser = 
  

  /// <summary>
  /// The evaluable expression type.
  /// </summary>  
  type Tree = 
  | Add of Tree * Tree
  | Sub of Tree * Tree
  | Neg of Tree
  | Mul of Tree * Tree
  | Div of Tree * Tree
  | Int of int
  | Float of float
  | Id of string


  /// <summary>
  /// This module contains element for debugging purposes.
  /// </summary>
  module private Debug = 


    /// <summary>
    /// Converts a tree to a string.
    /// </summary>
    /// 
    /// <param name="tree">The tree.</param>
    /// 
    /// <results>A string.</results>
    /// 
    /// <example>
    ///   <code>
    ///     let tree = ... in
    ///     let str = tree_to_string tree in
    ///     printfn "Tree : %s" str
    ///   </code>
    /// </example>
    let rec tree_to_string (tree: Tree) : string = 
      match tree with
      | Add (left, right) ->  
        let left' = tree_to_string left in 
        let right' = tree_to_string right in
        sprintf "(%s + %s)" left' right'
      | Sub (left, right) ->  
        let left' = tree_to_string left in 
        let right' = tree_to_string right in
        sprintf "(%s - %s)" left' right'
      | Mul (left, right) ->  
        let left' = tree_to_string left in 
        let right' = tree_to_string right in
        sprintf "(%s * %s)" left' right'
      | Div (left, right) ->  
        let left' = tree_to_string left in 
        let right' = tree_to_string right in
        sprintf "(%s ÷ %s)" left' right'
      | Neg op -> 
        let op' = tree_to_string op in
        sprintf "(- %s)" op'
      | Int i -> i |> string
      | Float f -> f |> string
      | Id i -> i


  /// <summary>
  /// This module contains all the logic.
  /// </summary>  
  module private Logic = 

    open Utils.Errors


    /// <summary>
    /// Attemps to convert an AST to a Tree.
    /// </summary>
    /// 
    /// <remarks>
    /// This function ensures that the processed AST is valid.
    /// </remarks>
    /// 
    /// <param name="ast">The AST.</param>
    /// 
    /// <returns>An error if the AST is invalid else a Tree.</returns>
    /// 
    /// <example>
    ///   <code>
    ///     open Utilities.Errors
    /// 
    ///     let ast = ... in
    ///     match parse_ast ast with
    ///     | Error e -> printfn "Something went wrong : %s" (error_to_string e)
    ///     | Ok _ -> printfn "The AST is parsed to a tree."
    ///   </code>
    /// </example>
    let rec parse_ast (ast: Ast.Expr) : Result<Tree, Error> = 
      match ast with
      | Ast.Expr.Token token -> 
        match token with
        | Tokens.Float f -> Float f |> Ok 
        | Tokens.Int i ->  Int i |> Ok
        | Tokens.Id i -> Id i |> Ok
        | _ -> ParsingError "Isolated non-value token found" |> Error
      | Ast.Expr.Block block -> 
        match block with
        | [] -> Error EmptyInput
        | token :: [] -> parse_ast token
        | token1 :: token2 :: [] -> 
          let token2' = parse_ast token2 in
          let token1' = 
            match token1 with | Ast.Expr.Token tk -> Ok (tk) | _ -> ParsingError "Found block as operator" |> Error  in
          match token1', token2' with
          | Error e, _ | _, Error e -> Error e
          | Ok token1'', Ok token2'' -> 
            match token1'' with
            | Tokens.Minus -> Neg token2'' |> Ok
            | _ -> ParsingError "" |> Error 
        | token1 :: token2 :: token3 :: [] -> 
          let token1' = parse_ast token1 in 
          let token3' = parse_ast token3 in
          let token2': Result<Tokens.Token,Error> = match token2 with | Ast.Expr.Token tk -> Ok (tk) | _ -> ParsingError "Found block as operator" |> Error in
          match token1', token2', token3' with
          | Error e, _, _ | _, Error e, _ | _, _, Error e -> Error e
          | Ok token1'', Ok token2'', Ok token3'' -> 
            match token2'' with
            | Tokens.Plus -> Add (token1'', token3'') |> Ok
            | Tokens.Minus -> Sub (token1'', token3'') |> Ok
            | Tokens.Times -> Mul (token1'', token3'') |> Ok
            | Tokens.Divide -> Div (token1'', token3'') |> Ok
            | _ -> ParsingError "Invalid operator found" |> Error
        | _ -> ParsingError "Too many operand found" |> Error


    /// <summary>
    /// Returns the value of a fixed variable.
    /// </summary>
    /// 
    /// <param name="var">The queried variable</param>
    /// <param name="var">The list of variables and their values.</param>
    /// 
    /// <returns>
    /// If the queried variable is not found, the function returns None else it returns the variable's value.
    /// </returns>
    /// 
    /// <example>
    ///   <code>
    ///     let var = "x"
    ///     let vals = [("y", 1.0); ("z", 2.0); ("x", 3.0); ("a", 4.0)]
    ///     match get_value var vals with  
    ///     | None -> printfn "Value not found."
    ///     | Some d -> printfn "The value of %s is %d" var d
    ///       // d = 3.0
    ///   </code>
    /// </example>
    let rec get_value (var: string) (vals: list<string * double>) : Option<double> = 
      match vals with
      | [] -> None
      | (n, v) :: t -> 
        if n = var then Some v 
        else get_value var t 


    /// <summary>
    /// Evaluates the given tree.
    /// </summary>
    /// 
    /// <param name="tree">The tree.</param>
    /// <param name="vals">The variables and their values.</param>
    /// 
    /// <returns>
    /// If the evaluation founds a variable with no associated value, it returns an error else it returns the evaluated value.
    /// </returns>
    /// 
    /// <example>
    ///   <code>
    ///     open Utilities.Errors
    /// 
    ///     let tree = ... in 
    ///     let vals = [ ... ] in
    ///     match eval_tree tree vals with
    ///     | Ok d -> printfn "The value of the tree is %d" d
    ///     | Error e -> printfn "Something went wrong %s" (error_to_string e)
    ///   </code>
    /// </example>
    let rec eval_tree (tree: Tree) (vals: list<string * double>) : Result<double, Error> = 
      match tree with
      | Add (left, right) -> 
        let left' = eval_tree left vals in 
        let right' = eval_tree right vals in 
        match left', right' with
        | Error e, _ | _, Error e -> Error e
        | Ok left'', Ok right'' -> left'' + right'' |> Ok
      | Sub (left, right) -> 
        let left' = eval_tree left vals in 
        let right' = eval_tree right vals in 
        match left', right' with
        | Error e, _ | _, Error e -> Error e
        | Ok left'', Ok right'' -> left'' - right'' |> Ok
      | Mul (left, right) -> 
        let left' = eval_tree left vals in 
        let right' = eval_tree right vals in 
        match left', right' with
        | Error e, _ | _, Error e -> Error e
        | Ok left'', Ok right'' -> left'' * right'' |> Ok
      | Div (left, right) -> 
        let left' = eval_tree left vals in 
        let right' = eval_tree right vals in 
        match left', right' with
        | Error e, _ | _, Error e -> Error e
        | Ok left'', Ok right'' -> 
          if right'' = 0 then NonContinueError "Division by 0" |> Error
          else left'' + right'' |> Ok
      | Neg op ->
        let op' = eval_tree op vals in 
        match op' with
        | Error e -> Error e
        | Ok op'' -> - op'' |> Ok
      | Float f -> f |> Ok
      | Int i -> i |> double |> Ok
      | Id i -> 
        match get_value i vals with
        | Some v -> Ok v
        | None -> EvalError (sprintf "No value for var <%s> was given" i) |> Error


    /// <summary>
    /// Returns all the identifiers in the given tree.
    /// </summary>
    /// 
    /// <param name="tree">The tree.</param>
    /// 
    /// <returns>A list of string.</returns>
    /// 
    /// <example>
    ///   <code>
    ///     open Utilities.Errors
    ///     
    ///     let tree = ... in
    ///     match get_list_id tree with
    ///     | Error e -> printfn "Something went wrong : %s" (error_to_string e)
    ///     | Ok list -> printfn "There is %i identifiers in the tree" list.Length
    ///   </code>
    /// </example>
    let rec get_list_id (tree: Tree) : Result<list<string>, Error> = 
      match tree with
      | Add (left, right) | Mul (left, right) | Sub (left, right) | Div (left, right) ->
        let left' = get_list_id left in
        let right' = get_list_id right in
        match left', right' with
        | Error e, _ | _, Error e -> Error e
        | Ok left'', Ok right'' -> Utils.Utilities.intersect_list left'' right'' |> Ok
      | Neg op -> get_list_id op 
      | Id i -> Ok [i]
      | _ -> Ok []


  /// <summary>
  /// Returns all the identifiers in the given tree.
  /// </summary>
  /// 
  /// <param name="tree">The tree.</param>
  /// 
  /// <returns>A list of string.</returns>
  /// 
  /// <example>
  ///   <code>
  ///     open Utilities.Errors
  ///     
  ///     let tree = ... in
  ///     match get_list_id tree with
  ///     | Error e -> printfn "Something went wrong : %s" (error_to_string e)
  ///     | Ok list -> printfn "There is %i identifiers in the tree" list.Length
  ///   </code>
  /// </example>
  let get_list_id (tree: Tree) : Result<list<string>, Utils.Errors.Error> = 
    Logic.get_list_id tree


  /// <summary>
  /// Attemps to convert an AST to a Tree.
  /// </summary>
  /// 
  /// <remarks>
  /// This function ensures that the processed AST is valid.
  /// </remarks>
  /// 
  /// <param name="ast">The AST.</param>
  /// 
  /// <returns>An error if the AST is invalid else a Tree.</returns>
  /// 
  /// <example>
  ///   <code>
  ///     open Utilities.Errors
  /// 
  ///     let ast = ... in
  ///     match parse_ast ast with
  ///     | Error e -> printfn "Something went wrong : %s" (error_to_string e)
  ///     | Ok _ -> printfn "The AST is parsed to a tree."
  ///   </code>
  /// </example>
  let parse_ast (ast: Ast.Expr) : Result<Tree, Utils.Errors.Error> = 
    Logic.parse_ast ast


  /// <summary>
  /// Converts a tree to a string.
  /// </summary>
  /// 
  /// <param name="tree">The tree</param>
  /// 
  /// <returns>
  /// A string.
  /// </returns>
  /// 
  /// <example>
  ///   <code>
  ///     let tree = ... in 
  ///     let str = tree_to_string tree in
  ///     printfn "Tree : %s" str
  ///   </code>
  /// </example>
  let tree_to_string (tree: Tree) : string = 
    Debug.tree_to_string tree
  

  /// <summary>
  /// Evaluates the given tree.
  /// </summary>
  /// 
  /// <param name="tree">The tree.</param>
  /// <param name="vals">The variables and their values.</param>
  /// 
  /// <returns>
  /// If the evaluation founds a variable with no associated value, it returns an error else it returns the evaluated value.
  /// </returns>
  /// 
  /// <example>
  ///   <code>
  ///     open Utilities.Errors    
  /// 
  ///     let tree = ... in 
  ///     let vals = [ ... ] in
  ///     match eval_tree tree vals with
  ///     | Ok d -> printfn "The value of the tree is %d" d
  ///     | Error e -> printfn "Something went wrong %s" (error_to_string e)
  ///   </code>
  /// </example>
  let eval_tree (tree: Tree) (vals: list<string * double>) : Result<double, Utils.Errors.Error> = 
    Logic.eval_tree tree vals
