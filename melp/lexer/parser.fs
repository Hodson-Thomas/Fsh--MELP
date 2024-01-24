namespace Lexer

module Parser = 
  
  let continued_tab_base = " ├─"
  let tab_base = " └─"
  let empty_tab = "   "
  let full_tab = " ──"

  type Tree = 
  | Add of Tree * Tree
  | Sub of Tree * Tree
  | Neg of Tree
  | Mul of Tree * Tree
  | Div of Tree * Tree
  | Int of int
  | Float of float
  | Id of string


  module private Debug = 

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


  module private Logic = 

    open Utils.Errors

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

  let parse_ast (ast: Ast.Expr) : Result<Tree, Utils.Errors.Error> = 
    Logic.parse_ast ast

  let tree_to_string (tree: Tree) : string = 
    Debug.tree_to_string tree