namespace Lexer

module Ast =

  type Expr = 
  | Token of Tokens.Token
  | Block of List<Expr>

  module private Debug = 

    let generate_tabs (n: int) : string = 
      let rec aux (m: int) (s: string) : string = 
        if m <= 0 then s else aux (m - 1) (s + "  ") 
      in aux n ""
      
    let rec expr_to_string (expression: Expr) (depth: int) : string = 
      match expression with
      | Token token -> (generate_tabs depth) + (Tokens.token_to_string token) + "\n" 
      | Block block -> 
        List.map (fun expr -> expr_to_string expr (depth + 1)) block 
        |> List.fold (fun s s' -> s + s') ""
    
    let rec expr_to_string_flat (expression: Expr) : string = 
      match expression with
      | Token token -> sprintf "~ %s ~" (Tokens.token_to_string token)
      | Block block -> 
        let str = 
          List.map (fun expr -> expr_to_string_flat expr) block 
          |> List.fold (fun s  s' -> s + s') ""
        in "[ " + str + "]"


  module public Validators = 
    let is_operator (expr: Expr) : bool = 
      match expr with
      | Token t -> Tokens.Validators.is_operator t
      | _ -> false

    let is_value (expr: Expr) : bool = 
      match expr with
      | Token t -> Tokens.Validators.is_value t
      | _ -> true

    let is_function (expr: Expr) : bool = 
      false

    let is_double_operand_pattern_valid (e1: Expr) (e2: Expr) (e3: Expr) : bool = 
      (is_value e1) && (is_operator e2) && (is_value e3)

    let is_single_operand_operation (e1: Expr) (e2: Expr) : bool = 
      (is_operator e1) && (is_value e2)

    let is_expr_token (expr: Expr) (token: Tokens.Token) : bool = 
      match expr with 
      | Block _ -> false
      | Token t -> 
        match t with
        | t when t = token -> true
        | _ -> false     


  module private Logic = 

    open Utils.Errors

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


    let extract_ok (list: list<Result<Expr, Error>>) : list<Expr> = 
      List.map (fun (n: Result<Expr, Error>) ->  match n with | Ok n' -> n' | _ -> failwith "Found Error variant") list


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
              then Error (InvalidPattern "No msg yet")
              else aux [] buff (Some e'')
          | Ok e'', Some left' -> 
            if Validators.is_expr_token e'' operator 
              then Error (InvalidPattern "No msg yet")
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
      
    let extract_times (expr: Result<Expr, Utils.Errors.Error>) : Result<Expr, Utils.Errors.Error> = 
      extract_double_operand_operation expr Tokens.Times

    let extract_divide (expr: Result<Expr, Utils.Errors.Error>) : Result<Expr, Utils.Errors.Error> = 
      extract_double_operand_operation expr Tokens.Divide

    let extract_add (expr: Result<Expr, Utils.Errors.Error>) : Result<Expr, Utils.Errors.Error> = 
      extract_double_operand_operation expr Tokens.Plus

    let extract_minus (expr: Result<Expr, Utils.Errors.Error>) : Result<Expr, Utils.Errors.Error> = 
      extract_double_operand_operation expr Tokens.Minus


  let tokens_to_ast (tokens: list<Tokens.Token>) : Result<Expr, Utils.Errors.Error> =
    tokens 
    |> Logic.extract_paranthesis
    |> Logic.extract_negate
    |> Logic.extract_times
    |> Logic.extract_divide
    |> Logic.extract_add
    |> Logic.extract_minus
    

  let expr_to_string (expression: Expr) : string = 
    Debug.expr_to_string expression 0