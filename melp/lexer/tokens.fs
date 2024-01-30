namespace Lexer 


/// <summary>
/// This module contains all the code to process a string to a list of Token.
/// </summary>
/// 
/// <remarks>
/// All the logic is nested in a private sub-module.
/// </remarks>
module Tokens = 


  /// <summary>
  /// The different tokens.
  /// </summary>
  type Token = 
  | Plus
  | Minus
  | Times
  | Divide
  | LPar
  | RPar
  | Int of int
  | Float of float
  | Id of string


  /// <summary>
  /// This module contains element for debugging purposes.
  /// </summary>
  module private Debug = 


    /// <summary>
    /// Converts a token to a string.
    /// </summary>
    /// 
    /// <param name="token">A token</param>
    /// 
    /// <returns>A string.</returns>
    /// 
    /// <example>
    ///   <code>
    ///     let token = ... in
    ///     let str = token_to_string str in
    ///     printfn "Token : %s" token
    ///   </code>
    /// </example>
    let token_to_string (token: Token) : string =
      match token with
      | Plus -> "<PLUS>"
      | Minus -> "<MINUS>"
      | Times -> "<TIMES>"
      | Divide -> "<DIVIDE>"
      | LPar -> "<LPAR>"
      | RPar -> "<RPAR>"
      | Int i -> sprintf "<INT<%i>>" i
      | Float f -> sprintf "<FLOAT<%f>>" f
      | Id i -> sprintf "<ID<%s>>" i


    /// <summary>
    /// Converts a list of tokens to a string.
    /// </summary>
    /// 
    /// <param name="tokens">The list of tokens.</param>
    /// 
    /// <returns>A string.</returns>
    /// 
    /// <example>
    ///   <code>
    ///     let tokens = [ ... ] in
    ///     let str = tokens in 
    ///     printfn "Tokens : %s" tokens
    ///   </code>
    /// </example>
    let tokens_to_string (tokens: list<Token>) : string = 
      let rec concat (str: list<string>) (otp: string) : string = 
        match str with
        | [] -> otp
        | [s] -> otp + " " + s
        | h :: t -> concat t (otp + " " + h)
      in
      concat (List.map (fun t -> token_to_string t) tokens) ""


  /// <summary>
  /// This module contains element for comparing tokens and process some custom conditions.
  /// </summary>
  module public Validators = 
    

    /// <summary>
    /// Checks if the token is an operator.
    /// </summary>
    /// 
    /// <param name="token">A token.</param>
    /// 
    /// <returns>A boolean.</returns>
    /// 
    /// <example>
    ///   <code>
    ///     let token = ... in
    ///     if is_operator token 
    ///       then printfn "The token is an operator."
    ///       else printfn "The token is not an operator."
    ///   </code>
    /// </example>
    let is_operator (token: Token) : bool = 
      match token with
      | Plus | Minus | Times | Divide -> true
      | _ -> false 


    /// <summary>
    /// Checks if the token is a value.
    /// </summary>
    /// 
    /// <param name="token">A token.</param>
    /// 
    /// <returns>A boolean.</returns>
    /// 
    /// <example>
    ///   <code>
    ///     let token = ... in
    ///     if is_value token 
    ///       then printfn "The token is a value."
    ///       else printfn "The token is not a value."
    ///   </code>
    /// </example>
    let is_value (token: Token) : bool = 
      match token with
      | Id _ | Int _ | Float _ -> true
      | _ -> false

    
    /// <summary>
    /// Checks if the char is an escape char.
    /// </summary>
    /// 
    /// <param name="c">A char.</param>
    /// 
    /// <returns>A boolean.</returns>
    /// 
    /// <example>
    ///   <code>
    ///     let c = ... in 
    ///      if is_escape_char c 
    ///       then printfn "'%c' is an escape char" c
    ///       else printfn "'%c' is not an escape char"
    ///   </code>
    /// </example>
    let is_escape_char (c: char) : bool = 
      List.contains c [' '; '\t'; '\n'; '\r']


  /// <summary>
  /// This module contains all the logic.
  /// </summary>  
  module private Logic = 

    open Utils.Errors;
    open System.Text.RegularExpressions


    /// <summary>
    /// The integer regex.
    /// </summary>
    let int_regex = "^[0-9]*$"


    /// <summary>
    /// The float regex.
    /// </summary>
    let float_regex = "^[0-9]+([.,][0-9]+)?$"


    /// <summary>
    /// The identifier regex.
    /// </summary>
    let id_regex = "^[a-z]+$"


    /// <summary>
    /// Attempts to convert a string to an atomic token.
    /// </summary>
    /// 
    /// <param name="str">The string.</param>
    /// 
    /// <returns>
    /// If the string is successfully converted it returns the token else it returns None.
    /// </returns>
    /// 
    /// <example>
    ///   <code>
    ///     let str = "..." in
    ///     match string_to_atomic_token str with
    ///     | None -> printfn "The string is not an atomic token."
    ///     | Some _ -> printfn "The string is an atomic token."
    ///   </code>
    /// </example>
    let string_to_atomic_token (str: string) : Option<Token> = 
      match str with
      | "("   -> Some LPar
      | ")"   -> Some RPar
      | "+"   -> Some Plus
      | "-"   -> Some Minus
      | "/"   -> Some Divide
      | "*"   -> Some Times
      | _ -> None


    /// <summary>
    /// Attempts to convert a string to a value token.
    /// </summary>
    /// 
    /// <param name="str">The string.</param>
    /// 
    /// <returns>
    /// If the string is successfully converted it returns the token else it returns None.
    /// </returns>
    /// 
    /// <example>
    ///   <code>
    ///     let str = "..." in
    ///     match string_to_value str with
    ///     | None -> printfn "The string is not a value."
    ///     | Some _ -> printfn "The string is a value."
    ///   </code>
    /// </example>
    let string_to_value (str: string) : Option<Token> = 
      match str with 
      | "+" | "-" | "*" | "/" | "(" | ")" -> None
      | "sin" | "cos" | "tan" -> None
      | _ -> 
        if Regex.Match(str, int_regex).Success then Some(Int(str |> int))
        else if Regex.Match(str, float_regex).Success then Some (
          Float((Utils.Utilities.replace_char_in_string str ',' '.') |> float )
        )
        else if Regex.Match(str, id_regex).Success then Some(Id(str))
        else None


    /// <summary>
    /// Attempts to convert a string to a token.
    /// </summary>
    /// 
    /// <param name="str">The string.</param>
    /// 
    /// <returns>
    /// If the string is successfully converted it returns the token else it returns None.
    /// </returns>
    /// 
    /// <example>
    ///   <code>
    ///     let str = "..." in 
    ///     match string_to_token str with
    ///     | None -> printfn "The string is not a token"
    ///     | Some _ -> printfn "The string is a token"
    ///   </code>
    /// </example>
    let string_to_token (str: string) : Option<Token> = 
      match string_to_atomic_token str with
      | None -> string_to_value str
      | Some tk -> Some tk
      

    /// <summary>
    /// Attempts to convert a list of chars to a list of tokens.
    /// </summary>
    /// 
    /// <remarks>
    /// This function ensures that no unknown sequence of char is in the expression.
    /// </remarks>
    /// 
    /// <param name="chars">The list of char.</param>
    /// <param name="buff">A temporary buffer.</param>
    /// <param name="output">The tokens processed.</param>
    /// 
    /// <returns>
    /// If the function detects an unknown sequence of char, it returns an error else it returns a list of tokens.
    /// </returns>
    /// 
    /// <example>
    ///   <code>
    ///     open Utilities.Errors
    /// 
    ///     let chars = [ ... ] in
    ///     match chars_to_tokens chars [] [] with
    ///     | Error e -> printfn "Something went wrong : %s." (error_to_string e)
    ///     | Ok _ -> printfn "Successfull parsing."
    ///   </code>
    /// </example>
    let rec chars_to_tokens (chars: list<char>) (buff: list<char>) (output: list<Token>) : Result<list<Token>, Error> = 
      match chars, buff with
      | [], [] -> if output.Length = 0 then Error EmptyInput else Ok (List.rev output)
      | [], _ -> 
        let b = buff |> List.rev |> Utils.Utilities.char_list_to_string in
        match string_to_token b with
        | Some tk -> tk :: output |> List.rev |> Ok
        | _ -> Error (UnknownSequence b)
      | h :: t, [] -> 
        if Validators.is_escape_char h 
          then chars_to_tokens t buff output
          else
            match string_to_atomic_token (h |> string) with
            | None -> chars_to_tokens t [h] output
            | Some tk -> chars_to_tokens t [] (tk :: output)
      | h :: t, _ -> 
        let b = buff |> List.rev |> Utils.Utilities.char_list_to_string in 
        if Validators.is_escape_char h then
          match string_to_token b  with
          | Some tk -> chars_to_tokens t [] (tk :: output) 
          | None -> Error (UnknownSequence b)
        else 
          match string_to_atomic_token (h |> string) with
          | Some tk -> 
            match string_to_token b with
            | Some tk' -> chars_to_tokens t [] (tk :: tk':: output)
            | None -> 
              Error (UnknownSequence b)
          | None -> 
            chars_to_tokens t (h :: buff) output


  /// <summary>
  /// Attempts to convert a string to a list of tokens.
  /// </summary>
  /// 
  /// <remarks>
  /// This function ensures that no unknown sequence of char is in the expression.
  /// </remarks>
  /// 
  /// <param name="str">The string.</param>
  /// 
  /// <returns>
  /// If the function detects an unknown sequence of char, it returns an error else it returns a list of tokens.
  /// </returns>
  /// 
  /// <example>
  ///   <code>
  ///     open Utilities.Errors
  /// 
  ///     let str = "..." in
  ///     match chars_to_tokens str with
  ///     | Error e -> printfn "Something went wrong : %s." (error_to_string e)
  ///     | Ok _ -> printfn "Successfull parsing."
  ///   </code>
  /// </example>
  let string_to_tokens (str: string) : Result<List<Token>, Utils.Errors.Error> = 
    Logic.chars_to_tokens (Seq.toList str) [] []


  /// <summary>
  /// Converts a token to a string.
  /// </summary>
  /// 
  /// <param name="token">The token.</param>
  /// 
  /// <result>A string.</result>
  /// 
  /// <example>
  ///   <code>
  ///     let token = ... in
  ///     let str = token_to_string token in 
  ///     printfn "Token : %s" str
  ///   </code>
  /// </example>
  let token_to_string (token: Token) : string = 
    Debug.token_to_string token  


  /// <summary>
  /// Converts a list of tokens to a string.
  /// </summary>
  /// 
  /// <param name="tokens">The list of tokens.</param>
  /// 
  /// <result>A string.</result>
  /// 
  /// <example>
  ///   <code>
  ///     let tokens = [ ... ] in
  ///     let str = tokens_to_string tokens in 
  ///     printfn "Tokens : %s" str
  ///   </code>
  /// </example>
  let tokens_to_string (tokens: List<Token>) : string = 
    Debug.tokens_to_string tokens 
