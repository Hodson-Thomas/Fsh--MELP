namespace Lexer 

module Tokens = 

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

  module private Debug = 

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

    let tokens_to_string (tokens: list<Token>) : string = 
      let rec concat (str: list<string>) (otp: string) : string = 
        match str with
        | [] -> otp
        | [s] -> otp + " " + s
        | h :: t -> concat t (otp + " " + h)
      in
      concat (List.map (fun t -> token_to_string t) tokens) ""


  module public Validators = 
    
    let is_operator (token: Token) : bool = 
      match token with
      | Plus | Minus | Times | Divide -> true
      | _ -> false 

    let is_value (token: Token) : bool = 
      match token with
      | Id _ | Int _ | Float _ -> true
      | _ -> false

    let is_function (token: Token) : bool =
      false

    let is_escape_char (c: char) : bool = 
      List.contains c [' '; '\t'; '\n'; '\r']


  module private Logic = 

    open Utils.Errors;
    open System.Text.RegularExpressions

    let int_regex = "^[0-9]*$"
    let float_regex = "^[0-9]+([.,][0-9]+)?$"

    let id_regex = "^[a-z]+$"

    let string_to_atomic_token (str: string) : Option<Token> = 
      match str with
      | "("   -> Some LPar
      | ")"   -> Some RPar
      | "+"   -> Some Plus
      | "-"   -> Some Minus
      | "/"   -> Some Divide
      | "*"   -> Some Times
      | _ -> None

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


    let string_to_token (str: string) : Option<Token> = 
      match string_to_atomic_token str with
      | None -> string_to_value str
      | Some tk -> Some tk
      

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

  let string_to_tokens (str: string) : Result<List<Token>, Utils.Errors.Error> = 
    Logic.chars_to_tokens (Seq.toList str) [] []

  let token_to_string (token: Token) : string = 
    Debug.token_to_string token  

  let tokens_to_string (tokens: List<Token>) : string = 
    Debug.tokens_to_string tokens 