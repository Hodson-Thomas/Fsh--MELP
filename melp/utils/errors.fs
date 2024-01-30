namespace Utils


/// <summary>
/// This module contains all the code to handle errors.
/// </summary>
module Errors = 

  /// <summary>
  /// Describes all lexer's and plotter's errors. 
  /// </summary>
  type Error = 
  | EmptyInput
  | MissingParanthesis
  | ExtractNegateFailed
  | UnknownSequence of string
  | InvalidPattern of string
  | ParsingError of string
  | EvalError of string
  | NonContinueError of string


  /// <summary>
  /// Converts an error to a string.
  /// </summary>
  /// 
  /// <param name="error">The error.</param>
  /// 
  /// <returns>A string.</returns>
  /// 
  /// <example>
  ///   <code>
  ///     let error = ... in
  ///     let str = error_to_string error in 
  ///     printfn "Error : %s" e
  ///   </code>
  /// </example>
  let error_to_string (error: Error) : string = 
    match error with
    | EmptyInput -> "Empty sequence given"
    | MissingParanthesis -> "Missing paranthesis"
    | ExtractNegateFailed -> "Extract negate error"
    | UnknownSequence s -> sprintf "Unknown char sequence found : %s" s 
    | InvalidPattern s -> sprintf "Encountered an invalid pattern : %s" s
    | ParsingError s -> sprintf "Encountered an error while parsing : %s" s
    | EvalError s -> sprintf "Encountered an error while evaluating : %s" s
    | NonContinueError s -> sprintf "Some values are not valid : %s" s