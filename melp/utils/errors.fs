namespace Utils

module Errors = 

  type Error = 
  | EmptyInput
  | MissingParanthesis
  | ExtractNegateFailed
  | UnknownSequence of string
  | InvalidPattern of string
  | ParsingError of string
  | EvalError of string
  | NonContinueError of string


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