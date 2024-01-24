namespace Utils

module Utilities = 
  
  let rec any_err (list: list<Result<'a, Errors.Error>>) : bool = 
    match list with
    | [] -> false
    | h :: t -> 
      match h with
      | Error _ -> true 
      | Ok _ -> any_err t

    
  let unwrap (elem: Result<'a, Errors.Error>) : 'a =
    match elem with 
    | Error e -> failwith "Something went wrong : Error variant found"
    | Ok e -> e


  let char_list_to_string (chars: list<char>) : string = 
    let rec aux (chars': list<char>) (otp: string) : string = 
      match chars' with 
      | [] -> otp
      | h :: t -> aux t (otp + (h |> string))
    in
    aux chars ""
