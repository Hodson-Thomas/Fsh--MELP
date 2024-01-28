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

  let intersect_list (list1: list<'a>) (list2: list<'a>) : list<'a> = 
    let rec aux (list1': list<'a>) (list2': list<'a>) (output: list<'a>) : list<'a> = 
      match list1', list2' with
      | [], [] -> output
      | h :: t, [] -> 
        if List.contains h output 
          then aux t [] output 
          else aux t [] (h :: output)
      | [], h :: t -> 
        if List.contains h output
          then aux [] t output
          else aux [] t (h :: output)
      | h :: t, h' :: t' ->
        let output' = if List.contains h output then output else (h :: output) in
        if List.contains h' output' 
          then aux t t' output'
          else aux t t' (h' :: output') 
    in
    aux list1 list2 []


  let replace_char_in_string (str: string) (old': char) (new': char) : string = 
    let rec aux (str': list<char>) (res: list<char>) : string = 
      match str' with
      | [] -> char_list_to_string (List.rev res) 
      | h :: t -> 
        if h = old' then aux t (new' :: res)
        else aux t (h :: res)
    in 
    aux (Seq.toList str) []
