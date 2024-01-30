namespace Utils


/// <summary>
/// This module contains all general purposes elements.
/// </summary>
module Utilities = 


  /// <summary>
  /// Checks if the given list contains at least one Error variant.
  /// </summary>
  /// 
  /// <param name="list">The list.</param>
  /// 
  /// <return>A boolean</param>
  /// 
  /// <example>
  ///   <code>
  ///     let list = [ ... ] in 
  ///     if any_err list 
  ///       then printfn "The list contains an error."
  ///       else printfn "The list does not contain an error."
  ///   </code>
  /// </example>
  let rec any_err (list: list<Result<'a, Errors.Error>>) : bool = 
    match list with
    | [] -> false
    | h :: t -> 
      match h with
      | Error _ -> true 
      | Ok _ -> any_err t

  
  /// <summary>
  /// Returns the Ok variant from a Result.
  /// </summary>
  /// 
  /// <remarks>
  /// If an Error variant is found, the program crashes.
  /// </remarks>
  /// 
  /// <param name="elem">The result.</param>
  /// 
  /// <returns>The Ok value.</returns>
  /// 
  /// <example>
  ///   <code>
  ///     let elem = ... in
  ///     let elem' = unwrap elem 
  ///   </code>
  /// </example>
  let unwrap (elem: Result<'a, Errors.Error>) : 'a =
    match elem with 
    | Error e -> failwith "Something went wrong : Error variant found"
    | Ok e -> e


  /// <summary>
  /// Converts a list of char to a string.
  /// </summary>
  /// 
  /// <param name="chars">The list of char.</param>
  /// 
  /// <returns>A string.</returns>
  /// 
  /// <example>
  ///   <code>
  ///     let chars = [ ... ] in 
  ///     let str = char_list_to_string str in
  ///     printfn "Char list converted to : %s" str
  ///   </code>
  /// </example>
  let char_list_to_string (chars: list<char>) : string = 
    let rec aux (chars': list<char>) (otp: string) : string = 
      match chars' with 
      | [] -> otp
      | h :: t -> aux t (otp + (h |> string))
    in
    aux chars ""


  /// <summary>
  /// Returns the intersection of two lists, i.e. all the common elements of both lists.
  /// </summary>
  /// 
  /// <remarks>
  /// The final list will not contain any duplicates.
  /// </remarks>
  /// 
  /// <param name="list1">The first list.</param>
  /// <param name="list1">The second list.</param>
  /// 
  /// <returns>A list.</returns>
  /// 
  /// <example>
  ///   <code>
  ///     let list1 = [ ... ] in 
  ///     let list2 = [ ... ] in
  ///     intersect_list list1 list2
  ///   </code>
  /// </example>
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


  /// <summary>
  /// Replaces all appearance of the targetted char with the new given char.
  /// </summary>
  /// 
  /// <param name="str">The string.</param>
  /// <param name="old'">The targetted char.</param>
  /// <param name="new'">The new char.</param>
  /// 
  /// <returns>A string.</returns>
  /// 
  /// <example>
  ///   <code>
  ///     let str = "..." in
  ///     let old' = '...' in
  ///     let new' = '...' in
  ///     let str' = replace_char_in_string str old' new' in
  ///     printfn "String '%s' has been transformed to %s" str str'
  ///   </code>
  /// </example>
  let replace_char_in_string (str: string) (old': char) (new': char) : string = 
    let rec aux (str': list<char>) (res: list<char>) : string = 
      match str' with
      | [] -> char_list_to_string (List.rev res) 
      | h :: t -> 
        if h = old' then aux t (new' :: res)
        else aux t (h :: res)
    in 
    aux (Seq.toList str) []
