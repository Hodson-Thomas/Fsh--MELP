namespace Utilities


/// <summary>
/// This module contains all the code to print out messages on the console.
/// </summary>
/// 
/// <remarks>
/// All the logic is nested in a private sub-module.
/// </remarks>
module Printer = 


  /// <summary>
  /// Indicates if Errors and Success messages are displayed.
  /// </summary>
  let DEBUG = false


  /// <summary>
  /// Describes the console's available foreground colors.
  /// </summary>
  type Color = 
  | Black
  | Red 
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White


  /// <summary>
  /// Describes the differents messages status.
  /// </summary>
  type Status = 
  | Info
  | Error
  | Success
  | Normal


  /// <summary>
  /// This module contains all the logic.
  /// </summary>  
  module private Logic = 


    /// <summary>
    /// Returns the header of a message depending on the given status.
    /// </summary>
    /// 
    /// <param name="status">The message's status.</param>
    /// 
    /// <returns>A string.</returns>
    /// 
    /// <example>
    ///   <code>
    ///     let status = ... in
    ///     let str = base_of_status status in 
    ///     printfn "Status : %s" str
    ///   </code>
    /// </example>
    let base_of_status (status: Status) : string = 
      match status with
      | Info -> "[INFO] > "
      | Error -> "[ERROR] > "
      | Success -> "[SUCCESS] > "
      | Normal -> ""


    /// <summary>
    /// Converts the given color to a System.ConsoleColor object.
    /// </summary>
    /// 
    /// <remarks>
    /// This function returns the console's default color to reset the message after it has been printed-out.
    /// </remarks>
    /// 
    /// <param name="color">The message's color.</param>
    /// 
    /// <returns>A tuple of System.ConsoleColor.</returns>
    /// 
    /// <example>
    ///   <code>
    ///     let color = ... in 
    ///     let new_fg, reset_fg = get_color_code_wrapper color
    ///   </code>
    /// </example>
    let get_color_code_wrapper (color: Color) : (System.ConsoleColor * System.ConsoleColor) = 
      let code = 
        match color with
        | Black -> System.ConsoleColor.Black
        | Red -> System.ConsoleColor.Red
        | Green -> System.ConsoleColor.Green
        | Yellow -> System.ConsoleColor.Yellow
        | Blue -> System.ConsoleColor.Blue
        | Magenta -> System.ConsoleColor.Magenta
        | Cyan -> System.ConsoleColor.Cyan
        | White -> System.ConsoleColor.White
      in (code, System.ConsoleColor.White)

    
    /// <summary>
    /// Converts a status to a color.
    /// </summary>
    /// 
    /// <param name="status">The status.</param>
    /// 
    /// <returns>A color.</returns>
    /// 
    /// <example>
    ///   <code>
    ///     let status = ... in
    ///     let color = color_of_status status
    ///   </code>
    /// </example>
    let color_of_status (status: Status) : Color = 
      match status with
      | Success -> Green
      | Error -> Red
      | Info -> Blue
      | _ -> White


    /// <summary>
    /// Prints the given message with the status' header and the status' color.
    /// </summary>
    /// 
    /// <param name="status">The message status.</param>
    /// <param name="str">The message.</param>
    /// 
    /// <example>
    ///   <code>
    ///     let status = ... in
    ///     let str = "..." in 
    ///     print_with_status status str
    ///   </code>
    /// </example>
    let print_with_status (status: Status) (str: string) : unit = 
      let base' = base_of_status status in 
      let clr, reset = color_of_status status |> get_color_code_wrapper in 
      let _ = System.Console.ForegroundColor<-clr in 
      let _ = printfn "%s" (base' + str + " ") in
      let _ = System.Console.ForegroundColor<-reset in
      ()


  /// <summary>
  /// Prints the given message with its status' configuration. 
  /// </summary>
  /// 
  /// <remarks>If the DEBUG flag isn't active, only Info & Normal status messages are printed out.</remarks>
  /// 
  /// <param name="status">The message status.</param>
  /// <param name="msg">The message.</param>
  /// 
  /// <example>
  ///   <code>
  ///     let status = ... in
  ///     let msg = "..." in
  ///     print status msg
  ///   </code>
  /// </example>
  let print (status: Status) (msg: string) : unit = 
    match status, DEBUG with
    | Info, _ |  Normal, _ -> Logic.print_with_status status msg
    | _, true -> Logic.print_with_status status msg
    | _, false -> ()