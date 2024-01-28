namespace Utilities

module Printer = 

  let esc = string (char 0x1B)

  let DEBUG = false

  type Color = 
  | Black
  | Red 
  | Green
  | Yellow
  | Blue
  | Magenta
  | Cyan
  | White

  type Status = 
  | Info
  | Error
  | Success
  | Normal

  module private Logic = 

    let base_of_status (status: Status) : string = 
      match status with
      | Info -> "[INFO] > "
      | Error -> "[ERROR] > "
      | Success -> "[SUCCESS] > "
      | Normal -> ""

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

    
    let color_of_status (status: Status) : Color = 
      match status with
      | Success -> Green
      | Error -> Red
      | Info -> Blue
      | _ -> White


    let print_with_status (status: Status) (str: string) : unit = 
      let base' = base_of_status status in 
      let clr, reset = color_of_status status |> get_color_code_wrapper in 
      let _ = System.Console.ForegroundColor<-clr in 
      let _ = printfn "%s" (base' + str + " ") in
      let _ = System.Console.ForegroundColor<-reset in
      ()


  let print (status: Status) (msg: string) : unit = 
    match status, DEBUG with
    | Info, _ |  Normal, _ -> Logic.print_with_status status msg
    | _, true -> Logic.print_with_status status msg
    | _, false -> ()