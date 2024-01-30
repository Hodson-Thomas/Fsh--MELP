namespace GUI

/// <summary>
/// This module contains all the code to plot the expression.
/// </summary>
/// 
/// <remarks>
/// All the logic is nested in a private sub-module.
/// </remarks>
module Plotting = 


  /// <summary>
  /// This type represents all the data required to plot an expression. 
  /// </summary>
  /// 
  /// <param name="tree">The expression to plot.</param>
  /// <param name="target">The identifer to vary.</param>
  /// <param name="target">The identifer to vary.</param>
  /// <param name="vals">All the other identifiers with a fixed value.</param>
  /// <param name="xmin">The variable's min value.</param>
  /// <param name="xmax">The variable's max value.</param>
  /// <param name="step">The variable's increment value.</param>
  /// <param name="stallowNonContinueep">Indicates if the function is plotted if the function is non-continue.</param>
  /// <param name="title">The chart's title.</param>
  type PlottingData = 
    {
      tree: Lexer.Parser.Tree
      target: string
      vals: list<string * double>
      xmin: double
      xmax: double
      step: double
      allowNonContinue: bool
      title: string
    }


  /// <summary>
  /// This module contains element for debugging purposes.
  /// </summary>
  module private Debug = 


    /// <summary>
    /// Converts the plotting data to a string-like representation.
    /// </summary>
    /// 
    /// <param name="data">The plotting data.</p>
    /// 
    /// <returns>A string</returns>
    /// 
    /// <example>
    ///   <code>
    ///     let data = {...} in
    ///     let str_data = plotting_data_to_string data in
    ///     printfn "%s" str_data
    ///   </code>
    /// </example>
    let plotting_data_to_string (data: PlottingData) : string = 
      let tree = Lexer.Parser.tree_to_string data.tree in
      let vals = List.fold (fun a b -> a + ", " + b) "" [ 
        for (name, value) in data.vals do 
        "(" + name + ", " + (value |> string) + ")"
      ] in
      let target = sprintf "%s (min=%e, max=%e, step=%e)" data.target data.xmin data.xmax data.step in
      let continue' = "AllowContinueValues=" + (data.allowNonContinue |> string) in
      sprintf "[PLOTTING DATA] : \n\ttarget=%s\n\tvals=%s\n\t%s\n\ttitle=\n\ttree=%s" target vals continue' tree


  /// <summary>
  /// This module contains all the logic.
  /// </summary>  
  module private Logic = 

    open Utils.Errors
    open Plotly.NET


    /// <summary>
    /// Using the given context, this functions calculates all the points to plot the expression.
    /// </summary>
    ///
    /// <remarks>
    /// If the funciton is non-continue and the context doesn't allow it, the function will return the Error variant.
    /// </remarks>
    /// 
    /// <remarks>
    /// If the funciton is non-continue and the context allow it, the function will return multiple list, each representing a continuous part.
    /// </remarks>
    /// 
    /// <param name="context">The plotting data.</param>
    /// 
    /// <returns>A 2D list of values.</returns>
    /// 
    /// <example>
    ///   <code>
    ///     open Utilities.Errors
    /// 
    ///     let ctx = {...} in
    ///     match calculate_points ctx with
    ///     | Error e -> printfn "Something went wrong : %s" (error_to_string e)
    ///     | Ok lines -> printfn "Calculated %i lines" lines.Length
    ///   </code>
    /// </example>
    let calcultate_points (context: PlottingData) : Result<list<list<double * double>>, Error> = 
      let rec aux (x: double) (buff: list<double * double>) (output: list<list<double * double>>) : Result<list<list<double * double>>, Error> = 
        if (x > context.xmax) && (buff.Length > 0) then buff :: output |> Ok
        else if (x > context.xmax) then output |> Ok else 
        let vals = (context.target, x) :: context.vals in 
        match Lexer.Parser.eval_tree context.tree vals with
        | Error e -> 
          match e with 
          | NonContinueError _ -> 
            let output' = if buff.Length > 0 then (buff :: output) else output in 
            if context.allowNonContinue then aux (x + context.step) [] output' else Error e 
          | _ -> Error e
        | Ok v -> 
          aux (x + context.step) ((x, v) :: buff) output
      in
      aux context.xmin [] []


    /// <summary>
    /// Converts a list of tuples to a tuple of list.
    /// </summary>
    /// 
    /// <param name="vals">A list of tuples (double, double)</param>
    /// 
    /// <return>Returns a tuple of double list.</return>
    /// 
    /// <example>
    ///   <code>
    ///     let vals = convert_list_tuple_to_tuple_list [(1.0, 2.0); (3.0, 4.0); (5.0, 6.0)]
    ///     // vals = ([1.0; 3.0; 5.0], [2.0; 4.0; 6.0]) 
    ///   </code>
    /// </example>
    let convert_list_tuple_to_tuple_list (vals: list<double * double>) : list<double> * list<double> =
      let rec aux (vals': list<double * double>) (x: list<double>) (y: list<double>) : list<double> * list<double> = 
        match vals' with
        | [] -> x, y
        | (x', y') :: t -> aux t (x' :: x) (y' :: y)
      in 
      aux vals [] []
    

    /// <summary>
    /// Attemps to plot the given expression and it's parameters.
    /// </summary>
    /// 
    /// <param name="context">The plotting data.</param>
    /// 
    /// <returns>An error if the program encountered an error while processing the expression.</returns>
    /// 
    /// <example>
    ///   <code>
    ///     open Utilities.Errors
    ///     
    ///     let data = { ... } in
    ///     match plot_tree data with
    ///     | Error _ -> printfn "Something went wrong : %s" (error_to_string e)
    ///     | Ok _ -> printfn "Expression plotted"
    ///   </code>
    /// </example>
    let plot_tree (context: PlottingData) : Result<unit, Error> = 
      match calcultate_points context with
      | Error e -> Error e
      | Ok parts ->
        [
          for part in parts do 
            let x, y = convert_list_tuple_to_tuple_list part in
            Chart.Line(x |> Seq.ofList, y |> Seq.ofList)
        ]
        |> Chart.combine
        |> Chart.withXAxisStyle(context.target)
        |> Chart.withYAxisStyle(sprintf "f(%s)" context.target)
        |> Chart.withTitle(context.title)
        |> Chart.withLineStyle(Width=2.0, Dash=StyleParam.DrawingStyle.Solid)
        |> Chart.show
        |> Ok


  /// <summary>
  /// Converts the given plotting data to a string.
  /// </summary>
  /// 
  /// <param name="context">The plotting data.</param>
  /// 
  /// <return>A string.</returns>
  /// 
  /// <example>
  ///   <code>
  ///     let str = { ... } |> plotting_data_to_string in
  ///     printfn "Plotting data : %s" str
  ///   </code>
  /// </example>
  let plotting_data_to_string (data: PlottingData) : string = 
    Debug.plotting_data_to_string data


  /// <summary>
  /// Attemps to plot the given expression and it's parameters.
  /// </summary>
  /// 
  /// <param name="context">The plotting data.</param>
  /// 
  /// <returns>An error if the program encountered an error while processing the expression.</returns>
  /// 
  /// <example>
  ///   <code>
  ///     open Utilities.Errors
  ///     
  ///     let data = { ... } in
  ///     match plot_function data with
  ///     | Error _ -> printfn "Something went wrong : %s" (error_to_string e)
  ///     | Ok _ -> printfn "Expression plotted"
  ///   </code>
  /// </example>
  let plot_function (context: PlottingData) : Result<unit, Utils.Errors.Error> = 
    Logic.plot_tree context
