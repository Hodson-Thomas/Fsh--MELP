namespace GUI

module Plotting = 

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

  module private Debug = 

    let plotting_data_to_string (data: PlottingData) : string = 
      let tree = Lexer.Parser.tree_to_string data.tree in
      let vals = List.fold (fun a b -> a + ", " + b) "" [ 
        for (name, value) in data.vals do 
        "(" + name + ", " + (value |> string) + ")"
      ] in
      let target = sprintf "%s (min=%e, max=%e, step=%e)" data.target data.xmin data.xmax data.step in
      let continue' = "AllowContinueValues=" + (data.allowNonContinue |> string) in
      sprintf "[PLOTTING DATA] : \n\ttarget=%s\n\tvals=%s\n\t%s\n\ttitle=\n\ttree=%s" target vals continue' tree



  module private Logic = 

    open Utils.Errors
    open Plotly.NET

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


    let convert_list_tuple_to_tuple_list (vals: list<double * double>) : list<double> * list<double> =
      let rec aux (vals': list<double * double>) (x: list<double>) (y: list<double>) : list<double> * list<double> = 
        match vals' with
        | [] -> x, y
        | (x', y') :: t -> aux t (x' :: x) (y' :: y)
      in 
      aux vals [] []
    

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

  let plotting_data_to_string (data: PlottingData) : string = 
    Debug.plotting_data_to_string data


  ///<summary>
  ///</summary>
  let plot_function (context: PlottingData) : Result<unit, Utils.Errors.Error> = 
    Logic.plot_tree context