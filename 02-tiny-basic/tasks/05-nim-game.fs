// ----------------------------------------------------------------------------
// 05 - A few more functions and operators
// ----------------------------------------------------------------------------
module TinyBASIC

open System

type Value =
  | StringValue of string
  | NumberValue of int
  | BoolValue of bool

type Expression = 
  | Const of Value
  | Function of string * Expression list
  | Variable of string

type Command = 
  | Run 
  | Goto of int
  | Assign of string * Expression
  | If of Expression * Command
  | Clear
  | Poke of Expression * Expression * Expression
  // NOTE: Input("X") reads a number from console and assigns it to X;
  // Stop terminates the program; I also modified Print to take a list of
  // expressions instead of just one (which is what C64 supports too).
  | Print of Expression list
  | Input of string 
  | Stop

type State = 
  { Program : list<int * Command> 
    Variables : Map<string, Value> 
    Random : Random }

// ----------------------------------------------------------------------------
// Utilities
// ----------------------------------------------------------------------------

let printValue value = 
  match value with
  | StringValue(s) -> printf "%s" s
  | NumberValue(n) -> printf "%i" n
  | BoolValue(b) -> printf "%b" b

let getLine state line =
  let res = state.Program |> List.tryFind (fun (x, y) -> x = line)
  match res with
  | Some(value) -> value
  | None -> failwith "line not found"

let addLine state (line, cmd) =
  state.Program
    |> List.filter (fun (x, y) -> x <> line)
    |> List.append [(line, cmd)]
    |> List.sortBy (fun (x, y) -> x)

// ----------------------------------------------------------------------------
// Evaluator
// ----------------------------------------------------------------------------

let binaryRelOp f args = 
  match args with 
  | [NumberValue a; NumberValue b] -> BoolValue(f a b)
  | _ -> failwith "expected two numerical arguments"

let binaryLogicOp f args = 
  match args with 
  | [BoolValue a; BoolValue b] -> BoolValue(f a b)
  | _ -> failwith "expected two boolean arguments"

let binaryNumOp f args = 
  match args with 
  | [NumberValue a; NumberValue b] -> NumberValue(f a b)
  | _ -> failwith "expected two numerical arguments"

let rec evalExpression expr state = 
  // TODO: We need an extra function 'MIN' that returns the smaller of
  // the two given numbers (in F#, the function 'min' does exactly this.)
  match expr with
  | Const(value) -> value
  | Function(op, [e]) ->
      let v = evalExpression e state
      match (op, v) with
      | ("RND", NumberValue(n)) -> NumberValue(state.Random.Next(n))
      | _ -> failwith "unsupported unary function"
  | Function(op, [e1; e2]) -> 
      let v1 = evalExpression e1 state
      let v2 = evalExpression e2 state
      match (op, v1, v2) with
      | ("-", _, _) -> binaryNumOp (fun x y -> x - y) [v1; v2]
      | ("=", _, _) -> BoolValue(v1 = v2)
      | ("||", _, _) -> binaryLogicOp (fun x y -> x || y) [v1; v2]
      | ("<", _, _) -> binaryRelOp (fun x y -> x < y) [v1; v2]
      | (">", _, _) -> binaryRelOp (fun x y -> x > y) [v1; v2]
      | ("MIN", _, _) -> binaryNumOp (fun x y -> min x y) [v1; v2]
      | _ -> failwith "unsupported binary function"
  | Variable(s) -> state.Variables[s]
  | _ -> failwith "invalid expression"

let rec runCommand state (line, cmd) =
  match cmd with 
  | Run ->
      let first = List.head state.Program    
      runCommand state first
  | Print(expr_list) ->
      match expr_list with
      | expr :: rest ->
          printValue (evalExpression expr state)
          runCommand state (line, Print(rest))
      | [] -> runNextLine state line
  | Goto(line) -> runCommand state (getLine state line)
  | Assign(var, e) ->
      let value = evalExpression e state
      let vars = Map.add var value state.Variables
      runNextLine { state with Variables = vars } line
  | If(expr, then_cmd) ->
      let res = evalExpression expr state
      match res with
      | BoolValue(true) -> runCommand state (line, then_cmd)
      | BoolValue(false) -> runNextLine state line
      | _ -> failwith "invalid condition"
  | Clear ->
      Console.Clear()
      runNextLine state line
  | Poke(e1, e2, e3) ->
      let v1 = evalExpression e1 state
      let v2 = evalExpression e2 state
      let v3 = evalExpression e3 state
      match (v1, v2, v3) with
      | NumberValue(x), NumberValue(y), StringValue(s) ->
          Console.CursorLeft <- x
          Console.CursorTop <- y
          Console.Write(s)
          runNextLine state line
      | _ -> failwith "invalid position"
  // TODO: Input("X") should read a number from the console using Console.RadLine
  // and parse it as a number using Int32.TryParse (retry if the input is wrong)
  // Stop terminates the execution (you can just return the 'state'.)
  | Input(var) ->
      let input = Console.ReadLine()
      match Int32.TryParse input with
      | true, value -> runCommand state (line, Assign(var, Const(NumberValue(value))))
      | false, _ -> runCommand state (line, Goto(line))
  | Stop -> state

and runNextLine state line =
  let head = state.Program |> List.filter (fun (x,y) -> x > line) |> List.sort |> List.tryHead
  match head with
  | Some(value) -> runCommand state value
  | None -> state

// ----------------------------------------------------------------------------
// Interactive program editing
// ----------------------------------------------------------------------------

let runInput state (line, cmd) = 
  match line with
  | Some(ln) -> addLine state (ln, cmd)
  | None -> (runCommand state (Int32.MaxValue, cmd)).Program

let runInputs state cmds = (state, cmds) ||> List.fold (fun st cmd -> { state with Program = runInput st cmd })

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

let num v = Const(NumberValue v)
let str v = Const(StringValue v)
let var n = Variable n
let (.||) a b = Function("||", [a; b])
let (.<) a b = Function("<", [a; b])
let (.>) a b = Function(">", [a; b])
let (.-) a b = Function("-", [a; b])
let (.=) a b = Function("=", [a; b])
let (@) s args = Function(s, args)

let empty = { Program = []; Variables = Map.empty; Random = System.Random() }

// NOTE: A simple game you should be able to run now! :-)
let nim = 
  [ Some 10, Assign("M", num 20)
    Some 20, Print [ str "THERE ARE "; var "M"; str " MATCHES LEFT\n" ]
    Some 30, Print [ str "PLAYER 1: YOU CAN TAKE BETWEEN 1 AND "; 
      "MIN" @ [num 5; var "M"]; str " MATCHES\n" ]
    Some 40, Print [ str "HOW MANY MATCHES DO YOU TAKE?\n" ]
    Some 50, Input("P")
    Some 60, If((var "P" .< num 1) .|| (var "P" .> num 5) .|| (var "P" .> var "M"), Goto 40)
    Some 70, Assign("M", var "M" .- var "P")
    Some 80, If(var "M" .= num 0, Goto 200)
    Some 90, Print [ str "THERE ARE "; var "M"; str " MATCHES LEFT\n" ]
    Some 100, Print [ str "PLAYER 2: YOU CAN TAKE BETWEEN 1 AND "; 
      "MIN" @ [num 5; var "M"]; str " MATCHES\n" ]
    Some 110, Print [ str "HOW MANY MATCHES DO YOU TAKE?\n" ]
    Some 120, Input("P")
    Some 130, If((var "P" .< num 1) .|| (var "P" .> num 5) .|| (var "P" .> var "M"), Goto 110)
    Some 140, Assign("M", var "M" .- var "P")
    Some 150, If(var "M" .= num 0, Goto 220)
    Some 160, Goto 20
    Some 200, Print [str "PLAYER 1 WINS!"]
    Some 210, Stop
    Some 220, Print [str "PLAYER 2 WINS!"]
    Some 230, Stop
    None, Run
  ]

runInputs empty nim |> ignore
