// ----------------------------------------------------------------------------
// 06 - Add support for more elegant programs with GOSUB
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
  | Print of Expression list
  | Input of string 
  | Stop
  // NOTE: Add the GOSUB jump and RETURN commands
  | GoSub of int
  | Return

type State = 
  { Program : list<int * Command> 
    Variables : Map<string, Value> 
    Random : Random 
    // TODO: Add a stack of line numbers to return to (list<int>)
    Stack : list<int> }

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
  | Input(var) ->
      let input = Console.ReadLine()
      match Int32.TryParse input with
      | true, value -> runCommand state (line, Assign(var, Const(NumberValue(value))))
      | false, _ -> runCommand state (line, Goto(line))
  | Stop -> state
  // TODO: GOSUB needs to store the current line number on the stack for
  // RETURN (before behaving as GOTO); RETURN pops a line number from the
  // stack and runs the line after the one from the stack.
  | GoSub(i) -> runCommand { state with Stack = line :: state.Stack } (line, Goto(i))
  | Return ->
      match state.Stack with
      | line :: stack -> runNextLine { state with Stack = stack } line
      | _ -> failwith "empty stack"

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

// TODO: Add empty stack of return line numbers here
let empty = { Program = []; Variables = Map.empty; Random = Random(); Stack = [] }

let nim = 
  [ Some 10, Assign("M", num 20)
    Some 20, Assign("U", num 1)
    Some 30, GoSub(100)
    Some 40, Assign("U", num 2)
    Some 50, GoSub(100)
    Some 60, Goto(20) 
    Some 100, Print [ str "THERE ARE "; var "M"; str " MATCHES LEFT\n" ]
    Some 110, Print [ str "PLAYER "; var "U"; str ": YOU CAN TAKE BETWEEN 1 AND "; 
      Function("MIN", [num 5; var "M"]); str " MATCHES\n" ]
    Some 120, Print [ str "HOW MANY MATCHES DO YOU TAKE?\n" ]
    Some 130, Input("P")
    Some 140, If((var "P" .< num 1) .|| (var "P" .> num 5) .|| (var "P" .> var "M"), Goto 120)
    Some 150, Assign("M", var "M" .- var "P")
    Some 160, If(var "M" .= num 0, Goto 200)
    Some 170, Return    
    Some 200, Print [str "PLAYER "; var "U"; str " WINS!"]
    None, Run
  ]

runInputs empty nim |> ignore
