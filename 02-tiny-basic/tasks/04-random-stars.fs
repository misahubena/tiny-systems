// ----------------------------------------------------------------------------
// 04 - Random function and (not quite correct) POKE
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
  | Print of Expression
  | Run 
  | Goto of int
  | Assign of string * Expression
  | If of Expression * Command
  // NOTE: Clear clears the screen and Poke(x, y, e) puts a string 'e' at 
  // the console location (x, y). In C64, the actual POKE writes to a given
  // memory location, but we only use it for screen access here.
  | Clear
  | Poke of Expression * Expression * Expression

type State = 
  { Program : list<int * Command> 
    Variables : Map<string, Value> 
    // TODO: You will need to include random number generator in the state!
    Random : Random
    }

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

// NOTE: Helper function that makes it easier to implement '>' and '<' operators
// (takes a function 'int -> int -> bool' and "lifts" it into 'Value -> Value -> Value')
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
  // TODO: Add support for 'RND(N)' which returns a random number in range 0..N-1
  // and for binary operators ||, <, > (and the ones you have already, i.e., - and =).
  // To add < and >, you can use the 'binaryRelOp' helper above. You can similarly
  // add helpers for numerical operators and binary Boolean operators to make
  // your code a bit nicer.
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
      | _ -> failwith "unsupported binary function"
  | Variable(s) -> state.Variables[s]
  | _ -> failwith "invalid expression"

let rec runCommand state (line, cmd) =
  match cmd with 
  | Run ->
      let first = List.head state.Program    
      runCommand state first
  | Print(expr) ->
      printValue (evalExpression expr state)
      runNextLine state line
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
  
  // TODO: Implement two commands for screen manipulation
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

// NOTE: Writing all the BASIC expressions is quite tedious, so this is a 
// very basic (and terribly elegant) trick to make our task a bit easier.
// We define a couple of shortcuts and custom operators to construct expressions.
// With these, we can write e.g.: 
//  'Function("RND", [Const(NumberValue 100)])' as '"RND" @ [num 100]' or 
//  'Function("-", [Variable("I"); Const(NumberValue 1)])' as 'var "I" .- num 1'
let num v = Const(NumberValue v)
let str v = Const(StringValue v)
let var n = Variable n
let (.||) a b = Function("||", [a; b])
let (.<) a b = Function("<", [a; b])
let (.>) a b = Function(">", [a; b])
let (.-) a b = Function("-", [a; b])
let (.=) a b = Function("=", [a; b])
let (@) s args = Function(s, args)

let empty = { Program = []; Variables = Map.empty; Random = new Random() } // TODO: Add random number generator!

// NOTE: Random stars generation. This has hard-coded max width and height (60x20)
// but you could use 'System.Console.WindowWidth'/'Height' here to make it nicer.
let stars = 
  [ Some 10, Clear
    Some 20, Poke("RND" @ [num 60], "RND" @ [num 20], str "*")
    Some 30, Assign("I", num 100)
    Some 40, Poke("RND" @ [num 60], "RND" @ [num 20], str " ")
    Some 50, Assign("I", var "I" .- num 1)
    Some 60, If(var "I" .> num 1, Goto(40)) 
    Some 100, Goto(20)
    None, Run
  ]

// NOTE: Make the cursor invisible to get a nicer stars animation
System.Console.CursorVisible <- false
runInputs empty stars |> ignore
