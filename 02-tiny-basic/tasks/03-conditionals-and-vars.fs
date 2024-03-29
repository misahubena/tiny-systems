﻿// ----------------------------------------------------------------------------
// 03 - Add variables, conditionals and integer values
// ----------------------------------------------------------------------------
module TinyBASIC

type Value =
  | StringValue of string
  // NOTE: Added numerical and Boolean values
  | NumberValue of int
  | BoolValue of bool

type Expression = 
  | Const of Value
  // NOTE: Added functions and variables. Functions  are used for both 
  // functions (later) and binary operators (in this step). We use only
  // 'Function("-", [e1; e2])' and 'Function("=", [e1; e2])' in the demo.
  | Function of string * Expression list
  | Variable of string

type Command = 
  | Print of Expression
  | Run 
  | Goto of int
  // NOTE: Assign expression to a given variable and conditional that 
  // runs a given Command only if the expression evaluates to 'BoolValue(true)'
  | Assign of string * Expression
  | If of Expression * Command

type State = 
  { Program : list<int * Command> 
    // TODO: Add variable context to the program state
    Variables : Map<string, Value>
  }

// ----------------------------------------------------------------------------
// Utilities
// ----------------------------------------------------------------------------

let printValue value = 
  // TODO: Add support for printing NumberValue and BoolValue
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

let rec evalExpression expr state = 
  // TODO: Add support for 'Function' and 'Variable'. For now, handle just the two
  // functions we need, i.e. "-" (takes two numbers & returns a number) and "="
  // (takes two values and returns Boolean). Note that you can test if two
  // F# values are the same using '='. It works on values of type 'Value' too.
  //
  // HINT: You will need to pass the program state to 'evalExpression' 
  // in order to be able to handle variables!
  match expr with
  | Const(value) -> value
  | Function(op, [e1; e2]) -> 
      let v1 = evalExpression e1 state
      let v2 = evalExpression e2 state
      match (op, v1, v2) with
      | ("-", NumberValue(n1), NumberValue(n2)) -> NumberValue(n1 - n2)
      | ("=", _, _) -> BoolValue(v1 = v2)
      | _ -> failwith "unsupported function"
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
  
  // TODO: Implement assignment and conditional. Assignment should run the
  // next line after setting the variable value. 'If' is a bit trickier:
  // * 'L1: IF TRUE THEN GOTO <L2>' will continue evaluating on line 'L2'
  // * 'L1: IF FALSE THEN GOTO <L2>' will continue on line after 'L1'
  // * 'L1: IF TRUE THEN PRINT "HI"' will print HI and continue on line after 'L1'
  //
  // HINT: If <e> evaluates to TRUE, you can call 'runCommand' recursively with
  // the command in the 'THEN' branch and the current line as the line number.
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
  | None -> (runCommand state (System.Int32.MaxValue, cmd)).Program

let runInputs state cmds = (state, cmds) ||> List.fold (fun st cmd -> { state with Program = runInput st cmd })

// ----------------------------------------------------------------------------
// Test cases
// ----------------------------------------------------------------------------

let empty = { Program = []; Variables = Map.empty } // TODO: Add empty variables to the initial state!

let helloOnce = 
  [ Some 10, Print (Const (StringValue "HELLO WORLD\n")) 
    Some 10, Print (Const (StringValue "HELLO NPRG077\n")) 
    None, Run ]

let helloInf = 
  [ Some 20, Goto 10
    Some 10, Print (Const (StringValue "HELLO WORLD\n")) 
    Some 10, Print (Const (StringValue "HELLO NPRG077\n")) 
    None, Run ]

let testVariables = 
  [ Some 10, Assign("S", Const(StringValue "HELLO WORLD\n")) 
    Some 20, Assign("I", Const(NumberValue 1))
    Some 30, Assign("B", Function("=", [Variable("I"); Const(NumberValue 1)]))
    Some 40, Print(Variable "S") 
    Some 50, Print(Variable "I") 
    Some 60, Print(Variable "B")
    None, Run ]

// NOTE: Simpler test program without 'If" (just variables and '=' function) 
runInputs empty testVariables |> ignore

let helloTen = 
  [ Some 10, Assign("I", Const(NumberValue 10))
    Some 20, If(Function("=", [Variable("I"); Const(NumberValue 1)]), Goto(60))
    Some 30, Print (Const(StringValue "HELLO WORLD\n")) 
    Some 40, Assign("I", Function("-", [ Variable("I"); Const(NumberValue 1) ]))
    Some 50, Goto 20
    Some 60, Print (Const(StringValue "")) 
    None, Run ]

// NOTE: Prints hello world ten times using conditionals
runInputs empty helloTen |> ignore
