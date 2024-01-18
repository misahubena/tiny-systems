// ----------------------------------------------------------------------------
// 05 - Rendering sheets as HTML
// ----------------------------------------------------------------------------

type Address = int * int

type Value = 
  | Number of int
  | String of string
  | Error of string
  
type Expr = 
  | Const of Value
  | Reference of Address
  | Function of string * Expr list

type CellNode = 
  { mutable Value : Value
    mutable Expr : Expr
    Updated : Event<unit> } 

type LiveSheet = Map<Address, CellNode>

// ----------------------------------------------------------------------------
// Reactive evaluation and graph construction
// ----------------------------------------------------------------------------

let rec eval (sheet:LiveSheet) expr = 
  match expr with
  | Const(v) -> v
  | Reference(a) ->
    if (Map.containsKey a sheet)
    then sheet[a].Value
    else Error("Missing value")
  | Function(op, a::b::[]) ->
    match eval sheet a, eval sheet b with
    | Number(x), Number(y) ->
      match op with
      | "+" -> Number(x + y)
      | "-" -> Number(x - y)
      | "*" -> Number(x * y)
      | "/" -> Number(x / y)
      | _ -> Error("Unknown function")
    | Error(s),_ | _, Error(s) -> Error(s)
    | _ -> Error("Invalid function arguments")
  | _ -> Error("Unknown function")

let rec collectReferences expr = 
  match expr with
  | Function(_, es) -> es |> List.collect (fun e -> collectReferences e)
  | Reference(a) -> [a]
  | _ -> []

let makeNode addr (sheet:LiveSheet) expr = 
  let e = Event<unit>()
  let node = { Value = eval sheet expr; Expr = expr; Updated = e}
  let update () = node.Value <- eval sheet expr; node.Updated.Trigger()
  let refs = collectReferences expr
  refs |> List.iter(fun a -> sheet[a].Updated.Publish.Add(update))
  node

let updateNode addr (sheet:LiveSheet) expr = 
  let node = sheet[addr]
  node.Expr <- expr
  node.Value <- eval sheet node.Expr
  node.Updated.Trigger()

let makeSheet list = 
  (Map.empty, list) ||> List.fold(fun s (a, e) -> s |> Map.add a (makeNode a s e))

// ----------------------------------------------------------------------------
// Drag down expansion
// ----------------------------------------------------------------------------

let rec relocateReferences (srcCol, srcRow) (tgtCol, tgtRow) (srcExpr:Expr) = 
  match srcExpr with
  | Const(v) -> Const(v)
  | Reference((a, b)) -> Reference(a + tgtCol - srcCol, b + tgtRow - srcRow)
  | Function(s, es) -> Function(s, es |> List.map(fun e -> relocateReferences (srcCol, srcRow) (tgtCol, tgtRow) e))

let expand (c1, r1) (c2, r2) (sheet:LiveSheet) = 
  let srcExpr = sheet[Address(c1, r1)].Expr
  let cells = [for c in c1 .. c2 do for r in r1 .. r2 do yield Address(c, r), relocateReferences (c1, r1) (c, r) srcExpr]
  (sheet, cells) ||> List.fold(fun s (a, c) -> s |> Map.add a (makeNode a s c))

// ----------------------------------------------------------------------------
// Rendering sheets as HTML
// ----------------------------------------------------------------------------

open System.IO
open System.Diagnostics

let displayValue (v:Value) : string =
  // TODO: Turn the given value into a string representing HTML
  // You can use the following to create an error string in red.
  match v with
  | Number(n) -> $"<span>{n}</span>"
  | String(s) -> $"<span>{s}</span>"
  | Error(s) -> $"<span class='e'>{s}</span>"
  
let display (sheet:LiveSheet) = 
  // TODO: Find the greates row and column index
  let maxCol = (0, sheet) ||> Map.fold(fun s (c, r) v -> max s c)
  let maxRow = (0, sheet) ||> Map.fold(fun s (c, r) v -> max s r)

  let f = Path.GetTempFileName() + ".html"
  use wr = new StreamWriter(File.OpenWrite(f))
  wr.Write("""<html><head>
      <style>
        * { font-family:sans-serif; margin:0px; padding:0px; border-spacing:0; } 
        th, td { border:1px solid black; border-collapse:collapse; padding:4px 10px 4px 10px }
        body { padding:50px } .e { color: red; } 
        th { background:#606060; color:white; } 
      </style>
    </head><body><table>""")

  // TODO: Write column headings
  wr.Write("<tr><th></th>")
  for col in 1 .. maxCol do 
    wr.Write($"<th>{char(64 + col)}</th>")
  wr.Write("</tr>")

  // TODO: Write row headings and data
  for row in 1 .. maxRow do 
    wr.Write($"<tr><th>{row}</th>")
    for col in 1 .. maxCol do 
      if Map.containsKey (col, row) sheet
      then wr.Write($"<td>{displayValue sheet[(col, row)].Value}</td>")
      else wr.Write("<td></td>")
    wr.Write("</tr>")
  wr.Write("</table></body></html>")
  wr.Close()
  //Process.Start(f)


// ----------------------------------------------------------------------------
// Helpers and test cases
// ----------------------------------------------------------------------------

let addr (s:string) = 
  Address(int(s.[0]) - 64, int(s.[1..]))

// NOTE: Let's visualize the Fibbonacci spreadsheet from Step 2!
let fib =  
  [ addr "A1", Const(Number 0) 
    addr "A2", Const(Number 1)
    addr "A3", Function("+", [Reference(addr "A1"); Reference(addr "A2")]) ]
  |> makeSheet
  |> expand (addr "A3") (addr "A10")
display fib

// NOTE: Let's visualize the Factorial spreadsheet from Step 2!
let fac = 
  [ addr "A2", Const(Number 1)
    addr "A3", Function("+", [Reference(addr "A2"); Const(Number 1)])
    addr "B1", Const(Number 1)
    addr "B2", Function("*", [Reference(addr "A2"); Reference(addr "B1")]) ] 
  |> makeSheet
  |> expand (addr "A3") (addr "A11")
  |> expand (addr "B2") (addr "B11")
display fac

// NOTE: Let's visualize the Temp convertor spreadsheet from Step 4! 
let tempConv = 
  [ addr "A1", Const(String "F to C")
    addr "B1", Const(Number 0) 
    addr "C1", 
      Function("/", [ 
        Function("*", [ 
          Function("-", [ Reference(addr "B1"); Const(Number 32) ])
          Const(Number 5) ])
        Const(Number 9) ]) 
    addr "A2", Const(String "C to F")
    addr "B2", Const(Number 0) 
    addr "C2",
      Function("+", [ 
        Function("/", [ 
          Function("*", [ Reference(addr "B2"); Const(Number 9) ])
          Const(Number 5) ])
        Const(Number 32) ])]
  |> makeSheet
display tempConv
