// ----------------------------------------------------------------------------
// Adding simple data types
// ----------------------------------------------------------------------------

type Expression = 
  | Constant of int
  | Binary of string * Expression * Expression
  | If of Expression * Expression * Expression
  | Variable of string
  | Application of Expression * Expression
  | Lambda of string * Expression
  | Let of string * Expression * Expression
  // NOTE: Added two types of expression for working with tuples
  | Tuple of Expression * Expression
  | TupleGet of bool * Expression

type Type = 
  | TyVariable of string
  | TyBool 
  | TyNumber 
  | TyList of Type
  | TyFunction of Type * Type
  // NOTE: Added type for tuples
  | TyTuple of Type * Type

// ----------------------------------------------------------------------------
// Constraint solving
// ----------------------------------------------------------------------------

let rec occursCheck vcheck ty = 
  // TODO: Add case for 'TyTuple' (same as 'TyFunction')
  match ty with
  | TyVariable(v) -> v = vcheck
  | TyList(t) -> occursCheck vcheck t
  | TyFunction(t1, t2) | TyTuple(t1, t2) -> occursCheck vcheck t1 || occursCheck vcheck t2
  | _ -> false

let rec substType (subst:Map<_, _>) t1 = 
  // TODO: Add case for 'TyTuple' (same as 'TyFunction')
  match t1 with
  | TyVariable(v) when subst.ContainsKey(v) -> subst.[v]
  | TyList(t) -> TyList(substType subst t)
  | TyFunction(t1, t2) -> TyFunction(substType subst t1, substType subst t2)
  | TyTuple(t1, t2) -> TyTuple(substType subst t1, substType subst t2)
  | t -> t

let substConstrs subst cs = 
  cs |> List.map(fun (t1, t2) -> (substType subst t1, substType subst t2))
 
let rec solve cs =
  // TODO: Add case for 'TyTuple' (same as 'TyFunction')
  match cs with 
  | [] -> []
  | (TyNumber, TyNumber)::cs | (TyBool, TyBool)::cs -> solve cs
  | (TyList(list1), TyList(list2))::cs -> solve ((list1, list2)::cs)
  | (TyFunction(ta1, tb1), TyFunction(ta2, tb2))::cs 
  | (TyTuple(ta1, tb1), TyTuple(ta2, tb2))::cs -> 
    solve ((ta1, ta2)::(tb1, tb2)::cs)
  | (t, TyVariable(v))::cs | (TyVariable(v), t)::cs ->
    if occursCheck v t then failwith "Cannot be solved (occurs check)"
    let cs = substConstrs (Map.ofList [v, t]) cs
    let subst = solve cs
    let t = substType (Map.ofList subst) t
    (v, t)::subst
  | _ -> failwith "Cannot be solved"


// ----------------------------------------------------------------------------
// Constraint generation & inference
// ----------------------------------------------------------------------------

type TypingContext = Map<string, Type>

let newTyVariable = 
  let mutable n = 0
  fun () -> n <- n + 1; TyVariable(sprintf "_a%d" n)

let rec generate (ctx:TypingContext) e = 
  match e with 
  | Constant _ -> TyNumber, []
  | Binary("+", e1, e2) | Binary("*", e1, e2) ->
      let t1, s1 = generate ctx e1
      let t2, s2 = generate ctx e2
      TyNumber, s1 @ s2 @ [ t1, TyNumber; t2, TyNumber ]
  | Binary("=", e1, e2) ->
      let t1, s1 = generate ctx e1
      let t2, s2 = generate ctx e2
      TyBool, s1 @ s2 @ [ t1, TyNumber; t2, TyNumber ]
  | Binary(op, _, _) -> failwithf "Binary operator '%s' not supported." op
  | Variable v -> if ctx.ContainsKey(v) then ctx.[v], [] else failwith "Cannot be generated" 
  | If(econd, etrue, efalse) ->
      let t1, s1 = generate ctx econd
      let t2, s2 = generate ctx etrue
      let t3, s3 = generate ctx efalse
      t2, s1 @ s2 @ s3 @ [t1, TyBool; t2, t3]
  | Let(v, e1, e2) ->
      let t1, s1 = generate ctx e1
      let t2, s2 = generate (Map.add v t1 ctx) e2
      t2, s1 @ s2
  | Lambda(v, e) ->
      let targ = newTyVariable()
      let t, s = generate (Map.add v targ ctx) e
      TyFunction(targ, t), s
  | Application(e1, e2) ->
      let tret = newTyVariable()
      let t1, s1 = generate ctx e1
      let t2, s2 = generate ctx e2
      tret, s1 @ s2 @ [t1, TyFunction(t2, tret)]

  | Tuple(e1, e2) ->
      // TODO: Easy. The returned type is composed of the types of 'e1' and 'e2'.
      let t1, s1 = generate ctx e1
      let t2, s2 = generate ctx e2
      TyTuple(t1, t2), s1 @ s2

  | TupleGet(b, e) ->
      // TODO: Trickier. The type of 'e' is some tuple, but we do not know what.
      // We need to generate two new type variables and a constraint.
      let t, s = generate ctx e
      let t1 = newTyVariable()
      let t2 = newTyVariable()
      let s = s @ [t, TyTuple(t1, t2)]
      if b then t1, s else t2, s

  

// ----------------------------------------------------------------------------
// Putting it together & test cases
// ----------------------------------------------------------------------------

let infer e = 
  let typ, constraints = generate Map.empty e 
  let subst = solve constraints
  let typ = substType (Map.ofList subst) typ
  typ

// Basic tuple examples:
// * (2 = 21, 123)
// * (2 = 21, 123)#1
// * (2 = 21, 123)#2
let etup = Tuple(Binary("=", Constant(2), Constant(21)), Constant(123))
etup |> infer
TupleGet(true, etup) |> infer
TupleGet(false, etup) |> infer

// Interesting case with a nested tuple ('a * ('b * 'c) -> 'a * 'b)
// * fun x -> x#1, x#2#1
Lambda("x", Tuple(TupleGet(true, Variable "x"), 
  TupleGet(true, TupleGet(false, Variable "x"))))
|> infer

// Does not type check - 'int' is not a tuple!
// * (1+2)#1
TupleGet(true, Binary("+", Constant 1, Constant 2)) |> infer


// Combining functions and tuples ('b -> (('b -> 'a) -> ('b * 'a)))
// * fun x f -> (x, f x)   
Lambda("x", Lambda("f", 
  Tuple(Variable "x", 
    Application(Variable "f", Variable "x"))))
|> infer
