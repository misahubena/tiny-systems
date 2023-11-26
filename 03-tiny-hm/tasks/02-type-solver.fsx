// ----------------------------------------------------------------------------
// 02 - Solving type constraints with numbers and Booleans
// ----------------------------------------------------------------------------

// NOTE: We will only need lists later, but to make this exercise 
// a bit more interesting, we will implement constraint resolution 
// for lists here already. This will help you in the next steps!
type Type = 
  | TyVariable of string
  | TyBool 
  | TyNumber 
  | TyList of Type

let rec occursCheck vcheck ty =
  // TODO: Return true of type 'ty' contains variable 'vcheck'
  match ty with
  | TyVariable(v) -> v = vcheck
  | TyList(t) -> occursCheck vcheck t
  | _ -> false
 
let rec substType (subst:Map<string, Type>) ty = 
  // TODO: Apply all the specified substitutions to the type 'ty'
  // (that is, replace all occurrences of 'v' in 'ty' with 'subst.[v]')
  match ty with
  | TyVariable(v) when subst.ContainsKey(v) -> subst.[v]
  | TyList(t) -> TyList(substType subst t)
  | t -> t
let substConstrs (subst:Map<string, Type>) (cs:list<Type * Type>) = 
  // TODO: Apply substitution 'subst' to all types in constraints 'cs'
  cs |> List.map(fun (t1, t2) -> (substType subst t1, substType subst t2))
 

let rec solve cs =
  match cs with 
  | [] -> []
  | (TyNumber, TyNumber)::cs -> solve cs
  // TODO: Fill in the remaining cases! You can closely follow the
  // example from task 1 - the logic here is exactly the same.
  | (TyBool, TyBool)::cs -> solve cs
  | (TyList(list1), TyList(list2))::cs -> solve ((list1, list2)::cs)
  | (t, TyVariable(v))::cs | (TyVariable(v), t)::cs ->
    if occursCheck v t then failwith "Cannot be solved (occurs check)"
    let cs = substConstrs (Map.ofList [v, t]) cs
    let subst = solve cs
    let t = substType (Map.ofList subst) t
    (v, t)::subst
  | _ -> failwith "Cannot be solved"


// ----------------------------------------------------------------------------
// Constraint solving tests
// ----------------------------------------------------------------------------

// Can be solved ('a = number, 'b = list<number>)
solve  
  [ TyList(TyVariable("a")), TyList(TyNumber)
    TyVariable("b"), TyList(TyVariable("a")) ]

// Cannot be solved (list<'a> <> bool)
solve  
  [ TyList(TyVariable("a")), TyVariable("b")
    TyVariable("b"), TyBool ]

// Can be solved ('a = number, 'b = list<number>)
solve  
  [ TyList(TyVariable("a")), TyVariable("b")
    TyVariable("b"), TyList(TyNumber) ]

// Cannot be solved ('a <> list<'a>)
solve  
  [ TyList(TyVariable("a")), TyVariable("a") ]
