// ----------------------------------------------------------------------------
// 04 - Generating and solving goals recursively
// ----------------------------------------------------------------------------

type Term = 
  | Atom of string
  | Variable of string
  | Predicate of string * Term list

type Clause =
  { Head : Term
    Body : Term list }

type Program = Clause list

let fact p = { Head = p; Body = [] }

let rule p b = { Head = p; Body = b }

// ----------------------------------------------------------------------------
// Substitutions and unification of terms
// ----------------------------------------------------------------------------

module Map =
  let append (m1:Map<_,_>) m2 =
    m1 |> Seq.fold (fun st (KeyValue(k, v)) -> Map.add k v st) m2

let rec substitute (subst:Map<string, Term>) term = 
  match term with
  | Variable(s) when subst.ContainsKey(s) -> subst.[s]
  | Predicate(s, ts) -> Predicate(s, (substituteTerms subst ts))
  | t -> t

and substituteTerms subst (terms:list<Term>) = 
  match terms with
  | t::ts -> [substitute subst t] @ (substituteTerms subst ts)
  | [] -> []

let rec substituteSubst (newSubst:Map<string, Term>) (subst:Map<string, Term>) = 
  subst |> Map.map (fun k v -> substitute newSubst v)

let rec unifyLists l1 l2 = 
  match l1, l2 with 
  | [], [] -> 
      Some(Map.empty)
  | h1::t1, h2::t2 -> 
      let r1 = unify h1 h2
      match r1 with
      | Some(s1) ->
        let r2 = unifyLists (substituteTerms s1 t1) (substituteTerms s1 t2)
        match r2 with
        | Some(s2) -> Some(Map.append (substituteSubst s2 s1) s2)
        | _ -> None
      | _ -> None
  | _ -> None

and unify t1 t2 = 
  match t1, t2 with
  | Atom(s1), Atom(s2) when s1 = s2 -> Some(Map.empty)
  | Predicate(s1, l1), Predicate(s2, l2) when s1 = s2 -> unifyLists l1 l2
  | Variable(s), t | t, Variable(s) -> Some(Map.ofList [s, t])
  | _, _ -> None

// ----------------------------------------------------------------------------
// Searching the program (database) and variable renaming
// ----------------------------------------------------------------------------

let nextNumber = 
  let mutable n = 0
  fun () -> n <- n + 1; n

let rec freeVariables term = 
  match term with
  | Variable(s) -> [s]
  | Predicate(s, ts) -> ts |> List.collect (fun t -> freeVariables t)
  | _ -> []

let withFreshVariables (clause:Clause) : Clause =
  let vars = clause.Body |> List.collect freeVariables
  let dl = List.distinct (freeVariables clause.Head) @ vars
  let n = nextNumber()
  let subst = Map.ofList (dl |> List.map (fun v -> (v, Variable(sprintf "%s%i" v n))))
  { Head = (substitute subst clause.Head); Body = (substituteTerms subst clause.Body) }

let query (program:list<Clause>) (query:Term) 
    : list<Clause * Map<string, Term>> =
  program |> List.choose (fun clause ->
    let clause = withFreshVariables clause
    let res = unify clause.Head query
    match res with
    | Some(subst) -> Some(clause, subst)
    | _ -> None  
  )

let rec solve program subst goals = 
  match goals with 
  | g::goals -> 
      // TODO: We need to solve the goal (term) 'g'. To do so, find all 
      // matching clauses in the 'program' using 'query' and iterate over
      // the returned list using 'for clause, newSubst in matches do'.
      // For each possible solution, we need to add the 'clause.Body' to 
      // the list of 'goals' and apply the substitution 'newSubst' to the
      // new concatentated list of 'goals'. Then we need to apply the 
      // substitution 'newSubst' to the substitution 'subst' we have so far,
      // append the two and call 'solve' recursively with this new substitution
      // to solve the new goals.
      let matches = query program g
      for clause, newSubst in matches do
        let newGoals = goals @ clause.Body
        let subst = Map.append (substituteSubst newSubst subst) newSubst
        let newGoals = substituteTerms newSubst newGoals
        solve program subst newGoals
  | [] -> 
    // TODO: We solved all goals, which means 'subst' is a possible solution!
    // Print 'subst' (either using printfn "%A" or in some nicer way).
    printfn "result = %A" subst

// ----------------------------------------------------------------------------
// Querying the British royal family 
// ----------------------------------------------------------------------------

// Some information about the British royal family 
let family = [ 
  fact (Predicate("male", [Atom("William")]))
  fact (Predicate("female", [Atom("Diana")]))
  fact (Predicate("male", [Atom("Charles")]))
  fact (Predicate("male", [Atom("George")]))
  fact (Predicate("parent", [Atom("Diana"); Atom("William")]))
  fact (Predicate("parent", [Atom("Charles"); Atom("William")]))
  fact (Predicate("parent", [Atom("William"); Atom("George")]))
  rule (Predicate("father", [Variable("X"); Variable("Y")])) [
    Predicate("parent", [Variable("X"); Variable("Y")])
    Predicate("male", [Variable("X")])
  ]
]

// Query: father(X, William)
// Result #1: [ X -> Charles, ... ]
solve family Map.empty [ Predicate("father", [Variable("X"); Atom("William")]) ]

// Query: father(X, Y)
// Result #1: [ X -> Charles, Y -> William, ... ]
// Result #2: [ X -> William, Y -> George, ... ]
solve family Map.empty [ Predicate("father", [Variable("X"); Variable("Y")]) ]
