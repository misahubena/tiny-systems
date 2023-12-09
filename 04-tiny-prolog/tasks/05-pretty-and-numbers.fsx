// ----------------------------------------------------------------------------
// 05 - Pretty printing & adding numbers to TinyProlog
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
// Pretty printing terms
// ----------------------------------------------------------------------------

let rec (|Number|_|) term = 
  match term with 
    // TODO: Write an active pattern to recognize numbers in the form used below.
    // If the term is 'Atom("zero")' return Some(0). 
    // If the term is 'Predicate("succ", [n])' where 'n' is itself
    // a term representing number, return the number value +1. 
  | Atom("zero") -> Some(0)
  | Predicate("succ", [Number(n)]) -> Some(n + 1)
  | _ -> None

let rec formatTerm term = 
  match term with 
  // Simple cases for number, atom and variable are done already...
  | Number n -> string n
  | Atom s -> s
  | Variable v -> v
  | Predicate(p, items) ->
      // TODO: format all arguments recursively using 'formatTerm'
      // You can then concatenate the arguments using 'String.concat'
      p + "(" + (String.concat ", " (items |> List.map formatTerm)) + ")"


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
  // TODO: When printing the computed substitution 'subst', print
  // the terms nicely using 'formatTerm'. You can use 'for' loop like:
  // 'for var, term in subst do printfn ...'
  match goals with 
  | g::goals -> 
      let matches = query program g
      for clause, newSubst in matches do
        let newGoals = goals @ clause.Body
        let subst = Map.append (substituteSubst newSubst subst) newSubst
        let newGoals = substituteTerms newSubst newGoals
        solve program subst newGoals
  | [] -> subst |> Map.iter (fun var term -> printfn "%s = %s" var (formatTerm term))


// ----------------------------------------------------------------------------
// Querying the British royal family 
// ----------------------------------------------------------------------------

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

// Queries from previous step (now with readable output)
solve family Map.empty [ Predicate("father", [Variable("X"); Atom("William")]) ]
solve family Map.empty [ Predicate("father", [Variable("X"); Variable("Y")]) ]


// ----------------------------------------------------------------------------
// Calculating with numbers
// ----------------------------------------------------------------------------

// Helper that generates a term representing a number
let rec num n = 
  // TODO: Write a helper that generates a term representing number.
  // This should return Atom("zero") when n is 0 and otherwise
  // succ(succ(...(zero))) with appropriate number of 'succ's.
  match n with
  | 0 -> Atom("zero")
  | n -> Predicate("succ", [num (n-1)])

// Addition and equality testing for Peano arithmetic
// $ add(zero, X, X)
// $ add(succ(X), Y, succ(Z)) :- add(X, Y, Z)
// $ eq(X, X)
let nums = [
  fact (Predicate("add", [Atom("zero"); Variable("X"); Variable("X")]))
  rule (Predicate("add", [Predicate("succ", [ Variable("X") ]); Variable("Y"); Predicate("succ", [ Variable("Z")]) ])) [
    Predicate("add", [Variable("X"); Variable("Y"); Variable("Z")])
  ]
  fact (Predicate("eq", [Variable("X"); Variable("X")]))
]

// Query: add(2, 3, X)
// Output should include: 'X = 5' 
//   (and other variables resulting from recursive calls)
solve nums Map.empty [ Predicate("add", [num 2; num 3; Variable("X")]) ]

// Query: add(2, X, 5)
// Output should include: 'X = 3' 
//   (we can use 'add' to calculate subtraction too!)
solve nums Map.empty [ Predicate("add", [num 2; Variable("X"); num 5]) ]

// Query: add(2, Y, X)
// Output should include: 'Y = Z??' and 'X = succ(succ(Z??))' 
//   (with some number for ?? - indicating that this can be any term)
solve nums Map.empty [ Predicate("add", [num 2; Variable("Y"); Variable("X")]) ]
