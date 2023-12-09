// ----------------------------------------------------------------------------
// 06 - Lazy search and support for lists
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
  | Atom("zero") -> Some(0)
  | Predicate("succ", [Number(n)]) -> Some(n + 1)
  | _ -> None

let rec (|List|_|) term : option<list<Term>> = 
  // TODO: If the term represents a list, this should return the 
  // elements of the list collected in an ordinary F# list.
  // If the term is 'Atom("empty")' return Some([])
  // If the term is 'Predicate("cons", [h; tl])' where 'tl' is itself
  // a term representing a list 'l', return Some(h::l).
  match term with
  | Atom("empty") -> Some([])
  | Predicate("cons", [h; List(l)]) -> Some(h::l)
  | _ -> None

let rec formatTerm term = 
  // TODO: Add a case for 'List(items)' - pretty print this as a list
  match term with
  | Number n -> string n
  | List(items) -> "[" + (String.concat ", " (items |> List.map formatTerm)) + "]"
  | Atom s -> s
  | Variable v -> v
  | Predicate(p, items) -> p + "(" + (String.concat ", " (items |> List.map formatTerm)) + ")"
  
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

let rec solve program subst goals : seq<Map<string, Term>> = seq {
  // TODO: We want to change this function to return a lazy sequence
  // of all possible substitutions solving the problem. I already 
  // wrapped the code in 'seq { .. }' block for you. Change the rest
  // to recursively call 'solve' using 'yield!' and return new 
  // solutions using 'yield' (replacing the printing code).
  match goals with 
  | g::goals -> 
      let matches = query program g
      for clause, newSubst in matches do
        let newGoals = goals @ clause.Body
        let subst = Map.append (substituteSubst newSubst subst) newSubst
        let newGoals = substituteTerms newSubst newGoals
        yield! solve program subst newGoals
  | [] -> yield subst
}

let run program query = 
  let vars = Set.ofSeq (freeVariables query)
  for subst in solve program Map.empty [query] do
    // TODO: To avoid cluttered output, we want to only print assignment
    // for variables that appear in the original query (and skip all 
    // variables generated by the various internal matches). You can do
    // this here by iterating over variables and printing them only if
    // they are included in 'vars' (test using 'vars.Contains')
    subst
    |> Map.filter (fun var term -> vars.Contains var)
    |> Map.map (fun var term -> sprintf "%s = %s" var (formatTerm term))
    |> Map.values
    |> String.concat ",\n"
    |> printfn "%s;"


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

// Queries from previous step (now called using 'run')
run family (Predicate("father", [Variable("X"); Atom("William")]))
run family (Predicate("father", [Variable("X"); Variable("Y")]))


// ----------------------------------------------------------------------------
// Calculating with numbers
// ----------------------------------------------------------------------------

// Helper that generates a term representing a number
let rec num n =
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

// Queries from previous step (now called using 'run')
run nums (Predicate("add", [num 2; num 3; Variable("X")]))
run nums (Predicate("add", [num 2; Variable("X"); num 5]))
run nums (Predicate("add", [num 2; Variable("Y"); Variable("X")]))


// ----------------------------------------------------------------------------
// Working with lists
// ----------------------------------------------------------------------------

// Helper that generates a term representing a list
let rec makeList l : Term = 
  // TODO: Write a helper that generates a term representing a list.
  // This should return Atom("empty") when 'l' is [] and otherwise
  // cons(t1, .. cons(tN, empty)) when 'l' is [t1; ...; tN]
  match l with
  | [] -> Atom("empty")
  | t::rest -> Predicate("cons", [t; (makeList rest)])

// TinyProlog code to represent 'append' operation on lists
// $ append([X|Y],Z,[X|W]) :- append(Y,Z,W).
// $ append([],X,X).
let append = [ 
  fact (Predicate("append", [Atom("empty"); Variable("X"); Variable("X") ]))
  rule (Predicate("append", [
    Predicate("cons", [Variable("X"); Variable("Y") ])
    Variable("Z"); Predicate("cons", [Variable("X"); Variable("W") ])
  ])) [
    Predicate("append", [ Variable("Y"); Variable("Z"); Variable("W") ])
  ]
]

let l1to4 = makeList [ for i in 1 .. 4 -> num i ]
let l5to9 = makeList [ for i in 5 .. 9 -> num i ]
let l1to9 = makeList [ for i in 1 .. 9 -> num i ]

// TODO: Test the term formatting - this should print nice outputs!
formatTerm l1to4
formatTerm l5to9
formatTerm l1to9

// Query: append([1..4], [5..9], X)
// Return: X -> [1..9]
run append (Predicate("append", [l1to4; l5to9; Variable "X"]))

// Query: append([1..4], X, [1..9])
// Return: X -> [5..9]
run append (Predicate("append", [l1to4; Variable "X"; l1to9]))

// Query: append(X, Y, [1..9])
// Return: 
//  * X -> [1..9], Y -> []
//  * X -> [1..8], Y -> [9]
//  * X -> [1..7], Y -> [8, 9]
//  * X -> [1..6], Y -> [7 .. 9]
//  * etc.
run append (Predicate("append", [Variable "Y"; Variable "X"; l1to9]))
