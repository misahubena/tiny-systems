// ----------------------------------------------------------------------------
// 07 - Generating magic squares in TinyProlog
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
  match term with
  | Atom("empty") -> Some([])
  | Predicate("cons", [h; List(l)]) -> Some(h::l)
  | _ -> None

let rec formatTerm term = 
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
    subst
    |> Map.filter (fun var term -> vars.Contains var)
    |> Map.map (fun var term -> sprintf "%s = %s" var (formatTerm term))
    |> Map.values
    |> String.concat ",\n"
    |> printfn "%s;"

// ----------------------------------------------------------------------------
// Calculating with numbers
// ----------------------------------------------------------------------------

let rec num n =
  match n with
  | 0 -> Atom("zero")
  | n -> Predicate("succ", [num (n-1)])

let nums = [
  fact (Predicate("add", [Atom("zero"); Variable("X"); Variable("X")]))
  rule (Predicate("add", [Predicate("succ", [ Variable("X") ]); Variable("Y"); Predicate("succ", [ Variable("Z")]) ])) [
    Predicate("add", [Variable("X"); Variable("Y"); Variable("Z")])
  ]
  fact (Predicate("eq", [Variable("X"); Variable("X")]))
]


// ----------------------------------------------------------------------------
// Working with lists
// ----------------------------------------------------------------------------

let rec makeList l : Term = 
  match l with
  | [] -> Atom("empty")
  | t::rest -> Predicate("cons", [t; (makeList rest)])

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

let permutation =
  append @ [
    fact (Predicate("perm", [ Atom("empty"); Atom("empty") ]))
    rule (Predicate("perm", [ Variable("L"); Predicate("cons", [Variable("H"); Variable("T")]) ])) [
      Predicate("append", [ Variable("V"); Predicate("cons", [Variable("H"); Variable("U")]); Variable("L") ])
      Predicate("append", [ Variable("V"); Variable("U"); Variable("W") ])
      Predicate("perm", [ Variable("W"); Variable("T") ])
    ]
  ]

// DEMO: Generate all permutations of the list [1 .. 4]
run permutation (Predicate("perm", [l1to4; Variable("X")]))


// ----------------------------------------------------------------------------
// Generating magic squares
// ----------------------------------------------------------------------------

// Custom operator and a hlper function for equality & defining variables
let (.=.) a b = Predicate("eq", [a; b])
let var x = Variable(x)

// TinyProlog is too slow! But if we give it the numbers in an order
// that is close to being a magic square (first row is correct), it will 
// manage to generate a magic square sooner or later...
//let l = [ 2;7;6; 1;3;4; 5;8;9 ]
let l = [ 2;7;6; 9;5;1; 4;3;8; ]

let magic = permutation @ nums @ [
  rule (Predicate("add3", [ var "A"; var "B"; var "C"; var "S" ])) [
    Predicate("add", [ var "A"; var "B"; var "T" ])
    Predicate("add", [ var "T"; var "C"; var "S" ])
  ]
  rule (Predicate("magic", [ var "S"; var "X" ])) [
    yield Predicate("perm", [makeList [ for i in l -> num i ]; var "X"])
    yield var "X" .=. makeList [ var "A1"; var "A2"; var "A3"; var "B1"; 
      var "B2"; var "B3"; var "C1"; var "C2"; var "C3" ]    
    for a, b, c in [ 
      ("A1","A2","A3"); ("B1","B2","B3"); ("C1","C2","C3") 
      ("A1","B1","C1"); ("A2","B2","C2"); ("A3","B3","C3")
      ("A1","B2","C3"); ("A3","B2","C1") ] do
      yield Predicate("add3", [var a; var b; var c; var "S"]) 
  ]
]

run magic (Predicate("magic", [num 15; var "X"]))
