// ----------------------------------------------------------------------------
// 07 - Generating magic squares in TinyProlog
// ----------------------------------------------------------------------------

type Term = 
  | Atom of string
  | Variable of string
  | Predicate of string * Term list
  // NOTE: Added 'Call' as a special kind of predicate
  | Call of Term * Term list

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
// TODO: Add a case for 'Call' to substitution 
  match term with
  | Variable(s) when subst.ContainsKey(s) -> subst.[s]
  | Predicate(s, ts) -> Predicate(s, (substituteTerms subst ts))
  | Call(t, args) -> Call((substitute subst t), (substituteTerms subst args))
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
  // TODO: This is where we need a clever trick to handle 'Call'!
  // Unification can succeed if we have a predicate and a call with a
  // corresponding predicate as the first argument. So we can unify:
  //
  //   Predicate(p1, args1) ~ Call(Predicate(p2, args2a), args2b)
  //
  // When 'p1 = p2' and when we can unify 'args1 ~ args2a @ args2b'.
  match t1, t2 with
  | Atom(s1), Atom(s2) when s1 = s2 -> Some(Map.empty)
  | Predicate(s1, l1), Predicate(s2, l2) when s1 = s2 -> unifyLists l1 l2
  | Variable(s), t | t, Variable(s) -> Some(Map.ofList [s, t])
  | Predicate(p1, args1), Call(Predicate(p2, args2a), args2b)
  | Call(Predicate(p2, args2a), args2b), Predicate(p1, args1) when p1 = p2 -> unifyLists args1 (args2a @ args2b)
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
  // TODO: You can format 'Call(args)' as 'Predicate("call", args)'
  match term with
  | Number n -> string n
  | List(items) -> "[" + (String.concat ", " (items |> List.map formatTerm)) + "]"
  | Atom s -> s
  | Variable v -> v
  | Predicate(p, items) -> p + "(" + (String.concat ", " (items |> List.map formatTerm)) + ")"
  | Call(_, args) -> formatTerm (Predicate("call", args))

// ----------------------------------------------------------------------------
// Searching the program (database) and variable renaming
// ----------------------------------------------------------------------------

let nextNumber = 
  let mutable n = 0
  fun () -> n <- n + 1; n

let rec freeVariables term = 
  // TODO: Add a case for 'Call' when getting free variables
  match term with
  | Variable(s) -> [s]
  | Predicate(s, ts) -> ts |> List.collect (fun t -> freeVariables t)
  | Call(t, args) -> t::args |> List.collect (fun t -> freeVariables t)
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
    |> printf "%s"

    printfn ";"

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

// ----------------------------------------------------------------------------
// Call and maplist
// ----------------------------------------------------------------------------

// The Prolog 'call' operation takes a term and a list of arguments
// and supplies the arguments as additional arguments to the term.
// So, for example, calling 'call(add(1), 2, X)' becomes 'add(1, 2, X)'
run nums (Call(Predicate("add", [num 1]), [num 2; Variable "X"]))
run nums (Call(Predicate("add", [num 1; Variable "X"]), [num 5]))

// This can be used to implement the 'maplist' function:
// $ maplist(_, [], []).
// $ maplist(G,[X|Xs],[Y|Ys]) :- maplist(G,Xs,Ys), call(G,X,Y).
let maplist = [
  fact (Predicate("maplist", [ Variable("_"); Atom("empty"); Atom("empty") ]))
  rule (Predicate("maplist", [ 
    Variable("G")
    Predicate("cons", [ Variable("X"); Variable("Xs") ])
    Predicate("cons", [ Variable("Y"); Variable("Ys") ]);  
  ])) [
    Predicate("maplist", [ Variable("G"); Variable("Xs"); Variable("Ys") ])
    Call(Variable("G"), [ Variable("X"); Variable("Y") ])
  ]
]

// Query: maplist(add(10), l1to9, Y)
// Returns: Y -> [11; 12; ..; 19]
run (nums @ maplist) (Predicate("maplist", 
  [ Predicate("add", [num 10]); l1to9; Variable("Y") ]))
