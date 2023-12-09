// ----------------------------------------------------------------------------
// 02 - Composing and applying substitutions
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
  // TODO: Replace variables in 'term' for which there is a
  // replacement specified by 'subst.[var]' with the replacement.
  // You can assume the terms in 'subst' do not contain
  // any of the variables that we want to replace.
  match term with
  | Variable(s) when subst.ContainsKey(s) -> subst.[s]
  | Predicate(s, ts) -> Predicate(s, (substituteTerms subst ts))
  | t -> t

and substituteTerms subst (terms:list<Term>) = 
  // TODO: Apply substitution 'subst' to all the terms in 'terms'
  match terms with
  | t::ts -> [substitute subst t] @ (substituteTerms subst ts)
  | [] -> []

let rec substituteSubst (newSubst:Map<string, Term>) (subst:Map<string, Term>) = 
  // TODO: Apply the substitution 'newSubst' to all the terms 
  // in the existing substitiution 'subst'. (We represent one 
  // as a map and the other as a list of pairs, which is a bit 
  // inelegant, but it makes calling this function easier later.)
  subst |> Map.map (fun k v -> substitute newSubst v)

let rec unifyLists l1 l2 = 
  // TODO: Modify the implementation to use 'substituteTerms' and 'substituteSubst'.
  //
  // Let's say that your code calls 'unify h1 h2' to get a substitution 's1'
  // and then it calls 'unifyLists t1 t2' to get a substitution 's2' and then
  // it returns a concatentated list 's1 @ s2'. Modify the code so that:
  //
  // (1) The substitution 's1' is aplied to 't1' and 't2' before calling 'unifyLists'
  // (2) The substitution 's2' is applied to all terms in substitution 's1' before returning
  //
  // You can look at your ML type inference code. The structure is very similar! 
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
// Advanced unification tests requiring correct substitution
// ----------------------------------------------------------------------------

// Rquires (1)
// Example: loves(narcissus, narcissus) ~ loves(X, X)
// Returns: [ X -> narcissus ]
unify
  (Predicate("loves", [Atom("narcissus"); Atom("narcissus")]))
  (Predicate("loves", [Variable("X"); Variable("X")]))

// Requires (1)
// Example: loves(odysseus, penelope) ~ loves(X, X)
// Returns: None (cannot unify)
unify
  (Predicate("loves", [Atom("odysseus"); Atom("penelope")]))
  (Predicate("loves", [Variable("X"); Variable("X")]))

// Requires (1)
// Example: add(zero, succ(zero)) ~ add(Y, succ(Y))
// Returns: [ Y -> zero ]
unify
  (Predicate("add", [Atom("zero"); Predicate("succ", [Atom("zero")])]))
  (Predicate("add", [Variable("Y"); Predicate("succ", [Variable("Y")])]))

// Requires (2)
// Example: loves(X, narcissus) ~ loves(Y, X)
// Returns: [ X -> narcissus; Y -> narcissus ]
unify
  (Predicate("loves", [Variable("X"); Atom("narcissus")]))
  (Predicate("loves", [Variable("Y"); Variable("X")]))

// Requires (2)
// Example: add(succ(X), X) ~ add(Y, succ(Z))
// Returns: [ X -> succ(Z); Y -> succ(succ(Z)) ]
unify
  (Predicate("add", 
      [ Predicate("succ", [Variable("X")]); 
        Variable("X") ]))
  (Predicate("add", 
      [ Variable("Y"); 
        Predicate("succ", [Variable("Z")]) ]))
