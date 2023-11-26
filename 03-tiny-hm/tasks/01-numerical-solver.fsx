// ----------------------------------------------------------------------------
// 01 - Complete the simple numerical constraint solver
// ----------------------------------------------------------------------------

type Number =
  | Zero
  | Succ of Number
  | Variable of string


// NOTE: The four functions below currently return a wrong 
// result, but one that makes the code run. As you implement
// them (one by one), the tests should graudally start working.


let rec occursCheck (v:string) (n:Number) = 
  // TODO: Check if variable 'v' appears anywhere inside 'n'
  match n with
  | Variable(v2) -> v = v2
  | Zero -> false
  | Succ(n2) -> occursCheck v n2

let rec substite (v:string) (subst:Number) (n:Number) =
  // TODO: Replace all occurrences of variable 'v' in the
  // number 'n' with the replacement number 'subst'
  match n with
  | Succ(n2) -> Succ(substite v subst n2)
  | Variable(v2) when v2 = v -> subst
  | n2 -> n2

let substituteConstraints (v:string) (subst:Number) (constraints:list<Number * Number>) = 
  // TODO: Substitute 'v' for 'subst' (use 'substitute') in 
  // all numbers in all the constraints in 'constraints'
  constraints |> List.map(fun (n1, n2) -> (substite v subst n1, substite v subst n2))

let substituteAll (subst:list<string * Number>) (n:Number) =
  // TODO: Perform all substitutions specified  in 'subst' on the number 'n'
  // HINT: You can use 'List.fold' to implement this. Fold has a type:
  //   ('State -> 'T -> 'State) -> 'State -> List<'T> -> 'State
  // In this case, 'State will be the Number on which we want to apply 
  // the substitutions and List<'T> will be a list of substitutions.
  (n, subst) ||> List.fold(fun n (v, s) -> substite v s n)

let rec solve constraints = 
  match constraints with 
  | [] -> []
  | (Succ n1, Succ n2)::constraints ->
      solve ((n1, n2)::constraints)
  | (Zero, Zero)::constraints -> solve constraints
  | (Succ _, Zero)::_ | (Zero, Succ _)::_ -> 
      failwith "Cannot be solved"
  | (n, Variable v)::constraints 
  | (Variable v, n)::constraints ->
      if occursCheck v n then failwith "Cannot be solved (occurs check)"
      let constraints = substituteConstraints v n constraints
      let subst = solve constraints
      let n = substituteAll subst n
      (v, n)::subst

// Should work: x = Zero
solve 
  [ Succ(Variable "x"), Succ(Zero) ]

// Should faild: S(Z) <> Z
solve 
  [ Succ(Succ(Zero)), Succ(Zero) ]

// Should fail: No 'x' such that S(S(x)) = S(Z)
solve 
  [ Succ(Succ(Variable "x")), Succ(Zero) ]

// Not done: Need to substitute x/Z in S(x)
solve 
  [ Succ(Variable "x"), Succ(Zero)
    Variable "y", Succ(Variable "x") ]

// Not done: Need to substitute z/Z in S(S(z))
solve 
  [ Variable "x", Succ(Succ(Variable "z"))
    Succ(Variable "z"), Succ(Zero) ]

// Not done: Need occurs check
solve
  [ Variable "x", Succ(Variable "x") ]
