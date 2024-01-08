// ----------------------------------------------------------------------------
// 04 - Representing & interpreting TinySelf expressions
// ----------------------------------------------------------------------------

type Slot = 
  { Name : string
    Contents : Objekt
    IsParent : bool } 

and Objekt = 
  { mutable Slots : Slot list 
    mutable Code : Objekt option
    mutable Special : Special option }

and Special = 
  | String of string
  | Native of (Objekt -> Objekt)

// ----------------------------------------------------------------------------
// Helpers for creating things that we will often need
// ----------------------------------------------------------------------------

let makeObject slots code = 
  { Code = Some code; Special = None; Slots = slots }
let makeDataObject slots = 
  { Code = None; Special = None; Slots = slots }
let makeSpecialObject slots special = 
  { Code = None; Special = Some special; Slots = slots }

let makeSlot n contents = 
  { Name = n; Contents = contents; IsParent = false }
let makeParentSlot n contents = 
  { Name = n; Contents = contents; IsParent = true }

let makeNativeMethod f =
  makeObject [] (makeSpecialObject [] (Native(f)))

// NOTE: Implemented in step #2
let addSlot (n:string) (contents:Objekt) (obj:Objekt) : unit = 
  let slot = makeSlot n contents
  obj.Slots <- slot::obj.Slots

let addParentSlot (n:string) (contents:Objekt) (obj:Objekt) : unit = 
  let slot = makeParentSlot n contents
  obj.Slots <- slot::obj.Slots

let cloneObject (obj:Objekt) : Objekt = 
  { Code = obj.Code; Special = obj.Special; Slots = obj.Slots }

// ----------------------------------------------------------------------------
// Lookup and message sending
// ----------------------------------------------------------------------------

let rec lookup msg obj =
  let slot = obj.Slots |> List.filter (fun s -> s.Name = msg) |> List.map (fun s -> (obj, s))
  if List.isEmpty slot then parentLookup msg obj else slot
and parentLookup msg obj =
  let parents = obj.Slots |> List.filter(fun s -> s.IsParent)
  ([], parents) ||> List.fold (fun res p -> res @ (lookup msg p.Contents))

// TODO: If 'slotValue' has some non-native 'Code', we want to evaluate it.
// But it will be easier to add this later, so copy code from step 3 and then
// return to it later (there is a TODO below telling you to do this).
//
// If the slot contains 'Some(code)', we run it by sending the 'eval' message
// to the 'code'. The method takes the activation record (the same as in the
// case of native code) as the argument, so create data object with 'activation' 
// slot and pass the as argument when calling 'eval'.
// (to call 'send', you will need to use 'let rec .. and ..' here)
let eval (slotValue:Objekt) (args:Objekt) (instance:Objekt) =
  match slotValue with
  | { Code = None } -> slotValue
  | { Code = Some({Special = Some(Native f)}) } ->
    let clone = cloneObject slotValue
    addParentSlot "self*" instance clone
    addParentSlot "args" args clone
    f clone
  | _ -> failwith "not implemented: non-special Code"
let send (msg:string) (args:Objekt) (instance:Objekt) : Objekt =
  let slots = lookup msg instance
  match slots with
  | [_, slot] -> eval slot.Contents args instance
  | [] -> failwith "no slot with such name"
  | _ -> failwith "more than one slot with such name"

// ----------------------------------------------------------------------------
// Helpers for testing & object construction
// ----------------------------------------------------------------------------

let lookupSlotValue n o = 
  match lookup n o with 
  // NOTE: We ignore the object returned by 'lookup' here.
  | [ _, { Contents = it } ] -> it
  | sl -> failwithf "lookupSlotValue: Expected slot '%s' (found %d)!" n sl.Length

let getStringValue o = 
  match lookupSlotValue "value" o with
  | { Special = Some(String s) } -> s
  | _ -> failwith "not a string value"

// NOTE: Implemented in step #2
let empty : Objekt = makeDataObject []

let printCode = makeNativeMethod (fun arcd ->
  printfn "%s" (getStringValue arcd)
  empty
)
let stringPrototype = makeDataObject [
  makeSlot "print" printCode  
]
let makeString s = 
  makeDataObject [ 
    makeSlot "value" (makeSpecialObject [] (String s)) 
    makeParentSlot "parent*" stringPrototype
  ]

// ----------------------------------------------------------------------------
// Cloning and assignments
// ----------------------------------------------------------------------------

let cloneMethod = makeNativeMethod (fun arcd -> 
  // TODO: The activation record contains a slot 'self*' which is the
  // target object. Use lookup to get it, clone it & retrn it!
  // (If the lookup returns a wrong thing, fail - that's wrong.)
    let slots = lookup "self*" arcd
    match slots with
    | [(o, _)] -> cloneObject o
    | [] -> failwith "no self* slot"
    | _ -> failwith "more than one self* slot"
  )

let clonablePrototype = 
  // TODO: Create an object that has the 'clone' method
  makeDataObject [
    makeSlot "clone" cloneMethod 
  ]

let assignmentMethod n = makeNativeMethod (fun arcd -> 
  // TODO: The activation record has a slot named 'n' somewhere in its
  // inheritance graph and a slot 'new' which is a method argument.
  // Find those two using 'lookup' and modify the slot value (in the 
  // that contained it - as returned from lookup). (Tiny)Self assignment 
  // should return the object that has been modified.
    let slots = lookup n arcd
    let args = lookup "new" arcd
    match slots, args with
    | [(obj, _)], [(_, arg)] ->
      obj.Slots <- (List.map (fun s -> if s.Name = n then {s with Contents = arg.Contents} else s) obj.Slots)
      obj
    | _ -> failwith "failed lookup of one or both slots"
  )

// Creates an assignment slot for a slot named 'n'
let makeAssignmentSlot n = 
  { Name = n + ":"; Contents = assignmentMethod n; IsParent = false }

// ----------------------------------------------------------------------------
// TinySelf code representation & interpreter
// ----------------------------------------------------------------------------

// NOTE: All code is represented as objects with 'eval' method. 
// The 'eval' method receives argument 'evalTarget' which represents 
// the object of the method containing the code that is being invoked.
// The 'eval' method is a native method implemented as F# function. We
// store the parameters of the expressions in the objects themselves 
// (and not in F# closures) to keep more things in the TinySelf world.


// TODO: 'self' expression has no arguments and needs no state. When 
// evaluated, it needs to get the original 'activation' we constructed
// when calling 'eval' - this is done by looking up 'activation' slot
// in the activation record with which the method is called - and then 
// from that, we can get the 'self*' slot.
// 
// NOTE: This is a bit confusing - the F# function we write gets 'arcd'
// which is the activation record for the 'eval' call. But this in turn 
// contains activation record 'activation' which is the activation record
// for the message send that our interpreter is handling!
let exprSelf = makeNativeMethod (fun arcd -> 
    match lookup "activation" arcd with
    | [(obj, _)] -> obj
    | _ -> failwith "activation slot not found"
  )
  
// DEMO: 'string' expression stores the string value in a slot 'str'. When 
// evaluated, it fetches 'str' (slot value) from the activation record.
let exprString (s:string) = makeDataObject [ 
  makeSlot "string" (makeString s) 
  makeSlot "eval" (makeNativeMethod (fun arcd ->
    lookupSlotValue "string" arcd
  )) ]

let exprSend msg rcv = makeDataObject [ 
  makeSlot "receiver" rcv
  makeSlot "msg" (makeString msg) 
  makeSlot "eval" (makeNativeMethod (fun arcd -> 
    // TODO: To evalaute 'send' expression, we need to:
    // * Get 'activation' (activation record of the method call we are 
    ///  interpreting) from the activation record 'arcd' and create
    //   a new data object with this as the 'activation' to be used
    //   as an argument of recursive 'eval' call(s) later
    // * Get the string value of 'msg' slot (lookup from the 'acrd')
    // * Get the receiver expression (from the 'acrd')
    //   and evaluate it by send it 'eval' with the data object 
    //   (containing 'activation') as argument
    // * Send 'msg' to the recursively evaluated receiver object!
    let activation = lookup "activation" arcd
    let msg = lookup "msg" arcd
    let receiver = lookup "receiver" arcd
    match activation, msg, receiver with
    | [(_, acts)], [(msgo, _)], [(rcvo, _)] -> 
        let obj = makeDataObject [acts]
        send "eval" 
    | _ -> failwith "activation slot, msg slot or receiver slot not found"
  )) ]

// ----------------------------------------------------------------------------
// Tests - hello world (finally!)
// ----------------------------------------------------------------------------

// TinySelf code that sends "print" to a Hello world string
let helloCode =
  exprString "Hello world!!" |> exprSend "print" 

// Run it! We need to create arguments for 'eval' with 'activation'
// but since we are not using it (no exprSelf), it can be empty.
let emptySelf = makeDataObject [makeSlot "activation" empty]
helloCode |> send "eval" emptySelf |> ignore

// TODO: Now go back to the missing case in 'eval'. 
// We now add code as methods to a TinySelf object.



// Object with 'hello' method that prints hello world!
let helloObj = makeDataObject [ 
  makeSlot "hello" (makeObject [] helloCode) 
]
helloObj |> send "hello" empty |> ignore

// TODO: A more interesting example! Create an object with 
// string slot 'sound' (Meow! if you like cats) and a 'speak'
// method that sends 'sound' to self (use exprSelf) and then
// sends 'print' to the result to print it.
let animalObj = failwith "not implemented"

animalObj |> send "speak" empty |> ignore
