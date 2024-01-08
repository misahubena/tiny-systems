// ----------------------------------------------------------------------------
// 03 - Cloning and mutating TinySelf objects
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

// TODO: To implement assignment, we need to know what object a slot
// comes from. Modify 'lookup' so that it returns not just the slot,
// but also the object that the slot comes from.
let rec lookup msg obj =
  let slot = obj.Slots |> List.filter (fun s -> s.Name = msg) |> List.map (fun s -> (obj, s))
  if List.isEmpty slot then parentLookup msg obj else slot
and parentLookup msg obj =
  let parents = obj.Slots |> List.filter(fun s -> s.IsParent)
  ([], parents) ||> List.fold (fun res p -> res @ (lookup msg p.Contents))

// TODO: Modify 'send' and 'eval' to also take message send arguments.
// In Self, the arguments are copied into the activation record. 
// In TinySelf, we use simpler trick - just make the 'args' object 
// another parent of the activation record! Lookup for argument name 
// in the activation record will then give us the value.
// NOTE: The object newly returned from 'lookup' should be ignored.
// BEWARE: All arguments are 'Objekt' so it is easy to swap them!! 
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
// Tests - cloning and modifying cats
// ----------------------------------------------------------------------------

let cat = makeDataObject [
  makeSlot "sound" (makeString "Meow")
]
let mogscats = makeDataObject [
  makeSlot "book" (makeString "Mog's Family of Cats")
  // NOTE: This allows us to rename the book (probably not
  // something you'd want to do, but for illustration purposes...)
  makeAssignmentSlot "book"
]
let mog = makeDataObject [
  // NOTE: Mog is now also clonable and has assignment slot "name:"
  makeParentSlot "parent*" cat
  makeParentSlot "clonable*" clonablePrototype
  makeParentSlot "fictional*" mogscats
  makeSlot "name" (makeString "Mog")
  makeAssignmentSlot "name"
]

// NOTE: We now pass empty arguments to all of the message sends
mog |> send "name" empty |> send "print" empty
mog |> send "sound" empty |> send "print" empty
mog |> send "book" empty |> send "print" empty

// NOTE: Clone Ginger and print its name & book
let ginger = mog |> send "clone" empty

ginger |> send "name" empty |> send "print" empty
ginger |> send "book" empty |> send "print" empty

// TODO: Write code to change the name of 'ginger' to "Ginger"!
// (send message "name:" with arument containing slot 'new' with the new value)
ginger
|> send "name:" (makeDataObject [makeSlot "new" (makeString "Ginger")])

// TODO: Write code to change the book of 'ginger' to "Goodbye, Mog"!
ginger
|> send "book:" (makeDataObject [makeSlot "new" (makeString "Goodbye, Mog")])

// TODO: What do we get if we run the following now?
mog |> send "name" empty |> send "print" empty
mog |> send "book" empty |> send "print" empty
ginger |> send "name" empty |> send "print" empty
ginger |> send "book" empty |> send "print" empty
