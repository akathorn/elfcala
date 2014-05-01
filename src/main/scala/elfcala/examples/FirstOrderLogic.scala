package elfcala.examples

import elfcala._
import LogicalFramework._
import LogicalFramework.Kind.Type


// From "A Framework for Defining Logics", by Harper, Honsell, and Plotkin
class FirstOrderLogic extends Signature {
  // Types
  val i = 'i
  val o = 'o

  i :> Type
  o :> Type

  // Terms
  val z    = 'z
  val succ = 'succ
  val plus = 'plus
  val prod = 'prod

  z    :> i
  succ :> i ->: i
  plus :> i ->: i ->: i
  prod :> i ->: i ->: i

  // Formulas
  val eq     = 'eq
  val less   = 'less
  val not    = 'not
  val or     = 'or
  val and    = 'and
  val forall = 'forall
  val exists = 'exists
  val impl   = 'impl

  eq     :> i ->: i ->: o
  less   :> i ->: i ->: o
  not    :> o ->: o
  or     :> o ->: o ->: o
  and    :> o ->: o ->: o
  forall :> (i ->: o) ->: o
  exists :> (i ->: o) ->: o
  impl   :> o ->: o ->: o

  // Rules (judgements)
  val tru = 'tru
  val raa = 'raa

  tru :> o ->: Type

  // "reductio ad absurdum" rule
  val p = 'p
  raa :> !!(p, o)/ (tru ( not ( not (p) )) ->: (tru (p)))


  // TODO: add remaining rules
  println(bindings)

}




