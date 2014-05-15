package elfcala.examples

import elfcala._
import LogicalFramework._
import LogicalFramework.Kind.Type


// From "A Framework for Defining Logics", by Harper, Honsell, and Plotkin
trait FirstOrderLogic extends Signature {
  // Types
  val i = 'i :> Type
  val o = 'o :> Type

  // Terms
  val z    = 'z    :> i
  val succ = 'succ :> i ->: i
  val plus = 'plus :> i ->: i ->: i
  val prod = 'prod :> i ->: i ->: i

  // Formulas
  val eq     = 'eq     :> i ->: i ->: o
  val less   = 'less   :> i ->: i ->: o
  val not    = 'not    :> o ->: o
  val or     = 'or     :> o ->: o ->: o
  val and    = 'and    :> o ->: o ->: o
  val forall = 'forall :> (i ->: o) ->: o
  val exists = 'exists :> (i ->: o) ->: o
  val impl   = 'impl   :> o ->: o ->: o

  // Rules (judgements)
  val p = 'p; val q = 'q
  val tru = 'tru :> o ->: Type

  // "reductio ad absurdum" rule
  val raa = 'raa :> !!(p, o)/ {tru ( not ( not (p) )) ->: (tru (p))}

  // "Implication introduction" rule
  val imp_i = 'imp_i :> !!(p, o) (q, o)/
                        { (tru(p) ->: tru(q)) ->: tru(impl(p)(q)) }

  // TODO: add remaining rules

}
