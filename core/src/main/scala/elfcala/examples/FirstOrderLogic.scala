package elfcala.examples

import elfcala._
import LogicalFramework._
import LogicalFramework.Kind.Type


// From "A Framework for Defining Logics", by Harper, Honsell, and Plotkin
trait FirstOrderLogic extends Signature {
  // Types
  val i = |{ Type }
  val o = |{ Type }

  // Terms
  val z    = |{ i }
  val succ = |{ i ->: i }
  val plus = |{ i ->: i ->: i }
  val prod = |{ i ->: i ->: i }

  // Formulas
  val eq     = |{ i ->: i ->: o }
  val less   = |{ i ->: i ->: o }
  val not    = |{ o ->: o }
  val or     = |{ o ->: o ->: o }
  val and    = |{ o ->: o ->: o }
  val forall = |{ (i ->: o) ->: o }
  val exists = |{ (i ->: o) ->: o }
  val impl   = |{ o ->: o ->: o }

  // Rules (judgements)
  val p = 'p; val q = 'q
  val tru = |{ o ->: Type }

  // "reductio ad absurdum" rule
  val raa = |{ !!(p, o)/ {tru ( not ( not (p) )) ->: (tru (p))} }

  // "Implication introduction" rule
  val imp_i = |{ !!(p, o) (q, o)/
                { (tru(p) ->: tru(q)) ->: tru(impl(p)(q)) } }

  // TODO: add remaining rules

}
