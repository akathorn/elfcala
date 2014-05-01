package elfcala

import org.scalatest._

import elfcala._
import elfcala.examples._

import LogicalFramework._

class Test extends FunSuite with Signature {

  test("First order logic") {
    assertResult(true) {
      val fol = new FirstOrderLogic
      // Typecheck(fol)
    }
  }

  test("Natural numbers") {
    assertResult(true) {
      val naturals = new Naturals
      // Typecheck(naturals)
    }
  }

  test("Syntactic sugar 1") {
    assertResult("ObjectBinding(Const(Constant('r)),Pi(Variable('p),Const(Constant('o)),Pi(Variable('x),App(Const(Constant('tru)),App(Const(Constant('not)),App(Const(Constant('not)),Var(Variable('p))))),App(Const(Constant('tru)),Var(Variable('p))))))") {
      val p = 'p; val o = 'o; val tru = 'tru; val not = 'not
      ('r :> Family.Pi(Variable(p),
                    Family.Const(Constant(o)),
                    tru ( not ( not (p) )) ->: tru (p))
       ).toString
    }
  }

}

