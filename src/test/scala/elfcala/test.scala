package elfcala

import org.scalatest._

import elfcala._
import elfcala.examples._

import LogicalFramework._

class Test extends FunSuite with Signature {
  test("First order logic") {
    assertResult(true) {
      Typecheck(new FirstOrderLogic {})
    }
  }

  test("Natural numbers") {
    assertResult(true) {
      Typecheck(new Naturals {})
    }
  }

  test("Proofs of natural numbers") {
    assertResult(true) {
      Typecheck(new PlusSRightInc {}) && Typecheck(new PlusZRightNeutral {})
    }
  }

  test("Expressions") {
    assertResult(true) {
      Typecheck(new Expressions {})
    }
  }

  test("Lists of naturals") {
    assertResult(true) {
      Typecheck(new NatList {})
    }
  }

  test("Lists of expressions") {
    assertResult(true) {
      Typecheck(new ExprList {})
    }
  }

  test("Full list example") {
    assertResult(true) {
      Typecheck(new FullLists {})
    }
  }

  // test("Syntactic sugar 1") {
  //   assertResult("ObjectBinding(Constant('r),Pi(Variable('p),Const(Constant('o)),Pi(Variable('x),App(Const(Constant('tru)),App(Const(Constant('not)),App(Const(Constant('not)),Var(Variable('p))))),App(Const(Constant('tru)),Var(Variable('p))))))") {
  //     val p = 'p; val o = 'o; val tru = 'tru; val not = 'not
  //     (
  //       'r :> !!(p, o)/ { tru ( not ( not (p) )) ->: (tru (p)) }
  //     ).toString
  //   }
  // }

}

