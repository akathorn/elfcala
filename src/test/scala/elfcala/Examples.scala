package elfcala

import org.scalatest._

import elfcala._
import elfcala.examples._

class ExamplesTests extends FunSuite with Signature {
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
}

