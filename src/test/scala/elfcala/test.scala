package elfcala

import org.scalatest._

import elfcala._
import elfcala.examples._

import LogicalFramework._

class Test extends FunSuite with Signature {
  test("First order logic") {
    assertResult(true) {
      class FO extends FirstOrderLogic
      val fol = new FO
      Typecheck(fol)
    }
  }

  test("Natural numbers") {
    assertResult(true) {
      class Nats extends Naturals
      val naturals = new Nats
      //      val even = new Even
      Typecheck(naturals) //&& Typecheck(even)
    }
  }

  test("Expressions") {
    assertResult(true) {
      class Exprs extends Expressions
      val exprs = new Exprs
      Typecheck(exprs)
    }
  }

  test("Lists of naturals") {
    assertResult(true) {
      class Lst extends NatList
      val lists = new Lst
      Typecheck(lists)
    }
  }

  test("Lists of expressions") {
    assertResult(true) {
      class Lst extends ExprList
      val lists = new Lst
      Typecheck(lists)
    }
  }

  test("Full list example") {
    assertResult(true) {
      class Lst extends FullLists
      val lists = new Lst
      Typecheck(lists)
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

