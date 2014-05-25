package elfcala

import org.scalatest._

import elfcala._
import elfcala.examples.twelf._
import elfcala.examples._
import elfcala.twelf._

class TwelfTests extends FunSuite with Signature {
  test("Twelf server test (low level)") {
    assertResult("%% OK %%") {
      TwelfServer.init()
      TwelfServer.serverInput.println("readDecl")
      TwelfServer.serverInput.println("nat: type.")
      TwelfServer.serverInput.println("quit")
      TwelfServer.serverInput.flush()
      val out = TwelfServer.serverOutput.getLines.toList.last
      TwelfServer.close()
      out
    }
  }

  test("Twelf test (naturals)") {
    assertResult(true) {
      TwelfCheck(new Naturals {})
    }
  }

  test("Twelf test (naturals with proofs)") {
    assertResult(true) {
      TwelfCheck(new PlusSRightInc with PlusZRightNeutral {})
    }
  }

  test("Twelf totality test (naturals with proofs)") {
    assertResult(true) {
      TwelfCheck(new TwelfNaturals {})
    }
  }

  test("Twelf test (lists)") {
    assertResult(true) {
      TwelfCheck(new FullLists {})
    }
  }

}

