package elfcala

import org.scalatest._

import elfcala._
import elfcala.examples._

class TwelfTests extends FunSuite with Signature {
  test("Twelf server test (low level)") {
    assertResult("%% OK %%") {
      Twelf.init()
      Twelf.serverInput.println("readDecl")
      Twelf.serverInput.println("nat: type.")
      Twelf.serverInput.println("quit")
      Twelf.serverInput.flush()
      val out = Twelf.serverOutput.getLines.toList.last
      Twelf.close()
      out
    }
  }

  test("Twelf test (naturals)") {
    assertResult(true) {
      Twelf.check_signature(new Naturals {})
    }
  }

  test("Twelf test (naturals with proofs)") {
    assertResult(true) {
      Twelf.check_signature(new PlusSRightInc with PlusZRightNeutral {})
    }
  }

  test("Twelf totality test (naturals with proofs)") {
    assertResult(true) {
      Twelf.check_signature(new TwelfExample {})
    }
  }

  test("Twelf test (lists)") {
    assertResult(true) {
      Twelf.check_signature(new FullLists {})
    }
  }

}

