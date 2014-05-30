package elfcala

import LogicalFramework._

// From "A Framework for Defining Logics"
object Reduce {

  def apply(x: Term): Term = {
    val rx = reduce(x)
    if (rx != x)
      this(reduce(rx))
    else
      x
  }

  private def reduce(x: Term): Term = x match {
    case x: Kind =>
      reduce(x)
    case x: Family =>
      reduce(x)
    case x: Object =>
      reduce(x)
  }

  // Kind reduction
  private def reduce(k: Kind): Kind = k match {
    case Kind.Pi(x, a, k) =>
      // R-PI-KIND
      Kind.Pi(x, reduce(a), reduce(k))
    case _ =>
      // R-REFL
      k
  }

  // Family reduction
  private def reduce(a: Family): Family = a match {
    case Family.App(Family.Abs(x, a, b), m) =>
      // R-BETA-FAM
      subst(x, reduce(m), reduce(b))
    case Family.App(a, m) =>
      // R-APP-FAM
      Family.App(reduce(a), reduce(m))
    case Family.Abs(x, a, b) =>
      // R-ABS-FAM
      Family.Abs(x, reduce(a), reduce(b))
    case Family.Pi(x, a, b) =>
      // R-PI-FAM
      Family.Pi(x, reduce(a), reduce(b))
    case _ =>
      // R-REFL
      a
  }

  // Object reduction
  private def reduce(a: Object): Object = a match {
    case Object.App(Object.Abs(x, a, m), n) =>
      // R-BETA-OBJ
      subst(x, reduce(n), reduce(m))
    case Object.App(m, n) =>
      // R-APP-OBJ
      Object.App(reduce(m), reduce(n))
    case Object.Abs(x, a, m) =>
      // R-ABS-OBJ
      Object.Abs(x, reduce(a), reduce(m))
    case _ =>
      // R-REFL
      a
  }


}
