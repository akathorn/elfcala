package elfcala

import LogicalFramework._
import Kind.Type

object Typecheck {

  type Sgn = List[Binding]
  type Ctx = List[VariableBinding]

  def apply(signature: Signature) = {
    valid(signature.bindings)
  }

  def valid(s: Sgn): Boolean = s match {
    case Nil => true
    case FamilyBinding(d, k) :: rest =>
      valid(rest) && valid(k, Nil, rest) && !(rest contains d)
    case ObjectBinding(c, a) :: rest =>
      valid(rest) && (kind(a, Nil, rest) == Type) && !(rest contains c)
  }

  def valid(c: Ctx, s: Sgn): Boolean = c match {
    case Nil => valid(s)
    case VariableBinding(x, a) :: rest =>
      valid(rest, s) && (kind(a, c, s) == Type) && !(rest contains x)
  }


  def valid(k: Kind, c: Ctx, s: Sgn): Boolean = k match {
    case Type =>
      valid(c, s)
    case Kind.Pi(x, a, kk) =>
      valid(kk, c :+ VariableBinding(Object.Var(x), a), s)
  }

  def kind(a: Family, c: Ctx, s: Sgn): Kind = a match {
    case Family.Const(d) =>
      // TODO: check if it is indeed defined
      (s map { case FamilyBinding(dd, k) if dd == d => k }).head
    case Family.Pi(x, a, b)
      if (kind(b, c :+ VariableBinding(Object.Var(x), a), s) == Type) =>
        Type
    case Family.Abs(x, a, b) =>
      val k = kind(b, c :+ VariableBinding(Object.Var(x), a), s)
      Kind.Pi(x, a, k)
    case Family.App(a, m) =>
      val b = typ(m, c, s)
      val Kind.Pi(x, b2, k) = kind(a, c, s)
      // b should be equal to b2
      k // substitute m in x!
      // TODO: substitution [M/x] K

  }


  def typ(m: Object, c: Ctx, s: Sgn): Family = Family.Const(Constant('a))

}


