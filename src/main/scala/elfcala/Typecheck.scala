package elfcala

import LogicalFramework._
import Kind.Type

object Typecheck {

  type Sgn = List[SignatureBinding]
  type Ctx = List[VariableBinding]

  def apply(signature: Signature) = {
    // TODO: what happens if there is a family binding and an object binding
    // for the same constant?
    println(PrettyPrinter(signature.bindings))
    valid(signature.bindings)
  }

  def valid(s: Sgn): Boolean = s match {
    case Nil => true
    case _ =>
      val rest = s.init
      (s.last) match {
        case FamilyBinding(d, k) =>
          valid(rest) && valid(k, Nil, rest) && !(rest contains d)
        case ObjectBinding(c, a) =>
          valid(rest) && (kind(a, Nil, rest) == Type) && !(rest contains c)
        case other =>
          throw new Exception("Invalid binding")
      }
  }

  def valid(c: Ctx, s: Sgn): Boolean = c match {
    case Nil => valid(s)
    case _ =>
      val rest = c.init
      (c.last) match {
        case VariableBinding(x, a) =>
          valid(rest, s) && (kind(a, c, s) == Type) && !(rest contains x)
        case other =>
          throw new Exception("Invalid binding")
      }
  }

  def valid(k: Kind, c: Ctx, s: Sgn): Boolean = k match {
    case Type =>
      valid(c, s)
    case Kind.Pi(x, a, kk) =>
      valid(kk, c :+ VariableBinding(x, a), s)
  }

  def kind(a: Family, c: Ctx, s: Sgn): Kind = a match {
    case Family.Const(d) =>
      s collectFirst { case FamilyBinding(dd, k) if dd == d => k } match {
        case Some(k) => k
        case None => throw new Exception("Binding for " + d + " not found")
        case other => throw new Exception("invalid binding")
      }

    case Family.Pi(x, a, b)
      if (kind(b, c :+ VariableBinding(x, a), s) == Type) =>
        Type
    case Family.Abs(x, a, b) =>
      val k = kind(b, c :+ VariableBinding(x, a), s)
      Kind.Pi(x, a, k)
    case Family.App(a, m) =>
      val b = typ(m, c, s)
      val Kind.Pi(x, b2, k) = kind(a, c, s)
      // b should be equal to b2
      // TODO: define equality
      k // TODO: substitute m in x!
      // TODO: substitution [M/x] K

    // TODO: (B-CONV-FAM) ?

  }

  def typ(m: Object, c: Ctx, s: Sgn): Family = m match {
    case Object.Const(cnst: Constant) =>
      s collectFirst { case ObjectBinding(cc, a) if cc == cnst => a } match {
        case Some(a) => a
        case None => throw new Exception
        case other => throw new Exception("invalid binding")
      }

    case Object.Var(x: Variable) =>
      c collectFirst { case VariableBinding(xx,a) if xx == x => a } match {
        case Some(a) => a
        case None =>
          println(c)
          throw new Exception("Binding for " + x + " not found")
        case other => throw new Exception("invalid binding")
      }

    case Object.Abs(x: Variable, a: Family, m: Object) =>
      val b = typ(m, c :+ VariableBinding(x, a), s)
      Family.Pi(x, a, b)

    case Object.App(m: Object, n: Object) =>
      val Family.Pi(x, a, b) = typ(m, c, s)
      if (typ(n, c, s) == a) {
        b // TODO: substitute n in x!
      } else {
        throw new Exception
      }

    // TODO: (B-CONV-OBJ) ?
  }
}


