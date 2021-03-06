package elfcala

import LogicalFramework._
import Kind.Type

object Typecheck {

  type Sgn = List[SignatureBinding]
  type Ctx = List[VariableBinding]

  def apply(signature: Signature) = {
    // TODO: what happens if there is a family binding and an object binding
    // for the same constant?
    // println(PrettyPrinter.twelfPrint(signature.bindings))
    valid(signature.bindings)
  }

  def apply(m: Object, signature: Signature) = {
    typ(m, Nil, signature.bindings)
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

  def kind(f: Family, c: Ctx, s: Sgn): Kind = f match {
    case Family.Const(d) =>
      s collectFirst { case FamilyBinding(dd, k) if dd == d => k } match {
        case Some(k) => k
        case None => throw new Exception("Binding for " + d + " not found")
        case other => throw new Exception("invalid binding")
      }

    case Family.Pi(x, a, b)
      if (kind(b, c :+ VariableBinding(x, a), s) == Type) =>
        // TODO: c :+ VariableBinding(x, a) hast to be a valid context!  we
        // could be introducing an invalid a in the context, for instance if
        // it has free variables. This happens many times in this file, I
        // think.
        Type
    case Family.Abs(x, a, b) =>
      val k = kind(b, c :+ VariableBinding(x, a), s)
      Kind.Pi(x, a, k)
    case Family.App(a, m) =>
      val b = typ(m, c, s)
      val Kind.Pi(x, b2, k) = kind(a, c, s)
      if (equal(b, b2)) {
        subst(x, m, k)
      } else {
        // TODO: this could really be more helpful than this
        throw new Exception("Type error in expression " + PrettyPrinter(f) +
                            ": " + PrettyPrinter(b) +
                            " expected but " + PrettyPrinter(b2) + " found")
      }
    // TODO: (B-CONV-FAM) ?

  }

  def typ(o: Object, c: Ctx, s: Sgn): Family = o match {
    case Object.Const(cnst: Constant) =>
      s collectFirst { case ObjectBinding(cc, a) if cc == cnst => a } match {
        case Some(a) => a
        case None => throw new Exception("Type error in expression '" +
                                         PrettyPrinter(o) + "': " +
                                         " Binding for constant " +
                                         PrettyPrinter(cnst) + " not found")
        case other => throw new Exception("invalid binding")
      }

    case Object.Var(x: Variable) =>
      c collectFirst { case VariableBinding(xx,a) if xx == x => a } match {
        case Some(a) => a
        case None =>
          throw new Exception("Binding for " + x + " not found")
        case other => throw new Exception("invalid binding")
      }

    case Object.Abs(x: Variable, a: Family, m: Object) =>
      val b = typ(m, c :+ VariableBinding(x, a), s)
      Family.Pi(x, a, b)

    case Object.App(m: Object, n: Object) =>
      val Family.Pi(x, a, b) = typ(m, c, s)
      if (equal(typ(n, c, s), a)) {
        subst(x, n, b)
      } else {
        throw new Exception("Type error in expression '" + PrettyPrinter(o) +
                            "': (" + PrettyPrinter(a) + ") expected but (" +
                            PrettyPrinter(typ(n, c, s)) + ") found")
      }

    // TODO: (B-CONV-OBJ) ?
  }
}


