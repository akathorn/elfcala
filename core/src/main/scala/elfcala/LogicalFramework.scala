package elfcala

object LogicalFramework {
  // Variables
  case class Variable(x: Name)
  case class Constant(c: Name)

  // Terms
  abstract class Term

  // Kinds
  abstract class Kind extends Term

  object Kind {
    case object Type extends Kind
    case class Pi(x: Variable, a: Family, k: Kind) extends Kind
  }

  // Families
  abstract class Family extends Term

  object Family {
    case class Const(d: Constant) extends Family
    case class Pi(x: Variable, a: Family, b: Family) extends Family
    case class Abs(x: Variable, a: Family, b: Family) extends Family
    case class App(a: Family, m: Object) extends Family
  }

  // Objects
  abstract class Object extends Term

  object Object {
    case class Const(c: Constant) extends Object
    case class Var(x: Variable) extends Object
    case class Abs(x: Variable, a: Family, m: Object) extends Object
    case class App(m: Object, n: Object) extends Object
  }


  // Free variables
  def freeVariables(k: Kind): Set[Variable] = k match {
    case Kind.Type =>
      Set.empty
    case Kind.Pi(x, a, k) =>
      (freeVariables(a) ++ freeVariables(k)) - x
  }

  def freeVariables(a: Family): Set[Variable] = a match {
    case Family.Const(_) =>
      Set.empty
    case Family.Pi(x, a, b) =>
      (freeVariables(a) ++ freeVariables(b)) - x
    case Family.Abs(x, a, b) =>
      (freeVariables(a) ++ freeVariables(b)) - x
    case Family.App(a, m) =>
      freeVariables(a) ++ freeVariables(m)
  }

  def freeVariables(m: Object): Set[Variable] = m match {
    case Object.Const(_) =>
      Set.empty
    case Object.Var(x) =>
      Set.empty + x
    case Object.Abs(x, a, b) =>
      (freeVariables(a) ++ freeVariables(b)) - x
    case Object.App(m, n) =>
      freeVariables(m) ++ freeVariables(n)
  }

  // Creates a new variable with a fresh name based on the given variable
  def freshVar(x: Variable): Variable = {
    val Variable(n) = x
    Variable(Name.fresh(n.name))
  }

  // Renaming
  // Kinds
  def rename(x: Variable, y: Variable, k: Kind): Kind = k match {
    case Kind.Type =>
      Kind.Type
    case Kind.Pi(z, a, l) if x == z =>
      Kind.Pi(y, rename(x, y, a), rename(x, y, l))
    case Kind.Pi(z, a, l) =>
      Kind.Pi(z, rename(x, y, a), rename(x, y, l))
  }

  // Families
  def rename(x: Variable, y: Variable, a: Family): Family = a match {
    case Family.Const(_) =>
      a
    case Family.Pi(z, b, c) if x == z =>
      Family.Pi(y, rename(x, y, b), rename(x, y, c))
    case Family.Pi(z, b, c) =>
      Family.Pi(z, rename(x, y, b), rename(x, y, c))
    case Family.Abs(z, b, c) if x == z =>
      Family.Abs(y, rename(x, y, b), rename(x, y, c))
    case Family.Abs(z, b, c) =>
      Family.Abs(z, rename(x, y, b), rename(x, y, c))
    case Family.App(b, m) =>
      Family.App(rename(x, y, b), rename(x, y, m))
  }

  // Objects
  def rename(x: Variable, y: Variable, m: Object): Object = m match {
    case Object.Const(_) =>
      m
    case Object.Var(x) if x == y =>
      Object.Var(y)
    case Object.Abs(z, a, b) if x == z =>
      Object.Abs(x, rename(x, y, a), rename(x, y, b))
    case Object.Abs(z, a, b) =>
      Object.Abs(z, rename(x, y, a), rename(x, y, b))
    case Object.App(m, n) =>
      Object.App(rename(x, y, m), rename(x, y, n))
  }

  // Capture-avoiding substitution
  // Kinds
  def subst(x: Variable, o: Object, k: Kind): Kind = k match {
    case Kind.Type =>
      Kind.Type
    case Kind.Pi(y, a, l) =>
      if (x != y && !(freeVariables(o) contains y)) {
        Kind.Pi(y, subst(x, o, a), subst(x, o, l))
      } else {
        subst(x, o, rename(y, freshVar(y), k))
      }
  }

  // Families
  def subst(x: Variable, o: Object, a: Family): Family = a match {
    case Family.Const(_) =>
      a
    case Family.Pi(y, b, c) if (x != y && !(freeVariables(o) contains y)) =>
        Family.Pi(y, subst(x, o, b), subst(x, o, c))
    case Family.Pi(y, b, c) =>
        subst(x, o, rename(y, freshVar(y), a))
    case Family.Abs(y, b, c) if (x != y && !(freeVariables(o) contains y)) =>
        Family.Abs(y, subst(x, o, b), subst(x, o, c))
    case Family.Abs(y, b, c) =>
        subst(x, o, rename(y, freshVar(y), a))
    case Family.App(b, m) =>
      Family.App(subst(x, o, b), subst(x, o, m))
  }

  // Objects
  def subst(x: Variable, o: Object, p: Object): Object = p match {
    case Object.Const(_) =>
      p
    case Object.Var(y) if x == y =>
      o
    case Object.Var(y) =>
      p
    case Object.Abs(y, a, m) if (x != y && !(freeVariables(o) contains y)) =>
        Object.Abs(y, subst(x, o, a), subst(x, o, m))
    case Object.Abs(y, a, m) =>
        subst(x, o, rename(y, freshVar(y), m))
    case Object.App(m, n) =>
      Object.App(subst(x, o, m), subst(x, o, n))
  }

  // Equality
  def equal(x: Term, y: Term): Boolean = {
    canonicalName(Reduce(x)) == canonicalName(Reduce(y))
  }

  // Canonical renaming
  def canonicalName(x: Term): Term = x match {
    case x: Kind   => canonicalName(x, 0)
    case x: Family => canonicalName(x, 0)
    case x: Object => canonicalName(x, 0)
  }

  // Kinds
  private def canonicalName(k: Kind, count: Int): Kind = k match {
    case Kind.Type =>
      Kind.Type
    case Kind.Pi(x, a, l) =>
      val y = Variable(new Name(count.toString))
      Kind.Pi(y,
              // TODO: It's not clear to me if we can safely do recursion in
              // this way
              canonicalName(rename(x, y, a), count + 1),
              canonicalName(rename(x, y, l), count + 1))
  }

  // Families
  private def canonicalName(a: Family, count: Int): Family = a match {
    case Family.Const(_) =>
      a
    case Family.Pi(x, b, c) =>
      val y = Variable(new Name(count.toString))
      Family.Pi(y,
                canonicalName(rename(x, y, b), count + 1),
                canonicalName(rename(x, y, c), count + 1))
    case Family.Abs(x, b, c) =>
      val y = Variable(new Name(count.toString))
      Family.Abs(y,
                 canonicalName(rename(x, y, b), count + 1),
                 canonicalName(rename(x, y, c), count + 1))
    case Family.App(b, m) =>
      Family.App(canonicalName(b, count + 1), canonicalName(m, count + 1))
  }

  // Objects
  private def canonicalName(m: Object, count: Int): Object = m match {
    case Object.Const(_) =>
      m
    case Object.Var(x) =>
      Object.Var(Variable(new Name(count.toString)))
    case Object.Abs(x, a, b) =>
      val y = Variable(new Name(count.toString))
      Object.Abs(y,
                 canonicalName(rename(x, y, a), count + 1),
                 canonicalName(rename(x, y, b), count + 1))
    case Object.App(m, n) =>
      Object.App(canonicalName(m, count + 1), canonicalName(n, count + 1))
  }

}
