package elfcala

object LogicalFramework {
  // Variables
  case class Variable(x: Name)
  case class Constant(a: Name)

  // Kinds
  abstract class Kind

  object Kind {
    case object Type extends Kind
    case class Pi(x: Variable, a: Family, k: Kind) extends Kind
  }

  // Families
  abstract class Family

  object Family {
    case class Const(d: Constant) extends Family
    case class Pi(x: Variable, a: Family, b: Family) extends Family
    case class Abs(x: Variable, a: Family, b: Family) extends Family
    case class App(a: Family, m: Object) extends Family
  }

  // Objects
  abstract class Object

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

  def freeVariables(o: Object): Set[Variable] = o match {
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
  def rename(x: Variable, y: Variable, o: Object): Object = o match {
    case Object.Const(_) =>
      o
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
    case Family.Pi(y, b, c) =>
      if (x != y && !(freeVariables(o) contains y)) {
        Family.Pi(y, subst(x, o, b), subst(x, o, c))
      } else {
        subst(x, o, rename(y, freshVar(y), a))
      }
    case Family.Abs(y, b, c) =>
      if (x != y && !(freeVariables(o) contains y)) {
        Family.Abs(y, subst(x, o, b), subst(x, o, c))
      } else {
        subst(x, o, rename(y, freshVar(y), a))
      }
    case Family.App(b, m) =>
      Family.App(subst(x, o, b), subst(x, o, m))
  }

  // Objects
  def subst(x: Variable, o: Object, p: Object): Object = p match {
    case Object.Const(_) =>
      p
    case Object.Var(y) if x == y =>
      o
    case Object.Abs(y, a, m) =>
      if (x != y && !(freeVariables(o) contains y)) {
        Object.Abs(y, subst(x, o, a), subst(x, o, m))
      } else {
        subst(x, o, rename(y, freshVar(y), m))
      }
    case Object.App(m, n) =>
      Object.App(subst(x, o, m), subst(x, o, n))
  }


}


