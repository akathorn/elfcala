package elfcala

object LogicalFramework {
  // Variables
  case class Variable(x: Symbol)
  case class Constant(a: Symbol)

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
  def freeVariables(k: Kind): Set[Symbol] = k match {
    case Kind.Type =>
      Set.empty
    case Kind.Pi(Variable(x), a, k) =>
      (freeVariables(a) ++ freeVariables(k)) - x
  }

  def freeVariables(a: Family): Set[Symbol] = a match {
    case Family.Const(_) =>
      Set.empty
    case Family.Pi(Variable(x), a, b) =>
      (freeVariables(a) ++ freeVariables(b)) - x
    case Family.Abs(Variable(x), a, b) =>
      (freeVariables(a) ++ freeVariables(b)) - x
    case Family.App(a, m) =>
      freeVariables(a) ++ freeVariables(m)
  }

  def freeVariables(o: Object): Set[Symbol] = o match {
    case Object.Const(_) =>
      Set.empty
    case Object.Var(Variable(x)) =>
      Set.empty + x
    case Object.Abs(Variable(x), a, b) =>
      (freeVariables(a) ++ freeVariables(b)) - x
    case Object.App(m, n) =>
      freeVariables(m) ++ freeVariables(n)
  }


  // Renaming
  // Kinds
  // def rename(x: Variable, y: Variable, k: Kind): Kind = k match {
  //   case Kind.Type =>
  //     Kind.Type
  //   case Kind.Pi(z, a, l) if x == z =>
  //     Kind.Pi(y, rename(x, y, a), rename(x, y, l))
  // }

  // Capture-avoiding substitution
  // Kinds
  // def subst(x: Variable, o: Object, k: Kind): Kind = k match {
  //   case Kind.Type =>
  //     Kind.Type
  //   case Kind.Pi(y, a, l) =>
  //     if (x != y && !(freeVariables(o) contains y)) {

  //     } else {
  //       subst(x, o, rename(y, Symbol.fresh(
  //     }
  // }

}


