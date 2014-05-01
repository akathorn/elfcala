package elfcala

object LogicalFramework {
  // Variables
  case class Variable(x: Symbol)
  case class Constant(a: Symbol)

  // TODO: define equality between terms (independent from variable names)

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
}


