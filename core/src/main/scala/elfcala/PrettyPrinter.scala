package elfcala

import LogicalFramework._
import Kind.Type

object PrettyPrinter extends PrettyPrinter

trait PrettyPrinter {
  def twelfPrint(bds: List[SignatureBinding]): String =
    ("" /: (bds map { x => this(x) + ".\n" })) (_+_)

  def apply(bds: List[SignatureBinding]): String =
    ("" /: (bds map { x => this(x) + "\n" })) (_+_)

  // Bindings
  def apply(b: SignatureBinding): String = b match {
    case FamilyBinding(d: Constant, k: Kind) =>
      this(d) + ": " + this(k)
    case ObjectBinding(c: Constant, a: Family) =>
      this(c) + ": " + this(a)
  }

  def apply(b: VariableBinding): String = b match {
    case VariableBinding(x: Variable, a: Family) =>
      this(x) + ": " + this(a)
  }

  // Variables and constants
  def apply(v: Variable): String = v match {
    case Variable(x) => x.name
  }
  def apply(c: Constant): String = c match {
    case Constant(a) => a.name
  }

  // Kind printing
  def apply(k: Kind): String = k match {
    case Type =>
      "type"
    case Kind.Pi(x, a, k) =>
      if (freeVariables(k) contains x) {
        // Use {x:t} notation
        "{" + this(x) + ":" + this(a) + "} " + this(k)
      } else {
        // Use -> notation
        k match {
          case Kind.Pi(y, _, _) => // if (freeVariables(k) contains y) =>
            "(" + this(a) + ") -> " + this(k)
          case _ =>
            this(a) + " -> " + this(k)
        }
      }
  }

  // Family printing
  def apply(a: Family): String = a match {
    case Family.Const(d: Constant) =>
      this(d)
    case Family.Pi(x: Variable, a: Family, b: Family) =>
      if (freeVariables(b) contains x) {
        // Use {x:t} notation
        "{" + this(x) + ":" + this(a) + "} " + this(b)
      } else {
        // Use -> notation
        a match {
          case Family.Pi(y, _, _) => // if (freeVariables(b) contains y) =>
            "(" + this(a) + ") -> " + this(b)
          case _ =>
            this(a) + " -> " + this(b)
        }
      }
    case Family.Abs(x: Variable, a: Family, b: Family) =>
      "λ" + this(x) + ":" + this(a) + ". " + this(b)
    case Family.App(a: Family, m: Object) => m match {
      case Object.App(_, _) =>
        this(a) + " (" + this(m) + ")"
      case _ =>
        this(a) + " " + this(m)
    }
  }

  // Object printing
  def apply(a: Object): String = a match {
    case Object.Const(d: Constant) =>
      this(d)
    case Object.Var(d: Variable) =>
      this(d)
    case Object.Abs(x: Variable, a: Family, b: Object) =>
      "λ" + this(x) + ":" + this(a) + ". " + this(b)
    case Object.App(a: Object, m: Object) => m match {
      case Object.App(_, _) =>
        this(a) + " (" + this(m) + ")"
      case _ =>
        this(a) + " " + this(m)
    }
  }


}
