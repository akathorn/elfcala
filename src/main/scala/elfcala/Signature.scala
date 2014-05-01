package elfcala

import scala.language.implicitConversions
import LogicalFramework._

abstract class Binding
case class FamilyBinding(d: Family.Const, k: Kind)   extends Binding
case class ObjectBinding(c: Object.Const, a: Family) extends Binding
case class VariableBinding(x: Object.Var, a: Family) extends Binding

trait Signature {
  var bindings: List[Binding] = Nil
  var familyConstants: Set[Symbol] = Set.empty
  var objectConstants: Set[Symbol] = Set.empty

  def |-(b: Binding) = b match {
    case FamilyBinding(Family.Const(Constant(d)), _) =>
      familyConstants = familyConstants + d
      bindings = bindings :+ b
    case ObjectBinding(Object.Const(Constant(c)), _) =>
      objectConstants = objectConstants + c
      bindings = bindings :+ b
  }

  def bindFamily(s: Symbol)(k: Kind) =
    FamilyBinding(Family.Const(Constant(s)), k)
  def bindObject(s: Symbol)(a: Family) =
    ObjectBinding(Object.Const(Constant(s)), a)


  // Syntax sugaring
  case class SymbolBinder(s: Symbol) {
    def :>(k: Kind)   = bindFamily(s)(k)
    def :>(a: Family) = bindObject(s)(a)
    def :>(o: Symbol) = bindObject(s)(Family.Const(Constant(o)))


    def ->:(o: Symbol) = Family.Pi(Variable('x),
                                   Family.Const(Constant(o)),
                                   Family.Const(Constant(s)))
    def ->:(o: Family) = Family.Pi(Variable('x),
                                   o,
                                   Family.Const(Constant(s)))

    def apply(o: Symbol): Application = Application(s, Object.Var(Variable(o)))
    def apply(o: Object): Application = Application(s, o)
  }

  case class Application(n: Symbol, m: Object) {
    def ->:(o: Family) = o ->: FamilyBinder(applicationToFamily(this))
  }


  case class FamilyBinder(a: Family) {
    def ->:(s: Symbol) = Family.Pi(Variable('x),
                                   Family.Const(Constant(s)),
                                   a)
    def ->:(o: Family) = Family.Pi(Variable('x),
                                   o,
                                   a)
  }

  case class KindBinder(k: Kind) {
    def ->:(s: Symbol) = Kind.Pi(Variable('x),
                                   Family.Const(Constant(s)),
                                   k)
  }



  implicit def symbolToBinder(s: Symbol): SymbolBinder = SymbolBinder(s)
  implicit def familyToBinder(a: Family): FamilyBinder = FamilyBinder(a)
  implicit def kindToBinder(k: Kind):     KindBinder   = KindBinder(k)
  implicit def applicationToFamily(a: Application): Family = {
    val Application(n, m) = a
    Family.App(Family.Const(Constant(n)), m)
  }
  implicit def applicationToObject(a: Application): Object = {
    val Application(n, m) = a
    Object.App(Object.Const(Constant(n)), m)
  }

}
