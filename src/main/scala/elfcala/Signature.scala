package elfcala

import scala.language.implicitConversions
import LogicalFramework._

abstract class Binding
abstract class SignatureBinding
abstract class ContextBinding
case class FamilyBinding(d: Constant, k: Kind)   extends SignatureBinding
case class ObjectBinding(c: Constant, a: Family) extends SignatureBinding
case class VariableBinding(x: Variable, a: Family) extends ContextBinding

trait Signature {
  var bindings: List[SignatureBinding] = Nil
  var familyConstants: Set[Symbol] = Set.empty
  var objectConstants: Set[Symbol] = Set.empty

  def |-(b: SignatureBinding) = b match {
    case FamilyBinding(Constant(d), _) =>
      familyConstants = familyConstants + d
      bindings = bindings :+ b
      b
    case ObjectBinding(Constant(c), _) =>
      objectConstants = objectConstants + c
      bindings = bindings :+ b
      b
  }

  def bindFamily(s: Symbol)(k: Kind) =
    |- (FamilyBinding(Constant(s), k))
  def bindObject(s: Symbol)(a: Family) =
    |- (ObjectBinding(Constant(s), a))


  // TODO:
  // implicit def symbolToObjectVariable(s: Symbol): Object = Object.Var(Variable(s))
  // implicit def symbolToObjectConstant(s: Symbol): Object = Object.Const(Constant(s))
  implicit def symbolToFamilyConstant(s: Symbol): Family = Family.Const(Constant(s))


  // Syntax sugaring
  // TODO: it's become huge, should it be in another class?

  // Binders
  case class SymbolBinder(s: Symbol) {
    def :>(k: Kind)   = bindFamily(s)(k)
    def :>(a: Family) = bindObject(s)(a)

    def ->:(o: Family) = Family.Pi(Variable('x), o, s)

    def apply(o: Symbol): Application = Application(s, Object.Var(Variable(o)))
    // TODO: we need context information to decide if the second argument of
    // the application is a variable or a constant

    def apply(o: Object): Application = Application(s, o)
    def apply(o: Application): Application =
      Application(s, applicationToObject(o))
  }

  // case class ObjectBinder(t: Object) {
  //   def apply(o: Symbol): Object = Object.App(t, Object.Var(Variable(o)))
  //   def apply(o: Object): Object = Object.App(t, o)
  // }

  case class FamilyBinder(a: Family) {
    def ->:(o: Family) = Family.Pi(Variable('x), o, a)

    def apply(o: Symbol): Family = Family.App(a, Object.Var(Variable(o)))
                                    // TODO: it could also be constant...
    def apply(o: Object): Family = Family.App(a, o)
  }
  case class KindBinder(k: Kind) {
    def ->:(a: Family) = Kind.Pi(Variable('x),
                                 a,
                                 k)
  }

  implicit def symbolToBinder(s: Symbol): SymbolBinder = SymbolBinder(s)
  implicit def familyToBinder(a: Family): FamilyBinder = FamilyBinder(a)
//  implicit def objectToBinder(o: Object): ObjectBinder = ObjectBinder(o)
  implicit def kindToBinder(k: Kind):     KindBinder   = KindBinder(k)


  // Pi sugar
  case class PiSugar(bindings: List[Pair[Symbol, Symbol]]) {
    def /(b: Family): Family = {
      def recBind(bindings: List[Pair[Symbol, Symbol]]): Family = bindings match {
        case Nil =>
          b
        case binding :: rest =>
          Family.Pi(Variable(binding._1),
                    binding._2,
                    recBind(rest))
      }
      recBind(bindings)
    }

    // TODO: eliminate code repetition
    def /(k: Kind): Kind = {
      def recBind(bindings: List[Pair[Symbol, Symbol]]): Kind = bindings match {
        case Nil =>
          k
        case binding :: rest =>
          Kind.Pi(Variable(binding._1),
                  binding._2,
                  recBind(rest))
      }
      recBind(bindings)
    }

    def apply(bind: Pair[Symbol, Symbol]) =
      PiSugar(bindings :+ bind)
  }

  def !!(bind: Pair[Symbol, Symbol]) = PiSugar(List((bind._1, bind._2)))
  // TODO: bind Pair[Symbol, Family]


  // Application sugar
  case class Application(n: Symbol, m: Object) {
    def ->:(o: Family) = o ->: FamilyBinder(applicationToFamily(this))

    def apply(s: Symbol): Family = applicationToFamily(this).apply(s)
    def apply(o: Object): Family = applicationToFamily(this).apply(o)
  }


  implicit def applicationToFamily(a: Application): Family = {
    val Application(n, m) = a
    Family.App(n, m)
  }
  implicit def applicationToObject(a: Application): Object = {
    val Application(n, m) = a
    Object.App(Object.Const(Constant(n)), m)
  }

}
