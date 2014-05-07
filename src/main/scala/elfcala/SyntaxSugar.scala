package elfcala

import scala.language.implicitConversions
import LogicalFramework._

trait SyntaxSugar {
  var bindings: List[SignatureBinding]
  var familyConstants: Set[Symbol] = Set.empty
  var objectConstants: Set[Symbol] = Set.empty


  def bindFamily(s: Symbol)(k: Kind) = {
    val b = FamilyBinding(Constant(s), k)
    familyConstants = familyConstants + s
    bindings = bindings :+ b
    b
  }

  def bindObject(s: Symbol)(a: Family) = {
    val b = ObjectBinding(Constant(s), a)
    objectConstants = objectConstants + s
    bindings = bindings :+ b
    b
  }


  // implicit def symbolToObjectVariable(s: Symbol): Object = Object.Var(Variable(s))
  // implicit def symbolToObjectConstant(s: Symbol): Object = Object.Const(Constant(s))
  implicit def symbolToFamilyConstant(s: Symbol): Family =
    Family.Const(Constant(s))
  implicit def symbolToObjectConstant(s: Symbol): Object = {
    // TODO: maybe we could use uppercase/lowercase to differenciate?
    if (objectConstants contains s) {
      Object.Const(Constant(s))
    } else {
      Object.Var(Variable(s))
    }
  }
  implicit def symbolToName(s: Symbol): Name = Name(s.name)


  // Syntax sugaring

  // Binders
  case class SymbolBinder(s: Symbol) {
    def :>(k: Kind)   = bindFamily(s)(k)
    def :>(a: Family) = bindObject(s)(a)

    def ->:(o: Family) = Family.Pi(Variable(Name.fresh("x")), o, s)

    def apply(o: Symbol): Application = SymbolApplication(s, o)

    def apply(o: Object): Application = SymbolApplication(s, o)
    def apply(o: Application): Application =
      SymbolApplication(s, applicationToObject(o))
  }

  case class ObjectBinder(t: Object) {
    def apply(o: Symbol): Object = Object.App(t, o)
    def apply(o: Object): Object = Object.App(t, o)
  }

  case class FamilyBinder(a: Family) {
    def ->:(o: Family) = Family.Pi(Variable(Name.fresh("x")), o, a)

    def apply(o: Symbol): Family = Family.App(a, o)
    def apply(o: Object): Family = Family.App(a, o)
  }
  case class KindBinder(k: Kind) {
    def ->:(a: Family) = Kind.Pi(Variable(Name.fresh("x")),
                                 a,
                                 k)
  }

  implicit def symbolToBinder(s: Symbol): SymbolBinder = SymbolBinder(s)
  implicit def familyToBinder(a: Family): FamilyBinder = FamilyBinder(a)
  implicit def objectToBinder(o: Object): ObjectBinder = ObjectBinder(o)
  implicit def kindToBinder(k: Kind):     KindBinder   = KindBinder(k)


  // Pi sugar
  case class PiSugar(bindings: List[Pair[Symbol, Family]]) {
    def /(b: Family): Family = {
      def recBind(bindings: List[Pair[Symbol, Family]]): Family = bindings match {
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
      def recBind(bindings: List[Pair[Symbol, Family]]): Kind = bindings match {
        case Nil =>
          k
        case binding :: rest =>
          Kind.Pi(Variable(binding._1),
                  binding._2,
                  recBind(rest))
      }
      recBind(bindings)
    }

    def apply(bind: Pair[Symbol, Family]) =
      PiSugar(bindings :+ bind)
  }

  def !!(bind: Pair[Symbol, Family]) = PiSugar(List((bind._1, bind._2)))


  // Application sugar
  abstract class Application {
    def ->:(o: Family): Family

    def apply(s: Symbol): AppApplication
    def apply(o: Object): AppApplication
  }

  case class SymbolApplication(n: Symbol, m: Object) extends Application {
    def ->:(o: Family) = o ->: FamilyBinder(applicationToFamily(this))

    def apply(s: Symbol): AppApplication = AppApplication(this, s)
    def apply(o: Object): AppApplication = AppApplication(this, o)
  }

  case class AppApplication(n: Application, m: Object) extends Application {
    def ->:(o: Family) = o ->: FamilyBinder(applicationToFamily(this))

    def apply(s: Symbol): AppApplication = AppApplication(this, s)
    def apply(o: Object): AppApplication = AppApplication(this, o)
  }


  implicit def applicationToFamily(a: Application): Family = a match {
    case SymbolApplication(n, m) =>
      Family.App(n, m)
    case AppApplication(n, m) =>
      Family.App(n, m)
  }
  implicit def applicationToObject(a: Application): Object = a match {
    case SymbolApplication(n, m) =>
      Object.App(n, m)
    case AppApplication(n, m) =>
      Object.App(n, m)
  }



}
