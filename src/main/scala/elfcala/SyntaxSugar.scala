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
  implicit def symbolToObjectConstant(s: Symbol): Object =
    Object.Const(Constant(s))
  implicit def symbolToName(s: Symbol): Name = Name(s.name)


  // Syntax sugaring

  // Binders
  case class SymbolBinder(s: Symbol) {
    def :>(k: Kind)   = bindFamily(s)(k)
    def :>(a: Family) = bindObject(s)(a)

    def ->:(o: Family) = Family.Pi(Variable(Name.fresh("x")), o, s)

    def apply(o: Symbol): Application =
      if (objectConstants contains o) {
        Application(s, Object.Const(Constant(o)))
      } else {
        Application(s, Object.Var(Variable(o)))
      }

    def apply(o: Object): Application = Application(s, o)
    def apply(o: Application): Application =
      Application(s, applicationToObject(o))
  }

  case class ObjectBinder(t: Object) {
    def apply(o: Symbol): Object =
      if (objectConstants contains o) {
        // TODO: maybe we could use uppercase/lowercase to differenciate?
        Object.App(t, Object.Const(Constant(o)))
      } else {
        Object.App(t, Object.Var(Variable(o)))
      }
    def apply(o: Object): Object = Object.App(t, o)
  }

  case class FamilyBinder(a: Family) {
    def ->:(o: Family) = Family.Pi(Variable(Name.fresh("x")), o, a)

    def apply(o: Symbol): Family =
      if (objectConstants contains o) {
        Family.App(a, Object.Const(Constant(o)))
      } else {
        Family.App(a, Object.Var(Variable(o)))
      }

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
