package elfcala

import LogicalFramework._

trait Signature extends SyntaxSugar {
  import scala.language.experimental.macros
  var bindings: List[SignatureBinding] = Nil

  def generic(decl: Family => Unit): Any =
    macro elfcala.macros.Macros.generic_impl

  def generic(decl: (Family, Family) => Unit): Any =
    macro elfcala.macros.Macros.generic_impl

  def generic(decl: (Family, Family, Family) => Unit): Any =
    macro elfcala.macros.Macros.generic_impl
}
