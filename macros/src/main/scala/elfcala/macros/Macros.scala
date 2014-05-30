package elfcala.macros

import scala.language.experimental.macros


trait SignatureMacros {
  def |(decl: Any): Symbol = macro Macros.bind_impl
}

object Macros {
  import scala.reflect.macros.Context

  def bind_impl(c: Context)(decl: c.Expr[Any]): c.Expr[scala.Symbol] = {
    import c.universe._
    val line = c.enclosingPosition.lineContent
    val result: String =
      // TODO: this fails if you do "val a: SomeType = bla blah"
      // maybe parse it first to get the tree?
      if (line.contains("=")) {
        line.split("=").head.split(" ").last
      } else {
        "undefined"
      }

    val bind = Select(c.prefix.tree, newTermName("bind"))

    c.Expr[scala.Symbol](Apply(bind, List(c.literal(result).tree, decl.tree)))
  }
}
