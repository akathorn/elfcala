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


  def generic_impl(c: Context)(decl: c.Tree): c.Tree = {
    import c.universe._

    val genericName = TypeName(c.fresh("Generic"))

    val Function(List(ValDef(_, oldparam, _, _)), Block(defs, _)) = decl

    val TermName(paramname) = oldparam
    val param = TermName(paramname)

    val definitionTransformer = new Transformer {
      // This actually implements a weird fix for a bug related to changing
      // value definitions
      var ctx: Map[TermName, TermName] = Map.empty + (oldparam -> param)

      override def transform(tree: Tree): Tree = tree match {
        case ValDef(modifiers, valname, typ, q"$bind ( $name, $typedef )") =>
          val TermName(name_str) = valname
          val newname = TermName(name_str)
          ctx = ctx + (valname -> newname)

          val newval =
            q"""$bind (PrettyPrinter($param) + "_" + $name, $typedef)"""

          super.transform(ValDef(modifiers, newname, typ, newval))
        case Ident(t: TermName) if (ctx contains t) =>
           super.transform(Ident(ctx(t)))
        case other =>
          super.transform(other)
      }
    }

    val generic_defs = definitionTransformer.transformTrees(defs)

    // // This should be the way to do this!
    // val generic_defs = defs map { d =>
    //   val q"val $valname = $bind ( $name, $typedef )" = d

    //   q"""val $valname =
    //           $bind (PrettyPrinter($param) + "_" + $name, $typedef)"""
    // }

    val generic_function_defs = defs map { d =>
      val q"val $valname = $bind ( $name, $typedef )" = d

      q"""def $valname($param: Family) = {
            if (!(defined contains $param)) {
              define_family($param)
            }
            Symbol(PrettyPrinter($param) + "_" + $name)
          }"""
    }

    val trait_definition = q"""
      trait $genericName {
        var defined: Set[Family] = Set.empty

        def define_family($param: Family) = {
          ..$generic_defs

          defined = defined + $param
        }

        ..$generic_function_defs
      }"""

    val generic_definition = q"""
      $trait_definition

      new $genericName {}
    """

    c.resetLocalAttrs(generic_definition)
  }
}
