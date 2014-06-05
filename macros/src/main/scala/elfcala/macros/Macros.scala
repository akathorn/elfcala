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
        c.fresh("undefined")
      }

    val bind = Select(c.prefix.tree, newTermName("bind"))

    c.Expr[scala.Symbol](Apply(bind, List(c.literal(result).tree, decl.tree)))
  }


  def generic_impl(c: Context)(decl: c.Tree): c.Tree = {
    import c.universe._

    val genericName = TypeName(c.fresh("Generic"))

    val Function(original_params, defs) = decl

    val original_params_names = original_params collect {
      case ValDef(_, termname, _, _) =>
        termname
    }

    val params = original_params_names collect {
      case TermName(name) =>
        // We have to create new copies, doesn't seem to work otherwise
        TermName(name)
    }
    val params_with_types = params map { p => q"$p: Family" }

    val pretty_names   = params map { n => q"PrettyPrinter($n)" }
    val generic_prefix = q"""List(..$pretty_names).mkString("_")"""

    val definitionTransformer = new Transformer {
      // This actually implements a weird fix for a bug related to changing
      // value definitions
      var ctx: Map[TermName, TermName] =
        (original_params_names zip params).toMap

      override def transform(tree: Tree): Tree = tree match {
        case ValDef(modifiers, valname, typ, q"$bind ( $name, $typedef )") =>
          val TermName(name_str) = valname
          val newname = TermName(name_str)
          ctx = ctx + (valname -> newname)

          val newval =
            q"""$bind ($generic_prefix + "_" + $name, $typedef)"""

          super.transform(ValDef(modifiers, newname, typ, newval))
        case Ident(t: TermName) if (ctx contains t) =>
           super.transform(Ident(ctx(t)))
        case other =>
          super.transform(other)
      }
    }

    val generic_defs = definitionTransformer.transform(defs)

    // // This should be the way to do this!
    // val generic_defs = defs map { d =>
    //   val q"val $valname = $bind ( $name, $typedef )" = d

    //   q"""val $valname =
    //           $bind (PrettyPrinter($param) + "_" + $name, $typedef)"""
    // }

    val generic_function_defs = defs collect { d => d match {
      case q"val $valname = $bind ( $name, $typedef )" =>
        q"""def $valname(..$params_with_types) = {
              if (!(defined contains List(..$params))) {
                define_family(..$params)
              }
              Symbol($generic_prefix + "_" + $name)
            }"""
      }
    }

    val trait_definition = q"""
      trait $genericName {
        private var defined: Set[List[Family]] = Set.empty

        private def define_family(..$params_with_types) = {
          ..$generic_defs

          defined = defined + List(..$params)
        }

        def apply(..$params_with_types) =
          if (!(defined contains List(..$params))) {
            define_family(..$params)
          }

        ..$generic_function_defs
      }"""

    val generic_definition = q"""
      import elfcala.PrettyPrinter

      $trait_definition

      new $genericName {}
    """

    c.resetLocalAttrs(generic_definition)
  }
}
