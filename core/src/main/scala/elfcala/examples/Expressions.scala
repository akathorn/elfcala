package elfcala.examples

import elfcala._
import LogicalFramework._
import LogicalFramework.Kind.Type

trait Expressions extends Naturals {
  val exp = |{ Type }
  val cst = |{ nat ->: exp }
  val vr  = |{ nat ->: exp }
  val pls = |{ exp ->: exp ->: exp }
}
