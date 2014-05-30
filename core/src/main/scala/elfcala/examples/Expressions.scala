package elfcala.examples

import elfcala._
import LogicalFramework._
import LogicalFramework.Kind.Type

trait Expressions extends Naturals {
  val exp = 'exp :> Type
  val cst = 'cst :> nat ->: exp
  val vr  = 'vr  :> nat ->: exp
  val pls = 'pls :> exp ->: exp ->: exp
}
