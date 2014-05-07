package elfcala.examples

import elfcala._
import LogicalFramework._
import LogicalFramework.Kind.Type

trait Expressions extends Naturals {
  val exp = 'exp
  val cst = 'cst
  val vr = 'vr
  val pls = 'pls

  exp :> Type
  cst :> nat ->: exp
  vr :> nat ->: exp
  pls :> exp ->: exp ->: exp
}
