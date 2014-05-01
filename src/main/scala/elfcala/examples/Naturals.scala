package elfcala.examples

import elfcala._
import LogicalFramework._
import Kind.Type


class Naturals extends Signature {
  // Terms
  val nat    = 'nat
  val z = 'z
  val s = 's
  val plus = 'plus
  val plus_z = 'plus_z
  val plus_s = 'plus_s


  nat  :> Type
  z    :> nat
  s    :> nat ->: nat

  plus :> nat ->: nat ->: nat ->: Type

  val n = 'n; val n1 = 'n1; val n2 = 'n2; val n3 = 'n3;
  plus_z :> !!(n, nat)/ ( plus(z)(n)(n) )
  plus_s :> !!(n1, nat) (n2, nat) (n3, nat)/
              { plus(n1)(n2)(n3) ->: plus(s(n1))(n2)(s(n3)) }


  println(bindings)
}
