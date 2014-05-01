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


  |- (nat  :> Type)
  |- (z    :> nat)
  |- (s    :> nat ->: nat )

  // |- (plus :> nat ->: nat ->: nat ->: Type )
  // |- (plus_z :> nat ->: nat ->: nat ->: Type )


  println(bindings)
}
