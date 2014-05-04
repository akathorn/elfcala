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
  plus_z :> !!(n, nat)/ { plus(z)(n)(n) }
  plus_s :> !!(n1, nat) (n2, nat) (n3, nat)/
              { plus(n1)(n2)(n3) ->: plus(s(n1))(n2)(s(n3)) }

}


class Even extends Naturals {
  val even = 'even
  val even_z = 'even_z
  val even_s = 'even_s

  even   :> nat ->: Type
  even_z :> even(z)
  even_s :> !!(n, nat)/ { even(n) ->: even(s(s(n))) }
}

// Some proofs
trait PlusZRightNeutral extends Naturals {
  val plus_z_right_neutral   = 'plus_z_right_neutral
  val plus_z_right_neutral_z = 'plus_z_right_neutral_z
  val plus_z_right_neutral_s = 'plus_z_right_neutral_s

  plus_z_right_neutral   :> !!(n, nat)/ { plus(n)(z)(n) ->: Type }
  plus_z_right_neutral_z :> plus_z_right_neutral(z)(plus_z(z))

  val d = 'd
  plus_z_right_neutral_s :> !!(n, nat) (d, plus)/
      { plus_z_right_neutral (n) (d) ->:
        plus_z_right_neutral (s(n)) (plus_s (d)) }
}


trait PlusSRightInc extends Naturals {
  val plus_s_right_inc   = 'plus_s_right_neutral
  val plus_s_right_inc_z = 'plus_s_right_neutral_z
  val plus_s_right_inc_s = 'plus_s_right_neutral_s

  plus_s_right_inc   :> !!(n1, nat) (n2, nat) (n3, nat)/
      { plus(n1)(n2)(n3) ->: plus(n1)(s(n2))(s(n3)) ->: Type }

  plus_s_right_inc_z :> !!(n, nat)/
      { plus_s_right_inc(z)(n)(n) (plus_z(n)) (plus_z(s(n))) }


  // TODO: implement !!(Symbol, Family)/
  // val d1 = 'd1; val d2 = 'd2;;
  // plus_s_right_inc_s :> !!(n1, nat) (n2, nat) (n3, nat)/ {
  //                       !!(d1, plus(n1)(n2)(n3))/ {
  //                       !!(d2, plus(n1)(s(n2))(s(n3)))/ {
  //     plus_s_right_inc (n1)(n2)(n3) (d1) (d2) ->:
  //     plus_s_right_inc (n1)(s(n2))(s(n3)) (plus_s(n1)(n2)(n3) (d1)) (plus_s(n1)(s(n2))(s(n3)) (d2))
  // } } }
}
