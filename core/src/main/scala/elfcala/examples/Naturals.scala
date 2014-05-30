package elfcala.examples

import elfcala._
import LogicalFramework._
import Kind.Type


trait Naturals extends Signature {
  // Variable names to use in expressions
  val n = 'N; val n1 = 'N1; val n2 = 'N2; val n3 = 'N3;
  val d = 'D; val d1 = 'D1; val d2 = 'D2;

  // Terms
  val nat = 'nat :> Type
  val z   = 'z   :> nat
  val s   = 's   :> nat ->: nat

  val plus = 'plus :> nat ->: nat ->: nat ->: Type
  val plus_z = 'plus_z :> !!(n, nat)/ { plus(z)(n)(n) }
  val plus_s = 'plus_s :> !!(n1, nat) (n2, nat) (n3, nat)/
                          { plus(n1)(n2)(n3) ->: plus(s(n1))(n2)(s(n3)) }

}

trait Even extends Naturals {
  val even   = 'even   :> nat ->: Type
  val even_z = 'even_z :> even(z)
  val even_s = 'even_s :> !!(n, nat)/ { even(n) ->: even(s(s(n))) }
}

// Some proofs
trait PlusZRightNeutral extends Naturals {
  val plus_z_right_neutral =
    'plus_z_right_neutral   :> !!(n, nat)/ { plus(n)(z)(n) ->: Type }
  val plus_z_right_neutral_z =
    'plus_z_right_neutral_z :> plus_z_right_neutral(z)(plus_z(z))

  val plus_z_right_neutral_s =
    'plus_z_right_neutral_s :> !!(n, nat) (d, plus(n)(z)(n))/
      { plus_z_right_neutral (n) (d) ->:
        plus_z_right_neutral (s(n)) ( (plus_s(n)(z)(n)) (d)) }
}


trait PlusSRightInc extends Naturals {
  val plus_s_right_inc =
    'plus_s_right_inc :> !!(n1, nat) (n2, nat) (n3, nat)/
      { plus(n1)(n2)(n3) ->: plus(n1)(s(n2))(s(n3)) ->: Type }

  val plus_s_right_inc_z =
    'plus_s_right_inc_z :> !!(n, nat)/
      { plus_s_right_inc(z)(n)(n) (plus_z(n)) (plus_z(s(n))) }

  val plus_s_right_inc_s =
    'plus_s_right_inc_s :> !!(n1, nat) (n2, nat) (n3, nat)/ {
                           !!(d1, plus(n1)(n2)(n3))/ {
                           !!(d2, plus(n1)(s(n2))(s(n3)))/ {
      plus_s_right_inc (n1)(n2)(n3) (d1) (d2) ->:
      plus_s_right_inc (s(n1))(n2)(s(n3)) (plus_s(n1)(n2)(n3) (d1)) (plus_s(n1)(s(n2))(s(n3)) (d2))
  } } }
}
