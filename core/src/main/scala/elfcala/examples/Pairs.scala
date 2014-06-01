package elfcala.examples

import elfcala._
import LogicalFramework._
import LogicalFramework.Kind.Type

trait Pairs extends Signature with Naturals with Expressions {
  val pairsGeneric =
    generic { (u, v) =>
      val pair      = |{ Type }
      val pair_cons = |{ u ->: v ->: pair }

      val fst       = |{ !!('A, u) ('B, v)/ { pair ->: u ->: Type } }
      val fst_cons  = |{ !!('A, u) ('B, v)/
                        { fst('A, 'B) (pair_cons('A,'B), 'A) } }

      val snd       = |{ !!('A, u) ('B, v)/ { pair ->: v ->: Type } }
      val snd_cons  = |{ !!('A, u) ('B, v)/
                        { snd('A, 'B) (pair_cons('A,'B), 'B) } }
    }

  pairsGeneric.pair(nat, exp)
  pairsGeneric.pair(nat, nat)
  pairsGeneric.pair(exp, exp)
}
