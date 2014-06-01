package elfcala.examples

import elfcala._
import LogicalFramework._
import LogicalFramework.Kind.Type

trait TwelfPairs extends Signature with Naturals with Expressions {
  val pairsGeneric =
    generic { (u, v) =>
      val pair      = |{ Type }
      val pair_cons = |{ u ->: v ->: pair }

      val fst       = |{ pair ->: u ->: Type }
      val fst_cons  = |{ fst (pair_cons('A,'B), 'A) }

      val snd       = |{ pair ->: v ->: Type }
      val snd_cons  = |{ snd (pair_cons('A,'B), 'B) }
    }

  pairsGeneric.pair(nat, exp)
  pairsGeneric.pair(nat, nat)
  pairsGeneric.pair(exp, exp)
}
