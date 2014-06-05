package elfcala.examples.twelf


import elfcala.examples._
import elfcala.twelf._
import elfcala.LogicalFramework._
import elfcala.LogicalFramework.Kind.Type

trait TwelfPairs extends TwelfSignature with Naturals with Expressions {
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
