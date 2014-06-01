package elfcala.examples

import elfcala._
import LogicalFramework._
import LogicalFramework.Kind.Type


trait GenericList extends Signature {
  val listGeneric = generic { t =>
    val list = |{ Type }
    val nil  = |{ list }
    val cons = |{ t ->: list ->: list }
  }
}

trait NatList extends GenericList with Naturals {
  import listGeneric._

  list(nat)
}

trait ExprList extends GenericList with Expressions {
  import listGeneric._

  list(exp)
}

// Relations defined on lists of generic elements
trait ListSize extends GenericList with Naturals {
  import listGeneric._

  val listSize = generic { t =>
    val list_size      = |{ list(t) ->: nat ->: Type }
    val list_size_nil  = |{ list_size (nil(t)) (z) }
    val list_size_cons = |{ !!('N, nat) ('L, list(t)) ('_, t)/
            { list_size ('L) ('N) ->: list_size (cons(t) ('_, 'L)) (s('N)) } }
  }

}

trait FullLists extends ListSize with NatList with ExprList {
  listSize.list_size(nat)
  listSize.list_size(exp)
}
