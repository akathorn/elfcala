package elfcala.examples

import elfcala._
import LogicalFramework._
import LogicalFramework.Kind.Type

trait LFList extends Signature {
  // def listOf(t: Family) = {
  //   val list = Symbol(PrettyPrinter(t) + "-list")
  //   val nil  = Symbol(PrettyPrinter(t) + "-nil" )
  //   val cons = Symbol(PrettyPrinter(t) + "-cons")

  //   list :> Type
  //   nil  :> list
  //   cons :> t ->: list ->: list

  //   (list, nil, cons)
  // }
}

// Examples with simple lists
trait NatList extends LFList with Naturals {
  // listOf(nat)
}

trait ExprList extends LFList with Expressions {
  // listOf(exp)
}

// Relations defined on lists of generic elements
trait ListSize extends LFList with Naturals {
  // override def listOf(t: Family) = {
  //   val (list, nil, cons) = super.listOf(t)

  //   val list_size      = Symbol(PrettyPrinter(t) + "-list-size")
  //   val list_size_nil  = Symbol(PrettyPrinter(t) + "-list-size/nil" )
  //   val list_size_cons = Symbol(PrettyPrinter(t) + "-list-size/cons")

  //   list_size      :> list ->: nat ->: Type
  //   list_size_nil  :> list_size (nil) (z)
  //   list_size_cons :> !!('N, nat) ('L, list) ('_, t)/
  //                { list_size ('L) ('N) ->: list_size (cons ('_) ('L)) (s('N)) }

  //   (list, nil, cons)
  // }
}

trait FullLists extends ListSize with NatList with ExprList
