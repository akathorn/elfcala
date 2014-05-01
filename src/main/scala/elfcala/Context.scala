package elfcala

import LogicalFramework._

class Context {
  var bindings: List[ObjectBinding] = Nil

  def Context(bindings: List[ObjectBinding]) = {
    this.bindings = bindings
  }

  def empty: Context = new Context
}
