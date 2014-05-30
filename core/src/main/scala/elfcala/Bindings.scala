package elfcala

import LogicalFramework._

abstract class SignatureBinding
abstract class ContextBinding
case class FamilyBinding(d: Constant, k: Kind)   extends SignatureBinding
case class ObjectBinding(c: Constant, a: Family) extends SignatureBinding
case class VariableBinding(x: Variable, a: Family) extends ContextBinding
