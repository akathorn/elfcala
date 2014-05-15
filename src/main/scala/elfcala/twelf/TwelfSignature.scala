package elfcala.twelf

import scala.language.implicitConversions

import elfcala.LogicalFramework
import elfcala.LogicalFramework._
import elfcala.Signature
import elfcala.SignatureBinding

abstract class TwelfSignatureBinding extends SignatureBinding
case class Worlds(a: Family, args: List[Object])
     extends TwelfSignatureBinding
case class ModeSpec(x: Object, m: String)
     extends TwelfSignatureBinding
case class Mode(a: Family, modes: List[ModeSpec])
     extends TwelfSignatureBinding
case class Total(x: Object, a: Family, args: List[Object])
     extends TwelfSignatureBinding


trait TwelfSignature extends Signature {
  def %(b: TwelfSignatureBinding) = {
    bindings = bindings :+ b
    b
  }

  // Mode Syntax Sugar
  def mode(s: Symbol): Mode =  Mode(s, List())

  case class ChainModeSpec(m: Mode) {
    def apply(spec: ModeSpec): Mode = m match {
      case Mode(a, modes) =>
        Mode(a, modes :+ spec)
    }

    def --(s: Symbol): Mode = m match {
      case Mode(a, modes) =>
        Mode(a, modes :+ ModeSpec(s, "-"))
    }

    def ++(s: Symbol): Mode = m match {
      case Mode(a, modes) =>
        Mode(a, modes :+ ModeSpec(s, "+"))
    }
  }

  implicit def chainModeSpec(m: Mode): ChainModeSpec = ChainModeSpec(m)

  // Worlds Syntax Sugar
  def worlds(s: Symbol): Worlds = Worlds(s, List())

  case class ChainWorlds(w: Worlds) {
    def apply(s: Symbol): Worlds = w match {
      case Worlds(a, args) =>
        Worlds(a, args :+ (s: Object))
    }
  }

  implicit def chainWorlds(m: Worlds): ChainWorlds = ChainWorlds(m)

  // Totality assertion Syntax Sugar
  def total(x: Symbol)(a: Symbol): Total = Total(x, a, List())

  case class ChainTotal(t: Total) {
    def apply(s: Symbol): Total = t match {
      case Total(x, a, args) =>
        Total(x, a, args :+ (s: Object))
    }
  }

  implicit def chainTotal(t: Total): ChainTotal = ChainTotal(t)


}

