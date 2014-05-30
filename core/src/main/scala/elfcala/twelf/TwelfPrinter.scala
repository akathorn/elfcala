package elfcala.twelf

import elfcala.PrettyPrinter
import elfcala.SignatureBinding

object TwelfPrinter extends PrettyPrinter {
  override def apply(b: SignatureBinding): String = b match {
    case t: TwelfSignatureBinding =>
      this(t)
    case _ =>
      super.apply(b)
  }

  def apply(t: TwelfSignatureBinding): String = t match {
    case Worlds(a, args) =>
      "%worlds () (" + this(a) + " " + args.map(this.apply).mkString(" ") + ")"
    case ModeSpec(x, m) =>
      m + this(x)
    case Mode(a, modes) =>
      "%mode " + this(a) + " " + modes.map(this.apply).mkString(" ")
    case Total(x, a, args) =>
      "%total " + this(x) +
        " (" + this(a) + " " + args.map(this.apply).mkString(" ") + ")"
  }
}
