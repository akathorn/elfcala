package elfcala

import scala.io._
import LogicalFramework._
import LogicalFramework.Kind.Type

abstract class TwelfSignatureBinding extends SignatureBinding
case class Worlds(a: Family, args: List[Object])
     extends TwelfSignatureBinding
case class ModeSpec(x: Object, m: String) extends TwelfSignatureBinding
case class Mode(a: Family, modes: List[ModeSpec])
     extends TwelfSignatureBinding
case class Total(x: Object, a: Family, args: List[Object])
     extends TwelfSignatureBinding

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

trait TwelfSignature extends Signature {
  def -|(b: TwelfSignatureBinding) = {
    bindings = bindings :+ b
    b
  }

}


trait TwelfExample extends TwelfSignature {
  // Terms
  val nat    = 'nat
  val z = 'z
  val s = 's
  val plus = 'plus
  val plus_z = 'plus_z
  val plus_s = 'plus_s


  nat  :> Type
  z    :> nat
  s    :> nat ->: nat

  plus :> nat ->: nat ->: nat ->: Type

  val n = 'n; val n1 = 'n1; val n2 = 'n2; val n3 = 'n3;
  plus_z :> !!(n, nat)/ { plus(z)(n)(n) }
  plus_s :> !!(n1, nat) (n2, nat) (n3, nat)/
              { plus(n1)(n2)(n3) ->: plus(s(n1))(n2)(s(n3)) }

  val even = 'even
  val even_z = 'even_z
  val even_s = 'even_s

  even   :> nat ->: Type
  even_z :> even(z)
  even_s :> !!(n, nat)/ { even(n) ->: even(s(s(n))) }

  val plus_z_right_neutral   = 'plus_z_right_neutral
  val plus_z_right_neutral_z = 'plus_z_right_neutral_z
  val plus_z_right_neutral_s = 'plus_z_right_neutral_s

  plus_z_right_neutral   :> !!(n, nat)/ { plus(n)(z)(n) ->: Type }
  -| { Mode(plus_z_right_neutral, List(ModeSpec('N, "+"), ModeSpec('D, "-"))) }

  plus_z_right_neutral_z :> plus_z_right_neutral(z)(plus_z(z))

  val d = 'd
  plus_z_right_neutral_s :> !!(n, nat) (d, plus(n)(z)(n))/
      { plus_z_right_neutral (n) (d) ->:
        plus_z_right_neutral (s(n)) ( (plus_s(n)(z)(n)) (d)) }
  -| { Worlds(plus_z_right_neutral, List('_, '_)) }
  -| { Total('N, plus_z_right_neutral, List('N, '_)) }


  val plus_s_right_inc   = 'plus_s_right_neutral
  val plus_s_right_inc_z = 'plus_s_right_neutral_z
  val plus_s_right_inc_s = 'plus_s_right_neutral_s

  plus_s_right_inc   :> !!(n1, nat) (n2, nat) (n3, nat)/
      { plus(n1)(n2)(n3) ->: plus(n1)(s(n2))(s(n3)) ->: Type }

  plus_s_right_inc_z :> !!(n, nat)/
      { plus_s_right_inc(z)(n)(n) (plus_z(n)) (plus_z(s(n))) }


}


object Twelf {
  var serverInput  : java.io.PrintStream = null
  var serverOutput : Source     = null
  var serverError  : Source     = null
  var server : Process = null

  var command = "./twelf-server"


  def init() = {
    val pb = new java.lang.ProcessBuilder(command)
    server = pb.start()
    serverInput = new java.io.PrintStream(server.getOutputStream())
    serverOutput = Source.fromInputStream(server.getInputStream())
    serverError = Source.fromInputStream(server.getErrorStream())
  }

  def close() = {
    server.waitFor()

    serverInput.close()
    serverOutput.close()
    serverError.close()
  }


  def check_signature(sgn: Signature) = {
    init()

    val tempfile = new java.io.PrintWriter(new java.io.File("tempfile.elf"))

    tempfile.print(TwelfPrinter.twelfPrint(sgn.bindings))
    tempfile.close()

    serverInput.println("loadFile tempfile.elf")
    serverInput.println("quit")
    serverInput.flush()
    val lines = serverOutput.getLines.toList
    val out = lines.last
    close()

    if(out != "%% OK %%") {
      println("Twelf failed! Output:")
      println( (lines map ("\t"+_)).mkString("\n") )
    }
    out == "%% OK %%"
  }
}
