package elfcala

import scala.io._

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

    tempfile.print(PrettyPrinter.twelfPrint(sgn.bindings))
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
