package Tecnico

import java.io.{File, PrintWriter}

import scala.util.{Failure, Success, Try}


object Archivo {

  def crearArchivoOut(domiciliosList: List[String]) = {
    val pw = new PrintWriter(new File("/home/daniel/out.txt"))
    val stringBuilder = new StringBuilder
    stringBuilder.append("== Reporte de entregas ==\n")
    stringBuilder.append("\n")
    domiciliosList.map(x => stringBuilder.append(x + "\n"))

    pw.write(stringBuilder.toString)
    pw.close
  }

  def validarArchivo(domicilio: String) = {
    if (domicilio.filterNot(c => c.toUpper == 'A' || c.toUpper == 'I' || c.toUpper == 'D').isEmpty) Some() else None
  }

}
