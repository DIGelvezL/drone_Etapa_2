import java.io.{File, PrintWriter}

import scala.util.Try

case class Posicion(x: Int, y: Int, direccion: Direccion)


class Dron {
  val limitePositivo = 10
  val limiteNegativo = -10
  val posicion = Posicion(0, 0, Direccion("Norte"))
}

object Dron {
  val dron = new Dron()

  def iniciar(domicilioList: List[List[String]] ) = {

    var posicionInicial = dron.posicion

    ValidarCantidadDomicilio.validar(domicilioList.flatten).fold(l1 => Mensajes.msgError(l1),
      r1 => {
        r1.map(s => println(s"Domicilio: $s"))
        !domicilioList.flatten.map(s => ValidarDomicilio.validar(s).fold(l => l, r => r)).contains(false);
        {
          val movimientosList = domicilioList.map(domicilio => domicilio.map(x => x.map(c => Movimientos(c.toUpper)))).flatten
          val posicionesList = movimientosList.map(domicilio =>
            domicilio.foldLeft(posicionInicial){(posicionAcumulada, Item) =>
              var posicionfinal = Domicilio.realizarDomicilios(Item, posicionAcumulada)
              posicionInicial = posicionfinal
              posicionfinal
            }
          )

          val entregaFinal = posicionesList.map(p => List(s"(${p.x}, ${p.y}) dirección ${p.direccion}")).flatten
          entregaFinal.map(println)
          Try {Archivo.crearArchivoOut(entregaFinal)}.recover { case e: Exception => Mensajes.msgError("La ubicación del archivo está mal!!") }
        }
      }
    )
  }
}

object ValidarCantidadDomicilio {
  def validar(direccionList: List[String]) = {
    if (direccionList.size <= 3) Right(direccionList) else Left("El Dron solo puede realizar 3 domicilios por archivo!!")
  }
}

object ValidarDomicilio {
  def validar(domicilio: String): Either[Boolean, Boolean] = {
    if (domicilio.filterNot(c => c.toUpper == 'A' || c.toUpper == 'I' || c.toUpper == 'D').isEmpty)
      Right(true)
    else {
      Mensajes.msgError(s"El domicilio $domicilio no es valido")
      Left(false)
    }
  }
}

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
}

object Domicilio extends Acciones {

  def realizarDomicilios(direccion: Movimientos, p: Posicion):Posicion = {

    direccion match {
        case Avanzar => Limite.limiteCuadras(p).fold(l => stop(), r => adelante(p))
        case GirarDerecha => derecha(p)
        case GirarIzquierda => izquierda(p)
      }

  }
}

object Limite {

  import Dron._

  def limiteCuadras(p: Posicion) = {
    if(p.x > dron.limitePositivo || p.x < dron.limiteNegativo ||
      p.y > dron.limitePositivo || p.y < dron.limiteNegativo) Left(false) else Right(true)
  }
}

