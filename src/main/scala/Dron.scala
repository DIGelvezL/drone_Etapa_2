import java.io.{BufferedWriter, File, FileWriter, PrintWriter}

import scala.collection.mutable
import scala.util.Try

trait Direccion
case object Norte  extends Direccion
case object Oriente   extends Direccion
case object Sur  extends Direccion
case object Occidente    extends Direccion

object Direccion {
  def apply(char: String): Direccion = {
    char match {
      case "Norte" => Norte
      case "Oriente" => Oriente
      case "Sur" => Sur
      case "Occidente" => Occidente
      case _ => Norte
    }
  }
}

trait Movimientos
case object Avanzar        extends Movimientos
case object GirarDerecha   extends Movimientos
case object GirarIzquierda extends Movimientos
case object Detener extends Movimientos

object Movimientos {
  def apply(char: Char): Movimientos = {
    char match {
      case 'A' => Avanzar
      case 'D' => GirarDerecha
      case 'I' => GirarIzquierda
      case _ => Detener
    }
  }
}

case class Posicion(x: Int, y: Int, direccion: Direccion)


class Dron {
  val limitePositivo = 10
  val limiteNegativo = -10
  var posicion = Posicion(0, 0, Direccion("Norte"))

}

  object Dron {
    val dron = new Dron()
    val listDomicilios: mutable.MutableList[String] = mutable.MutableList()

    def iniciar(direccionList: List[String]) = {

      println(dron.posicion.direccion.toString)

      ValidarCantidadDomicilio.validar(direccionList).fold(l1 => Mensajes.msgError(l1),
        r1 => {
          r1.map(s => println(s"Domicilio: $s"))
          !direccionList.map(s => ValidarDomicilio.validar(s).fold(l => l, r => r)).contains(false);
          direccionList.map(s => Domicilio.realizarDomicilios(s))

          Try {
            Archivo.crearArchivoOut(listDomicilios)
          }.recover { case e: Exception => "La ubicación del archivo está mal está mal!!" }
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

    val pw = new PrintWriter(new File("/home/daniel/out.txt"))

    def crearArchivoOut(domiciliosList: mutable.MutableList[String]) = {
      domiciliosList.map(x => pw.write(x + "\n"))
      pw.close
    }
  }

  object Domicilio extends Acciones {

    import Dron._

    def realizarDomicilios(direccion: String) = {

      direccion.map(l => {

        Movimientos(l.toUpper) match {
          case Avanzar => Limite.limiteCuadras().fold(l => stop(), r => adelante(dron.posicion))
          case GirarDerecha => derecha(dron.posicion)
          case GirarIzquierda => izquierda(dron.posicion)
        }
      }
      )

      listDomicilios += s"(${dron.posicion.x}, ${dron.posicion.y}) dirección ${dron.posicion.direccion}"

      Mensajes.msgSuccess(s"(${dron.posicion.x}, ${dron.posicion.y}) dirección ${dron.posicion.direccion}")
    }
  }

object Limite {

  import Dron._

  def limiteCuadras() = {
    if(dron.posicion.x > dron.limitePositivo || dron.posicion.x < dron.limiteNegativo ||
      dron.posicion.y > dron.limitePositivo || dron.posicion.y < dron.limiteNegativo) Left(false) else Right(true)
  }
}

  trait Acciones {

    import Dron._

    def adelante(direccion: Posicion) = {
      direccion.direccion match {
        /*case Norte => dron.posicion.copy(y = (dron.posicion.y + 1))
        case Oriente => dron.posicion.copy(x = (dron.posicion.x + 1))
        case Sur => dron.posicion.copy(y = (dron.posicion.y - 1))
        case Occidente => dron.posicion.copy(x = (dron.posicion.x - 1))*/

        case Norte => dron.posicion = Posicion(dron.posicion.x, (dron.posicion.y + 1), dron.posicion.direccion)
        case Oriente => dron.posicion = Posicion((dron.posicion.x + 1), dron.posicion.y, dron.posicion.direccion)
        case Sur => dron.posicion = Posicion(dron.posicion.x, (dron.posicion.y - 1), dron.posicion.direccion)
        case Occidente => dron.posicion = Posicion((dron.posicion.x - 1), dron.posicion.y, dron.posicion.direccion)
      }

    }

    def derecha(direccion: Posicion) = {
      direccion.direccion match {
        case Norte => dron.posicion = Posicion(dron.posicion.x, dron.posicion.y, Oriente)
        case Oriente => dron.posicion = Posicion(dron.posicion.x, dron.posicion.y, Sur)
        case Sur => dron.posicion = Posicion(dron.posicion.x, dron.posicion.y, Occidente)
        case Occidente => dron.posicion = Posicion(dron.posicion.x, dron.posicion.y, Norte)
      }
    }

    def izquierda(direccion: Posicion) = {
      direccion.direccion match {
        case Norte => dron.posicion = Posicion(dron.posicion.x, dron.posicion.y, Direccion("Occidente"))
        case Oriente => dron.posicion = Posicion(dron.posicion.x, dron.posicion.y, Direccion("Norte"))
        case Sur => dron.posicion = Posicion(dron.posicion.x, dron.posicion.y, Direccion("Oriente"))
        case Occidente => dron.posicion = Posicion(dron.posicion.x, dron.posicion.y, Direccion("Sur"))
      }
    }

    def stop() = {
      val res = new Exception(s"Error: El Dron se paso de las ${dron.limitePositivo} cuadras limite")
      Mensajes.msgError(res.getMessage)
      throw res
    }
  }

