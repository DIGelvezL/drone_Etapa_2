import java.io.{BufferedWriter, File, FileWriter, PrintWriter}

import scala.collection.mutable
import scala.util.Try

/**
  * Created by daniel on 28/02/17.
  */
/*case class Norte(n:String)
case class Oriente(or:String)
case class Sur(s:String)
case class Occidente(oc:String)
case class Coordenada(coo:Norte)
case class Coordenada(coo:Oriente)
case class Coordenada(coo:Sur)
case class Coordenada(coo:Occidente)*/

case class Coordenada(coo:String)
case class Posicion(x:Int, y:Int, direccion:Coordenada)

class Dron {
  val norte = "Norte"
  val oriente = "Oriente"
  val sur = "Sur"
  val occidente = "Occidente"

  var posicion = Posicion(0, 0, Coordenada(norte))

}

object Dron{
  val dron = new Dron()
  val listDomicilios: mutable.MutableList[String] = mutable.MutableList()

  def iniciar(direccionList:List[String])= {

    ValidarCantidadDomicilio.validar(direccionList).fold(l1 => Mensajes.msgError(l1),
      r1 => {
        r1.map(s => println(s"Domicilio: $s"))
        !direccionList.map(s => ValidarDomicilio.validar(s).fold(l => l, r => r)).contains(false); direccionList.map(s => Domicilio.realizarDomicilios(s))

        Try {Archivo.crearArchivoOut(listDomicilios)}.recover{case e: Exception => "La ubicación del archivo está mal está mal!!"}
      }
    )
  }
}

object ValidarCantidadDomicilio{
  def validar(direccionList:List[String]) = {
    if (direccionList.size <= 3) Right(direccionList) else Left("El Dron solo puede realizar 3 domicilios por archivo!!")
  }
}

object ValidarDomicilio{
  def validar(domicilio:String): Either[Boolean, Boolean] = {
    if(domicilio.filterNot(c => c.toUpper == 'A' || c.toUpper == 'I' || c.toUpper == 'D').isEmpty)
      Right(true)
    else {
      Mensajes.msgError(s"El domicilio $domicilio no es valido")
      Left(false)
    }
  }
}

object Archivo{

  val pw = new PrintWriter(new File("/home/daniel/out.txt"))

  def crearArchivoOut(domiciliosList:mutable.MutableList[String]) = {
    domiciliosList.map(x => pw.write(x + "\n"))
    pw.close
  }
}

object Domicilio extends Acciones{
  import Dron._

  def realizarDomicilios(direccion:String)= {

    direccion.map(l =>
      l.toUpper match {
        case 'A' => avanzar(dron.posicion.direccion)
        case 'D' => girarDerecha(dron.posicion.direccion)
        case 'I' => girarIzquierda(dron.posicion.direccion)
      }
    )

    listDomicilios += s"(${dron.posicion.x}, ${dron.posicion.y}) dirección ${dron.posicion.direccion.coo}"

    Mensajes.msgSuccess(s"(${dron.posicion.x}, ${dron.posicion.y}) dirección ${dron.posicion.direccion.coo}")
  }
}

trait Acciones {

  import Dron._

  def avanzar(direccion: Coordenada) = {
    direccion match {
      case Coordenada(dron.norte) => dron.posicion = Posicion(dron.posicion.x, (dron.posicion.y + 1), dron.posicion.direccion)
      case Coordenada(dron.oriente) => dron.posicion = Posicion((dron.posicion.x + 1), dron.posicion.y, dron.posicion.direccion)
      case Coordenada(dron.sur) => dron.posicion = Posicion(dron.posicion.x, (dron.posicion.y - 1), dron.posicion.direccion)
      case Coordenada(dron.occidente) => dron.posicion = Posicion((dron.posicion.x - 1), dron.posicion.y, dron.posicion.direccion)
    }
  }

  def girarDerecha(direccion: Coordenada) = {
    direccion match {
      case Coordenada(dron.norte) => dron.posicion = Posicion(dron.posicion.x, dron.posicion.y, Coordenada(dron.oriente))
      case Coordenada(dron.oriente) => dron.posicion = Posicion(dron.posicion.x, dron.posicion.y, Coordenada(dron.sur))
      case Coordenada(dron.sur) => dron.posicion = Posicion(dron.posicion.x, dron.posicion.y, Coordenada(dron.occidente))
      case Coordenada(dron.occidente) => dron.posicion = Posicion(dron.posicion.x, dron.posicion.y, Coordenada(dron.norte))
    }
  }

  def girarIzquierda(direccion: Coordenada) = {
    direccion match {
      case Coordenada(dron.norte) => dron.posicion = Posicion(dron.posicion.x, dron.posicion.y, Coordenada(dron.occidente))
      case Coordenada(dron.oriente) => dron.posicion = Posicion(dron.posicion.x, dron.posicion.y, Coordenada(dron.norte))
      case Coordenada(dron.sur) => dron.posicion = Posicion(dron.posicion.x, dron.posicion.y, Coordenada(dron.oriente))
      case Coordenada(dron.occidente) => dron.posicion = Posicion(dron.posicion.x, dron.posicion.y, Coordenada(dron.sur))
    }
  }
}