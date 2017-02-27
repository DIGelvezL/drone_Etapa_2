import akka.actor.{ActorSystem, Props}
import akka.util.Timeout

import scala.concurrent.duration._

/**
  * Created by daniel on 23/02/17.
  */

case class MensajeEnviado(id:String, to:String, asunto:String, msg: String)
case class MensajeRecibido(id:String, from:String, asunto:String, msg: String)
case class ConsultarMail(mail:String)
case class ErrorEnviarMensaje(mail:String)
case class CrearMail(mail:String)
case class ValidarCorreos(id:String, to:String)

object EmailAkka extends App {

  val system = ActorSystem("Email")
  val usuario = system.actorOf(Props(new Usuario), "usuario")

  implicit val t = Timeout(10.second)

  usuario ! CrearMail("daniel@seven4n.com")
  usuario ! CrearMail("daniel@seven4n.com")
  usuario ! CrearMail("daniel@seven.com")
  usuario ! CrearMail("ivan@seven4n.com")
  usuario ! CrearMail("gelvez@seven4n.com")

  Thread.sleep(2000)
  usuario ! MensajeEnviado("daniel@seven4n.com", "ivan@seven4n.com", "probando", "Este es un correo de prueba... xD")
  usuario ! MensajeEnviado("gelvez@seven4n.com", "ivan@seven4n.com", "Juego de futbol", "Tenemos partido el sabdo a las 06:00")
  usuario ! MensajeEnviado("daniel@seven4n.com", "ivan@seven.com", "Examen medico", "Debes hacer el examen medico el día 27 de febrero")
  usuario ! MensajeEnviado("ivan@seven4n.com", "gelvez@seven4n.com", "Curso scala", "Debes ir al curso de scala en Seven4n")

  Thread.sleep(2000)
  usuario ! ConsultarMail("ivan@seven4n.com")

}
