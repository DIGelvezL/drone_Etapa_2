import akka.actor.{Actor, ActorSystem, AllForOneStrategy, OneForOneStrategy, Props}
import akka.actor.SupervisorStrategy._
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.duration._
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
  * Created by daniel on 23/02/17.
  */

case class MensajeEnviado(id:String, to:String, asunto:String, msg: String)
case class MensajeRecibido(id:String, from:String, asunto:String, msg: String)
case class ConsultarMail(mail:String)
case class NoExisteCorreo(mail:String)

object Listas {

  var listCorreos: mutable.MutableList[String] = mutable.MutableList()
  var listMailEnv: mutable.MutableList[MensajeEnviado] = mutable.MutableList()
  var listMailRec: mutable.MutableList[MensajeRecibido] = mutable.MutableList()

}

class Persis extends Actor {
  import Listas._

  override def preRestart(reason: Throwable, message: Option[Any]) = {
    println("restarting...")
    super.preRestart(reason, message)
  }

  override def postRestart(reason: Throwable) = {
    println("...restart completed!")
    super.postRestart(reason)
  }

  override def preStart() = println("Enviando el correo")
  override def postStop() = println("Se detuvo el envío del correo")

  def receive = {
    case MensajeEnviado(id, to, asunto, msg) => {
      if(listCorreos.contains(id)) {
        if(listCorreos.contains(to)) {
          listMailEnv += MensajeEnviado(id, to, asunto, msg)
          listMailRec += MensajeRecibido(to, id, asunto, msg)
          println(s"Correo enviado a: $to")
        }else{
          context.actorOf(Props(new Server), "server") ! NoExisteCorreo(to)
        }
      }
    }
    case ConsultarMail(m) =>{
      sender() ! listMailRec.filter(x => x.id == m)
    }
  }
}

class Server extends Actor {

  override def supervisorStrategy = OneForOneStrategy() {
    case _: Exception => Restart
  }

  val persis = context.actorOf(Props[Persis], "persis")

  def receive = {
    case MensajeEnviado(id, to, asunto, msg) => persis ! MensajeEnviado(id, to, asunto, msg)
    case ConsultarMail(m) =>{
      implicit val t = Timeout(5.second)
      val originalSender = sender()
      val fut = (persis ? ConsultarMail(m))
      fut.foreach(x => originalSender ! x)
    }
  }
}

class Emails extends Actor {

  def receive = {
    case correo: String => Listas.listCorreos += correo
  }
}

object EmailAkka extends App {
  import Listas._

  val system = ActorSystem("Email")
  val emails = system.actorOf(Props(new Emails), "emails")
  emails ! "daniel@seven4n.com"
  emails ! "ivan@seven4n.com"
  emails ! "gelvez@seven4n.com"

  println(listCorreos)

  val server = system.actorOf(Props(new Server), "server")

  server ! MensajeEnviado("daniel@seven4n.com", "ivan@seven4n.com", " probando ", " Este es un correo de prueba... xD")
  server ! MensajeEnviado("daniel@seven4n.com", "ivan@seven4n.com", " Examen medico ", " Debes hacer el examen medico el día 27 de febrero")
  server ! MensajeEnviado("ivan@seven4n.com", "gelvez@seven4n.com", "Curso scala", "Debes ir al curso de scala en Seven4n")

  Thread.sleep(5000)
  implicit val t = Timeout(10.second)
  val mails = server ? ConsultarMail("ivan@seven4n.com")
  mails.map(x => x.asInstanceOf[mutable.MutableList[MensajeRecibido]].map(y => println(s"Correo enviado por ${y.from} - asunto: ${y.asunto} - mensaje: ${y.msg}")))
  var aux = mails.map(x => x)


}
