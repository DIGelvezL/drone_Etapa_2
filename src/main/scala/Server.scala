import akka.actor.{Actor, OneForOneStrategy, Props}
import akka.actor.SupervisorStrategy._


import scala.collection.mutable


/**
  * Created by daniel on 24/02/17.
  */

class Server extends Actor {
  import Listas._

  override def supervisorStrategy = OneForOneStrategy() {
    case _: Exception => Resume
  }

  lazy val usuario = context.actorOf(Props(new Usuario), "usuario")

  def receive = {
    case MensajeEnviado(id, to, asunto, msg) => {
      usuario ! MensajeRecibido(to, id, asunto, msg)
      println("Se envío el correo a: " + to)
    }
    case ValidarCorreos(id, to) => {
      Server.existeCorreo(id).fold(l => Server.existeCorreo(to).fold(l2 => sender() ! "", r2 => sender() ! s"No existe el correo ${r2} al que se le va a enviar el mensaje"),
        r => sender() ! s"No existe el correo $r quien es el que envia el mensaje")
    }
    case ConsultarMail(m) =>{
      println(s"El usuario $m tiene ${listMailRec.filter(x => x.id == m).size} correos que son: ")
      sender() ! listMailRec.filter(x => x.id == m)
    }
    case CrearMail(mail) => {
      val valiMail = Server.validarCorreo(mail)
      valiMail.fold(l => sender() ! s"El usuario $l no es valido, no se puede crear!!",
        r => Server.existeCorreo(r).fold(l2 => sender() ! s"el usuario ${l2} ya existe!!", r2 => (Server.listCorreos += r2, sender() ! s"Se creó el usuario ${r2}!!")))
    }
    case ErrorEnviarMensaje(mail) => usuario ! ErrorEnviarMensaje(mail)
  }
}

object Server {
  val mailList = List("seven4n", "gmail", "hotmail", "yahoo")
  val listCorreos: mutable.MutableList[String] = mutable.MutableList()

  def validarCorreo(correo:String): Either[String, String] = {
    val co = correo.split("@")(1).split('.')(0)
    if(mailList.contains(co))
      Right(correo)
    else
      Left(correo)
  }

  def existeCorreo(correo:String): Either[String, String] = {
    if(Server.listCorreos.contains(correo))
      Left(correo)
    else
      Right(correo)
  }
}
