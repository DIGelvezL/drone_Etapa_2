import akka.actor.{Actor, ActorSystem, AllForOneStrategy, OneForOneStrategy, Props}
import akka.actor.SupervisorStrategy._
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.duration._
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future


/**
  * Created by daniel on 24/02/17.
  */

class Server extends Actor {
  import Listas._
  var msjRespuesta = ""

  override def supervisorStrategy = OneForOneStrategy() {
    case _: Exception => Resume
  }

  lazy val usuario = context.actorOf(Props(new Usuario), "usuario")

  def receive = {
    case MensajeEnviado(id, to, asunto, msg) => {
      usuario ! MensajeRecibido(to, id, asunto, msg)
    }
    case ValidarCorreos(id, to) => {
      if(Server.listCorreos.contains(id)) {
        if(Server.listCorreos.contains(to)) {
          sender() ! msjRespuesta = ""
        }else{
          msjRespuesta = s"No existe el correo $to al que se le va a enviar el mensaje"
          sender() ! msjRespuesta
        }
      }else{
        msjRespuesta = s"No existe el correo $id quien es el que envia el mensaje"
        sender() ! msjRespuesta
      }
    }
    case ConsultarMail(m) =>{
      println(s"El usuario $m tiene ${listMailRec.filter(x => x.id == m).size} correos que son: ")
      sender() ! listMailRec.filter(x => x.id == m)
    }
    case CrearMail(mail) => {
      if(Server.validarCorreo(mail)){
        if(!Server.listCorreos.contains(mail)) {
          Server.listCorreos += mail
          msjRespuesta = s"Se creó el usuario $mail!!"
          sender() ! msjRespuesta
        }else{
          msjRespuesta = s"el usuario $mail ya existe!!"
          sender() ! msjRespuesta
        }
      }else{
        msjRespuesta = s"El usuario $mail no es valido, no se puede crear!!"
        sender() ! msjRespuesta
      }
    }
    case ErrorEnviarMensaje(mail) => usuario ! ErrorEnviarMensaje(mail)
  }
}

object Server {
  val mailList = List("seven4n", "gmail", "hotmail", "yahoo")
  val listCorreos: mutable.MutableList[String] = mutable.MutableList()

  def validarCorreo(correo:String): Boolean = {
    val co = correo.split("@")(1).split('.')(0)
    if(mailList.contains(co))
      true
    else
      false
  }
}
