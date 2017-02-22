import akka.actor.{Actor, ActorSystem, Props}

import akka.actor.SupervisorStrategy._
import akka.actor.{Actor, AllForOneStrategy, Props}


/**
  * Created by daniel on 22/02/17.
  */

case class Mensaje(msg: String)
case class Number(num: Int)

class ActorSupervisado extends Actor {

  override def preRestart(reason: Throwable, message: Option[Any]) = {
    println("restarting...")
    super.preRestart(reason, message)
  }

  override def postRestart(reason: Throwable) = {
    println("...restart completed!")
    println("No se puede dividir por cero, cambia el número")
    super.postRestart(reason)
  }

  override def preStart() = println("Iniciando el actor supervisado")
  override def postStop() = println("Se detuvo el actor supervisado")

  def receive = {
    case num:Int => println(50 / num)
    case Mensaje(msg) => println(msg)
  }
}

class ActorSuperVisor extends Actor {
  override def preStart() = println("El supervisor está listo para supervisar!!")
  override def postStop() = println("Chao al supervisor!!")

  override def supervisorStrategy = AllForOneStrategy() {
    case _: ArithmeticException => Restart
  }

  val actorSupervisado = context.actorOf(Props[ActorSupervisado], "segundoActor")

  def receive = {
    case Number(n) => actorSupervisado ! n
    case Mensaje(msg) => println(msg)
  }
}

object SupervisorAkka2 extends App {

  val system = ActorSystem("printer-service")
  val actorSupervisor = system.actorOf(Props(new ActorSuperVisor), "printer-supervisor")
  actorSupervisor ! Number(0)

}
