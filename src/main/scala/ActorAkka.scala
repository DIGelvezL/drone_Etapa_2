import akka.actor.{Actor, ActorRef, ActorSystem, Inbox, Props}
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.duration._
import akka.actor.Props


class PrimerActor extends Actor {
  import context._
  def angry: Receive = {
    case "foo" => sender() ! "I am already angry?"
    case "final" => {
      implicit val t = Timeout(5.second)
      val originalSender = sender()
      val fut = (actorOf(Props[SegundoActor], "segundoActor") ? WhoToGreet("Master"))
      fut.foreach(x => originalSender ! x)
    }
    case "bar" => become(happy)
  }

  def happy: Receive = {
    case "bar" => sender() ! "I am already happy :-)"
    case "foo" => unbecome()
  }

  def receive = {
    case "foo" => become(angry)
    case "bar" => become(happy)
  }
}

class SegundoActor extends Actor {
  var greeting = ""

  def receive = {
    case WhoToGreet(who) => sender() ! s"hello, $who"
    case Greet           => sender ! Greeting(greeting + " - " + "prueba") // Send the current greeting back to the sender
    case SaludoMaster    => sender ! Saludo(greeting + " - " + "Test")
  }
}

object ActorAkka extends App {

  val system = ActorSystem("actorAkka")

  val primerActor = system.actorOf(Props[PrimerActor], "primerActor") // se crea el actor!!

  primerActor ! "foo"
  primerActor ! "bar"
  primerActor ! "foo"
  primerActor ! "foo"

  implicit val t = Timeout(10.second)
  val msgAsk = primerActor ? "final"

  import scala.concurrent.ExecutionContext.Implicits.global
  msgAsk.map(println)

}
