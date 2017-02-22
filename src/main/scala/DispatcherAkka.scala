import akka.actor.{Actor, ActorSystem, Props}
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.duration._
import akka.actor.Props

import scala.concurrent.ExecutionContext.Implicits.global

/**
  * Created by daniel on 21/02/17.
  */
class PrimerActorDispatcher extends Actor {

  def angry: Receive = {
    case "hijo" => {
      implicit val t = Timeout(5.second)
      val originalSender = sender()
      val actorDispatcherHijo = context.actorOf(Props[PrimerActorDispatcher].withDispatcher("akka.actor.Pinned-dedicated-dispatcher"), "PrimerActorDispatcherHijo")
      actorDispatcherHijo ! "bar"
      val fut = (actorDispatcherHijo ? "bar")
      fut.foreach(x => originalSender ! x)
    }
    case "foo" => sender() ! "I am already angry?"
    case "bar" => context.become(happy)
  }

  def happy: Receive = {
    case "bar" => sender() ! "I am already happy :-)"
    case "foo" => context.unbecome()
  }

  def receive = {
    case "foo" => context.become(angry)
    case "bar" => context.become(happy)
  }
}

object DispatcherAkka extends App {

  val system = ActorSystem("actorAkkaDispatcher")

  val actorDispatcher = system.actorOf(Props[PrimerActorDispatcher].withDispatcher("akka.actor.ThreadPool-dispatcher"), "PrimerActorDispatcher")

  actorDispatcher ! "foo"

  implicit val t = Timeout(10.second)
  val msgAsk = actorDispatcher ? "hijo"
  val msgAsk2 = actorDispatcher ? "foo"

  msgAsk.map(println)
  msgAsk2.map(println)

  val actorhijo = system.actorSelection("user/PrimerActorDispatcher/PrimerActorDispatcherHijo")
  val msgAskHijo = actorhijo ? "bar"

  msgAskHijo.map(x => println("Hijo: " + x))

}
