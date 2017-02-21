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
  import context._
  def angry: Receive = {
    case "foo" => sender() ! "I am already angry?"
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

object DispatcherAkka extends App {

  val system = ActorSystem("actorAkka")

  val primerActor = system.actorOf(Props[PrimerActor], "primerActor") // se crea el actor!!

  primerActor ! "foo"

  implicit val t = Timeout(10.second)
  val msgAsk = primerActor ? "foo"

  msgAsk.map(println)

  val actorDispatcher = system.actorOf(Props[PrimerActor].withDispatcher("ThreadPool-dispatcher"), "PrimerActorDispatcher")
  println(actorDispatcher)

}
