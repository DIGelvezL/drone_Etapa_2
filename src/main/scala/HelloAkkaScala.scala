import akka.actor.{Actor, ActorRef, ActorSystem, Inbox, Props}
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.Future
import scala.concurrent.duration._

import scala.concurrent.ExecutionContext.Implicits.global


case object Greet
case object SaludoMaster
case class WhoToGreet(who: String)
case class Greeting(message: String)
case class Saludo(message: String)

class Greeter extends Actor {
  var greeting = ""

  def receive = {
    case WhoToGreet(who) => greeting = s"hello, $who"
    case Greet           => sender ! Greeting(greeting + " - " + "prueba") // Send the current greeting back to the sender
    case SaludoMaster    => sender ! Saludo(greeting + " - " + "Test")
  }
}

class HotSwapActor extends Actor {
  import context._
  def angry: Receive = {
    case "foo" => sender() ! "I am already angry?"
    case "final" => sender() ! "Recorrido final del become and unbecome"
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

object HelloAkkaScala extends App {

  // Create the 'helloakka' actor system
  val system: ActorSystem = ActorSystem("helloakka")

  // Create the 'greeter' actor
  val greeter: ActorRef = system.actorOf(Props[Greeter], "greeter") // se crea el actor!!

  // Create an "actor-in-a-box"
  val inbox: Inbox = Inbox.create(system)

  val hotSwap: ActorRef = system.actorOf(Props[HotSwapActor], "hotSwap") // se crea el actor!!

  hotSwap.tell("foo", ActorRef.noSender)
  inbox.send(hotSwap, "bar")
  inbox.send(hotSwap, "foo")
  inbox.send(hotSwap, "foo")
  inbox.send(hotSwap, "final")
  val message4 = inbox.receive(5.seconds)
  println(s"Greeting: $message4")

  // Tell the 'greeter' to change its 'greeting' message
  greeter.tell(WhoToGreet("akka"), ActorRef.noSender)

  // Ask the 'greeter for the latest 'greeting'
  // Reply should go to the "actor-in-a-box"
  inbox.send(greeter, Greet)

  // Wait 5 seconds for the reply with the 'greeting' message
  val Greeting(message1) = inbox.receive(5.seconds)
  println(s"Greeting: $message1")

  // Change the greeting and ask for it again
  greeter.tell(WhoToGreet("typesafe"), ActorRef.noSender)
  inbox.send(greeter, Greet)
  val Greeting(message2) = inbox.receive(5.seconds)
  println(s"Greeting: $message2")

  //prueba de Actor
  greeter.tell(WhoToGreet("Master"), ActorRef.noSender)
  inbox.send(greeter, SaludoMaster)
  val Saludo(message3) = inbox.receive(5.seconds)
  println(s"Greeting: $message3")

  val greetPrinter = system.actorOf(Props[GreetPrinter])
  // after zero seconds, send a Greet message every second to the greeter with a sender of the greetPrinter
  system.scheduler.schedule(0.seconds, 5.second, greeter, SaludoMaster)(system.dispatcher, greetPrinter)
  
}

// prints a greeting
class GreetPrinter extends Actor {
  def receive = {
    case Greeting(message) => println(message)
    case Saludo(message) => println(message)
  }
}