package Tecnico

import akka.actor.{ActorSystem, Props}
import akka.util.Timeout
import scala.concurrent.duration._

object DomicilioByDrone extends App {

  val system = ActorSystem("DomicilioByDrone")
  val restaurant = system.actorOf(Props(new Restaurant), "restaurant")

  implicit val t = Timeout(10.second)

  restaurant ! UbicacionRutas("/home/daniel/in01.txt")
  restaurant ! UbicacionRutas("/home/daniel/in02.txt")
  restaurant ! UbicacionRutas("/home/daniel/in03.txt")

}
