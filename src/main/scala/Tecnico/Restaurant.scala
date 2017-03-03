package Tecnico

import Dominio.{Dron, ListaRutas}
import akka.actor.{Actor, Props}

import scala.io.Source
import scala.util.Try

case class UbicacionRutas(id:String)
case class MensajeSuccess(msg:String)
case class MensajeFailure(msg:String)

class Restaurant extends Actor {

  val restaurant = context.actorOf(Props[Restaurant], "mensajeRestaurant")
  val dron = context.actorOf(Props[Dron], "dron")
  def receive ={
    case UbicacionRutas(id) => {
      val res = Try {Source.fromFile(id).getLines.toList}.recover{case e: Exception => restaurant ! MensajeFailure("La ubicación del archivo está mal!!")}
      res.map(l => dron ! ListaRutas(l.asInstanceOf[List[String]]))
    }
    case MensajeSuccess(msg) => {
      println(msg)
    }
    case MensajeFailure(msg) => {
      println(msg)
    }
  }

}