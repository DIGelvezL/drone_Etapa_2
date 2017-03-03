package Dominio


import Tecnico.{Archivo, MensajeFailure, MensajeSuccess, Restaurant}
import akka.actor.{Actor, Props}
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Try}

case class Posicion(x: Int, y: Int, direccion: Direccion)
case class ListaRutas(rutasList: List[String])
case class ListaPosiciones(lista: List[Posicion])
case class CambiarMovimiento(direccion: Movimiento, p: Posicion, limitePositivo: Int, limiteNegativo: Int)


class Dron extends Actor {
  val limitePositivo = 10
  val limiteNegativo = -10
  var posicion = Posicion(0, 0, Direccion("Norte"))

  lazy val restaurant = context.actorOf(Props[Restaurant], "mensajeRestaurant")
  lazy val dronActor = context.actorOf(Props[Dron], "dronActor")

  def receive ={
    case ListaRutas(rutasList) => {
      validarCatidadRutas(rutasList).fold(l1 => restaurant ! MensajeFailure(l1),
        r1 => {
          r1.map(s => println(s"Domicilio: $s"))
          if(!rutasList.map(s => {
            val rutaBn = Archivo.validarArchivo(s)
            if (rutaBn.isEmpty) restaurant ! MensajeFailure(s"El domicilio $s no es valido")
            rutaBn
          }).contains(None)){
            val domiciliosList = rutasList.map(x => x.map(c => Movimiento(c.toUpper)))
            val posicionesList = domiciliosList.map(domicilio =>
              domicilio.foldLeft(posicion){(posicionAcumulada, Item) =>
                val o = Controller.cambiarMovimiento(Item, posicionAcumulada, limitePositivo, limiteNegativo)
                if(o.isEmpty) {
                  val res = new Exception(s"Error: El Dron se paso de las $limitePositivo cuadras limite")
                  restaurant ! MensajeFailure(res.getMessage)
                  throw res
                }else o.map(x => posicion = x)
                posicion
              }
            )
            dronActor ! ListaPosiciones(posicionesList)
          }
        }
      )
    }
    case ListaPosiciones(lista) => {
      val entregaFinal = lista.map(p => List(s"(${p.x}, ${p.y}) dirección ${p.direccion}")).flatten
      entregaFinal.map(println)
      val res = Try {Archivo.crearArchivoOut(entregaFinal)}.recover { case e: Exception => restaurant ! MensajeFailure("La ubicación del archivo está mal!!") }
      if(res.isSuccess) restaurant ! MensajeSuccess("Se creó el archivo con exito!!")
    }
  }

  def validarCatidadRutas(direccionList: List[String]) = {
    if (direccionList.size <= 3) Right(direccionList) else Left("El Dron solo puede realizar 3 domicilios por archivo!!")
  }
}

object Controller extends Acciones {
  def cambiarMovimiento(direccion: Movimiento, p: Posicion, limitePositivo: Int, limiteNegativo: Int): Option[Posicion] = {
    direccion match {
      case Avanzar => {limiteCuadras(p, limitePositivo, limiteNegativo).fold(l => None, r => Some(adelante(p)))}
      case GirarDerecha => Some(derecha(p))
      case GirarIzquierda => Some(izquierda(p))
    }
  }

  def limiteCuadras(p: Posicion, lp: Int, ln: Int) = {
    if(p.x > lp || p.x < ln || p.y > lp || p.y < ln) Left(false) else Right(true)
  }
}