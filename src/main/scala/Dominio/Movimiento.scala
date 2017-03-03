package Dominio

import scala.util.{Failure, Success, Try}

trait Movimiento
case object Avanzar        extends Movimiento
case object GirarDerecha   extends Movimiento
case object GirarIzquierda extends Movimiento
case object Detener extends Movimiento

object Movimiento {
  def apply(char: Char): Movimiento = {
    char match {
      case 'A' => Avanzar
      case 'D' => GirarDerecha
      case 'I' => GirarIzquierda
      case x => throw new IllegalArgumentException(s"No conozco la instruccion $x")
    }
  }
}


object MovimientosBuilder {

  def buildFromChar(char: Char): Try[Movimiento] = {
    char match {
      case 'A' => Success(Avanzar)
      case 'D' => Success(GirarDerecha)
      case 'I' => Success(GirarIzquierda)
      case x => Failure(new IllegalArgumentException(s"No conozco la instruccion $x"))
    }
  }

}
