trait Movimientos
case object Avanzar        extends Movimientos
case object GirarDerecha   extends Movimientos
case object GirarIzquierda extends Movimientos
case object Detener extends Movimientos

object Movimientos {
  def apply(char: Char): Movimientos = {
    char match {
      case 'A' => Avanzar
      case 'D' => GirarDerecha
      case 'I' => GirarIzquierda
      case _ => Detener
    }
  }
}
