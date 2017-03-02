trait Direccion
case object Norte  extends Direccion
case object Oriente   extends Direccion
case object Sur  extends Direccion
case object Occidente    extends Direccion

object Direccion {
  def apply(char: String): Direccion = {
    char match {
      case "Norte" => Norte
      case "Oriente" => Oriente
      case "Sur" => Sur
      case "Occidente" => Occidente
      case _ => Norte
    }
  }
}
