package Dominio


trait Acciones {

  def adelante(p: Posicion):Posicion = {
    p.direccion match {
      case Norte => Posicion(p.x, (p.y + 1), p.direccion)
      case Oriente => Posicion((p.x + 1), p.y, p.direccion)
      case Sur => Posicion(p.x, (p.y - 1), p.direccion)
      case Occidente => Posicion((p.x - 1), p.y, p.direccion)
    }
  }

  def derecha(p: Posicion):Posicion = {
    p.direccion match {
      case Norte => Posicion(p.x, p.y, Oriente)
      case Oriente => Posicion(p.x, p.y, Sur)
      case Sur => Posicion(p.x, p.y, Occidente)
      case Occidente => Posicion(p.x, p.y, Norte)
    }
  }

  def izquierda(p: Posicion):Posicion = {
    p.direccion match {
      case Norte => Posicion(p.x, p.y, Direccion("Occidente"))
      case Oriente => Posicion(p.x, p.y, Direccion("Norte"))
      case Sur => Posicion(p.x, p.y, Direccion("Oriente"))
      case Occidente => Posicion(p.x, p.y, Direccion("Sur"))
    }
  }

  def stop(limitePositivo: Int) = {
    val res = new Exception(s"Error: El Dron se paso de las $limitePositivo cuadras limite")
    //Mensajes.msgError(res.getMessage)
    throw res
  }
}