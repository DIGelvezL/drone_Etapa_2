import scala.io.Source
import scala.util.Try

/**
  * Created by daniel on 27/02/17.
  */

object User extends App {

  val dron = new Dron()
  val res = Try {Source.fromFile("/home/daniel/in.txt").getLines.toList}.recover{case e: Exception => "El archivo está mal!!"}
  res.map(l => Dron.iniciar(l.asInstanceOf[List[String]]))

}

object Mensajes{

  def msgSuccess(msg:String) = {println(msg)}
  def msgError(msg:String) = {println(msg)}
}

