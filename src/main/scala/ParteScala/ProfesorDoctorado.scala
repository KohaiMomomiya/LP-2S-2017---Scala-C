package ParteScala
import PackArbolDosTres._

class ProfesorDoctorado(nombre: String) extends Profesor(nombre) {
  this.ocupacion += " (Ph.D)"

  val cursosPosgrado: ArbolDosTres[CursoPosgrado] = new ArbolDosTres[CursoPosgrado]()

  def CursosPosgrado: ArbolDosTres[CursoPosgrado] = cursosPosgrado

  override def compare(that: Profesor): Int = {
    if (that.isInstanceOf[ProfesorDoctorado]) {
      super.compare(that)
    } else {
      -1
    }
  }
}