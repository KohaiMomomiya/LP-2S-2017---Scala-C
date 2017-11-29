package ParteScala

import PackArbolDosTres._

class ProfesorDoctorado(nombre: String) extends Profesor(nombre) {
  this.ocupacion += " (Ph.D)"

  override def compare(that: Profesor): Int = {
    if (that.isInstanceOf[ProfesorDoctorado]) {
      super.compare(that)
    } else {
      -1
    }
  }
}