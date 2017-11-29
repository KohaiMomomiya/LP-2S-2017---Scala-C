package ParteScala

import PackArbolDosTres._

class Profesor(val nombre: String) extends Ordered[Profesor] with Profesional {
  val this.nombre: String = nombre
  this.ocupacion = "Profesor"

  val cursos: ArbolDosTres[Curso] = new ArbolDosTres[Curso]()

  // Ordenamiento por nombre.
  override def compare(that: Profesor): Int = this.nombre.compareTo(that.nombre)

  override def toString: String = s"Nombre: $nombre\nOcupación: $ocupacion\n"

  def getCursos: ArbolDosTres[Curso] = cursos
}