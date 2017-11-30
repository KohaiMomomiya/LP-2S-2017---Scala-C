package ParteScala

import PackArbolDosTres._

class Profesor(val nombre: String) extends Ordered[Profesor] with Profesional {
  val this.nombre: String = nombre
  this.ocupacion = "Profesor"

  val cursosBachillerato: ArbolDosTres[CursoBachillerato] = new ArbolDosTres[CursoBachillerato]()

  def CursosBachillerato: ArbolDosTres[CursoBachillerato] = this.cursosBachillerato

  // Ordenamiento por nombre.
  override def compare(that: Profesor): Int = this.nombre.compareTo(that.nombre)

  override def toString: String = s"Nombre: $nombre\nOcupación: $ocupacion\n"
}