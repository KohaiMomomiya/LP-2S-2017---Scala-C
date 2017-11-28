package ParteScala

class Profesor(val nombre: String) extends Ordered[Profesor] with Profesional {
  val this.nombre: String = nombre
  this.ocupacion = "Profesor"

  // Ordenamiento por nombre.
  override def compare(that: Profesor): Int = this.nombre.compareTo(that.nombre)

  override def toString: String = s"Nombre: $nombre\nOcupación: $ocupacion\n"
}