package ParteScala

class Curso(val nombre: String) extends Ordered[Curso] with Actividad {
  this.descripcion = "Curso"

  def this(nombre: String, descripcion: String) {
    this(nombre)
    this.descripcion = descripcion
  }

  // Ordenamiento por nombre.
  override def compare(that: Curso): Int = this.nombre.compareTo(that.nombre)

  override def clone(): Curso = {
    new Curso(nombre)
  }


  override def toString: String =
    s"Nombre del curso: $nombre\nDescripción del curso: $descripcion\n"
}

