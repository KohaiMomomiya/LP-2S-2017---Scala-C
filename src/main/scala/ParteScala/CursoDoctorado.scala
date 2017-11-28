package ParteScala

class CursoDoctorado(nombre: String) extends Curso(nombre) {
  this.descripcion = "Curso de Doctorado"

  def this(nombre: String, descripcion: String) = {
    this(nombre)
    this.descripcion = descripcion
  }

  // Ordenamiento por grado y nombre.
  override def compare(that: Curso): Int = {
    if (that.isInstanceOf[CursoDoctorado]) {
      super.compare(that)
    } else {
      -1
    }
  }
}
