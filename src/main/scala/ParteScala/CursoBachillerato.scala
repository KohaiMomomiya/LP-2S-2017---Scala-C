package ParteScala

class CursoBachillerato(nombre: String) extends Curso(nombre) {
  this.descripcion = "Curso de Doctorado"

  def this(nombre: String, descripcion: String) = {
    this(nombre)
    this.descripcion = descripcion
  }


  override def clone(): Curso = {
    new CursoBachillerato(nombre)
  }


  // Ordenamiento por grado y nombre.
  override def compare(that: Curso): Int = {
    if (that.isInstanceOf[CursoPosgrado]) {
      -1
    } else {
      super.compare(that)
    }
  }
}