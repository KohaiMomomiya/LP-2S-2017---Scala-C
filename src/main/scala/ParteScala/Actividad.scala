package ParteScala

trait Actividad {
  protected var descripcion: String = _

  def Descripcion: String = descripcion

  protected def Descripcion_(descripcion: String) = this.descripcion = descripcion
}
