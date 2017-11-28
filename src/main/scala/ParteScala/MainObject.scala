package ParteScala

import PackArbolDosTres._

object MainObject {
  def main(args: Array[String]): Unit = {
    println("My first Scala project!")

    var arbol1: ArbolDosTres[Curso] = new ArbolDosTres[Curso]()
    arbol1.insertar(new Curso("Matemáticas"))
    arbol1.insertar(new Curso("Ciencias"))
    arbol1.insertar(new Curso("Humanidades"))
    arbol1.insertar(new Curso("Estudios Sociales"))
    arbol1.insertar(new Curso("Programación I"))

    println(arbol1.Tamaño)

    val iter1 = arbol1.iteradorPreorden()



    while(iter1.hasNext) {
      println(iter1.next())
    }
  }
}