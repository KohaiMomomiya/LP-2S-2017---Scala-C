package ParteScala

import PackArbolDosTres._

import scala.io.StdIn

class InterfazMenu {
  val cursosBachillerato: ArbolDosTres[CursoBachillerato] = new ArbolDosTres[CursoBachillerato]()
  val cursosPosgrado: ArbolDosTres[CursoPosgrado] = new ArbolDosTres[CursoPosgrado]()
  val profesores: ArbolDosTres[Profesor] = new ArbolDosTres[Profesor]()
  val profesoresDoctorado: ArbolDosTres[ProfesorDoctorado] = new ArbolDosTres[ProfesorDoctorado]()


  def menu(): Unit = { //Funcion que despliega el menu
    splash()

    var mantenerAbierto = true

    while (mantenerAbierto) {
      println("Bienvenido al Menú")
      println("Introduzca el número de la operación desea realizar")
      println("1. Insertar")
      println("2. Eliminar")
      println("3. Modificar")
      println("4. Cerrar el programa")

      var opcion: Option[Int] = None

      try {
        val input = StdIn.readInt
        opcion = Option[Int](input)
      } catch {
        case e: Exception => mostrarErrorSeleccion()
      }

      if (opcion.isDefined) {
        val opcionRecibida = opcion.get
        opcionRecibida match {
          case 1 => op1()
          case 2 => op2()
          case 3 => op3()
          case 4 => mantenerAbierto = !confirmarSalida()
          case _ => mostrarErrorSeleccion()
        }
      }
    }
  }

  def splash(): Unit = {
    println("Instituto Tecnológico de Costa Rica\nSede Centro Académico San José\n")
    println("Escuela de Ingeniería en Computación\nCurso Lenguajes de Programción\n")
    println("Grupo 40 - Segundo Semestre, 2017\nProf. José Castro Mora\n")
    println("Proyecto 3 - Parte Scala")
    println("Elaborado por: Dafne Vargas, Emmanuel Pérez y Esteban Fonseca\n")
    println("=======================================================================")
  }

  // Función que verifica si el usuario desea terminar el programa.
  def confirmarSalida(): Boolean = {
    var seguirPreguntando = true
    var seleccionUsuario: Option[Boolean] = None

    while (seguirPreguntando) {
      println("¿Desea cerrar este programa? S/N")

      try {
        val input = StdIn.readChar()
        input match {
          case 'S' => seleccionUsuario = Option[Boolean](true); seguirPreguntando = false
          case 's' => seleccionUsuario = Option[Boolean](true); seguirPreguntando = false
          case 'N' => seleccionUsuario = Option[Boolean](false); seguirPreguntando = false
          case 'n' => seleccionUsuario = Option[Boolean](false); seguirPreguntando = false
          case _ => mostrarErrorSeleccion()
        }
      } catch {
        case e: Exception => mostrarErrorSeleccion()
      }
    }

    // Si no se pudo determinar la decisión del usuario, el programa se mantiene abierto.
    seleccionUsuario.getOrElse(false)
  }

  //Funciones de la operacion Insertar
  def op1(): Unit = {
    var cerrarMenu = false

    while (!cerrarMenu) {
      println("Introduzca el número de la operación que desea realizar")
      println("1. Insertar Curso de Bachillerato")
      println("2. Insertar curso de Posgrado")
      println("3. Insertar Profesor")
      println("4. Insertar Profesor de Posgrado")
      println("5. Regresar al Menú")

      var opcion: Option[Int] = None

      try {
        val input = StdIn.readInt
        opcion = Option[Int](input)
      } catch {
        case e: Exception => mostrarErrorSeleccion()
      }

      if (opcion.isDefined) {
        val opcionRecibida = opcion.get
        opcionRecibida match {
          case 1 => op1_curso()
          case 2 => op1_cursoPosgrado()
          case 3 => op1_profesor()
          case 4 => op1_profesorDoctorado()
          case 5 => cerrarMenu = true
          case _ => mostrarErrorSeleccion()
        }
      }
    }
  }

  private def op1_curso(): Unit = {
    println("Introduzca el curso:")
    val nombreCurso = StdIn.readLine
    val nuevoCurso = new CursoBachillerato(nombreCurso)

    val cursoInsertado: Boolean = cursosBachillerato.insertar(nuevoCurso)
    if (cursoInsertado) {
      println("El curso ha sido agregado.\n")
    } else {
      println("Ocurrió un error y el curso no pudo se agregado.\n")
    }
  }

  private def op1_cursoPosgrado(): Unit = {
    println("Introduzca el curso de posgrado:")
    val nombreCurso = StdIn.readLine
    val nuevoCurso = new CursoPosgrado(nombreCurso)

    val cursoInsertado: Boolean = cursosPosgrado.insertar(nuevoCurso)
    if (cursoInsertado) {
      println("El curso ha sido agregado.\n")
    } else {
      println("Ocurrió un error y el curso no pudo se agregado.\n")
    }
  }

  private def op1_profesor(): Unit = {
    println("Introduzca el nombre del profesor:")
    val nombreProfesor = StdIn.readLine
    val nuevoProfesor = new Profesor(nombreProfesor)

    val profInsertado: Boolean = profesores.insertar(nuevoProfesor)
    if (profInsertado) {
      println("El profesor ha sido registado.\n")
    } else {
      println("Ocurrió un error y el profesor no pudo ser agregado.\n")
    }
  }

  private def op1_profesorDoctorado(): Unit = {
    println("Introduzca el nombre del profesor con doctorado:")
    val nombreProfesor = StdIn.readLine
    val nuevoProfesor = new ProfesorDoctorado(nombreProfesor)

    val profInsertado: Boolean = profesoresDoctorado.insertar(nuevoProfesor)
    if (profInsertado) {
      println("El profesor ha sido registado.\n")
    } else {
      println("Ocurrió un error y el profesor no pudo ser agregado.\n")
    }
  }

  // Funciones de la operación Eliminar
  def op2(): Unit = { //Función de la operación eliminar
    var cerrarMenu = false

    while (!cerrarMenu) {
      println("Introduzca el número de la operación que desea realizar")
      println("1. Eliminar Curso de Bachillerato")
      println("2. Eliminar Curso de Posgrado")
      println("3. Eliminar Profesor")
      println("4. Eliminar Profesor de Posgrado")
      println("5. Regresar al Menú")

      var opcion: Option[Int] = None

      try {
        val input = StdIn.readInt
        opcion = Option[Int](input)
      } catch {
        case e: Exception => mostrarErrorSeleccion()
      }

      if (opcion.isDefined) {
        val opcionRecibida = opcion.get
        opcionRecibida match {
          case 1 => op2_cursoBachillerato()
          case 2 => op2_cursoPosgrado()
          case 3 => op2_profesor()
          case 4 => op2_profesorDoctorado()
          case 5 => cerrarMenu = true
          case _ => mostrarErrorSeleccion()
        }
      }
    }
  }

  private def op2_cursoBachillerato(): Unit = {
    println("=== Lista de Cursos de Bachillerato ===")
    listarCursosBachillerato()

    println("Introduzca el curso que desea eliminar")
    val nombreCurso = StdIn.readLine
    val cursoEliminar = new CursoBachillerato(nombreCurso)

    val cursoEliminado = cursosBachillerato.borrar(cursoEliminar)
    if (cursoEliminado) {
      println("El curso ha sido eliminado exitosamente.")
    } else {
      println("Hubo un error; el curso solicitado no pudo ser eliminado.")
    }
  }

  private def op2_cursoPosgrado(): Unit = {
    println("=== Lista de Cursos de Posgrado ===")
    listarCursosPosgrado()

    println("Introduzca el curso que desea eliminar")
    val nombreCurso = StdIn.readLine
    val cursoEliminar = new CursoPosgrado(nombreCurso)

    val cursoEliminado = cursosPosgrado.borrar(cursoEliminar)
    if (cursoEliminado) {
      println("El curso ha sido eliminado exitosamente.")
    } else {
      println("Hubo un error; el curso solicitado no pudo ser eliminado.")
    }
  }

  private def op2_profesor(): Unit = {
    println("=== Lista de Profesores ===")
    listarProfesores()

    println("Introduzca el nombre del profesor que desea eliminar")
    val nombreProf = StdIn.readLine
    val profEliminar = new Profesor(nombreProf)

    val profEliminado = profesores.borrar(profEliminar)
    if (profEliminado) {
      println("El profesor ha sido eliminado exitosamente.")
    } else {
      println("Hubo un error; el profesor solicitado no pudo ser eliminado.")
    }
  }

  private def op2_profesorDoctorado(): Unit = {
    println("=== Lista de Profesores con Doctorado===")
    listarProfesoresDoctorado()

    println("Introduzca el nombre del profesor que desea eliminar")
    val nombreProf = StdIn.readLine
    val profEliminar = new ProfesorDoctorado(nombreProf)

    val profEliminado = profesoresDoctorado.borrar(profEliminar)
    if (profEliminado) {
      println("El profesor ha sido eliminado exitosamente.")
    } else {
      println("Hubo un error; el profesor solicitado no pudo ser eliminado.")
    }
  }

  // Funciones de la operación Modificar
  def op3(): Unit = { //Función de la operación modificar
    var cerrarMenu = false

    while (!cerrarMenu) {
      println("Introduzca el número de la operación que desea realizar")
      println("1. Modificar Curso de Bachillerato")
      println("2. Modificar curso de Posgrado")
      println("3. Modificar Profesor")
      println("4. Modificar Profesor de Doctorado")
      println("5. Regresar al Menú")

      var opcion: Option[Int] = None

      try {
        val input = StdIn.readInt
        opcion = Option[Int](input)
      } catch {
        case e: Exception => mostrarErrorSeleccion()
      }

      if (opcion.isDefined) {
        val opcionRecibida = opcion.get
        opcionRecibida match {
          case 1 => op3_cursoBachillerato()
          case 2 => op3_cursoPosgrado()
          case 3 => op3_profesor()
          case 4 => op3_profesorDoctorado()
          case 5 => cerrarMenu = true
          case _ => mostrarErrorSeleccion()
        }
      }
    }
  }

  private def op3_cursoBachillerato(): Unit = {
    println("=== Lista de Cursos de Bachillerato ===")
    listarCursosBachillerato()

    println("Introduzca el curso que desea modificar")
    val nombreCurso = StdIn.readLine
    val cursoModificar = new CursoBachillerato(nombreCurso)

    val cursoEncontrado = cursosBachillerato.encontrar(cursoModificar)
    if (cursoEncontrado.isDefined) {
      println("Introduzca el nuevo nombre que desea agregar para este curso:")
      val nuevoNombre = StdIn.readLine
      val cursoModificado = new CursoBachillerato(nuevoNombre)

      val modificacion = cursosBachillerato.modificar(cursoEncontrado.get, cursoModificado)

      if (modificacion) {
        println("El curso ha sido modificado exitosamente.")
      } else {
        println("Hubo un error; el curso solicitado no pudo ser modificado.")
      }
    }
  }

  def listarCursosBachillerato(): Unit = {
    val iterCursos = cursosBachillerato.iteradorPreorden()
    while (iterCursos.hasNext) {
      println(iterCursos.next.nombre)
    }
    println("\n")
  }

  private def op3_cursoPosgrado(): Unit = {
    println("=== Lista de Cursos de Posgrado ===")
    listarCursosPosgrado()

    println("Introduzca el curso que desea modificar")
    val nombreCurso = StdIn.readLine
    val cursoModificar = new CursoPosgrado(nombreCurso)

    val cursoEncontrado = cursosPosgrado.encontrar(cursoModificar)
    if (cursoEncontrado.isDefined) {
      println("Introduzca el nuevo nombre que desea agregar para este curso:")
      val nuevoNombre = StdIn.readLine
      val cursoModificado = new CursoPosgrado(nuevoNombre)

      val modificacion = cursosPosgrado.modificar(cursoEncontrado.get, cursoModificado)

      if (modificacion) {
        println("El curso ha sido modificado exitosamente.")
      } else {
        println("Hubo un error; el curso solicitado no pudo ser modificado.")
      }
    }
  }

  def listarCursosPosgrado(): Unit = {
    val iterCursos = cursosPosgrado.iteradorPreorden()
    while (iterCursos.hasNext) {
      println(iterCursos.next.nombre)
    }
    println("\n")
  }

  private def op3_profesor(): Unit = {
    println("=== Lista de Profesores ===")
    listarProfesores()

    println("Introduzca el nombre del profesor que desea modificar")
    val nombreProfesor = StdIn.readLine
    val profesorModificar = new Profesor(nombreProfesor)

    val profesorEncontrado = profesores.encontrar(profesorModificar)
    if (profesorEncontrado.isDefined) {
      println("Introduzca el nuevo nombre que desea ingresar para este profesor:")
      val nuevoNombre = StdIn.readLine
      val profesorModificado = new Profesor(nuevoNombre)

      val modificacion = profesores.modificar(profesorEncontrado.get, profesorModificado)

      if (modificacion) {
        println("El profesor ha sido modificado exitosamente.")
      } else {
        println("Hubo un error; el curso solicitado no pudo ser modificado.")
      }
    }
  }

  def listarProfesores(): Unit = {
    val iterProf = profesores.iteradorPreorden()
    while (iterProf.hasNext) {
      println(iterProf.next.nombre)
    }
    println("\n")
  }

  private def op3_profesorDoctorado(): Unit = {
    println("=== Lista de Profesores con Doctorado ===")
    listarProfesoresDoctorado()

    println("Introduzca el nombre del profesor que desea modificar")
    val nombreProfesor = StdIn.readLine
    val profesorModificar = new ProfesorDoctorado(nombreProfesor)

    val profesorEncontrado = profesoresDoctorado.encontrar(profesorModificar)
    if (profesorEncontrado.isDefined) {
      println("Introduzca el nuevo nombre que desea ingresar para este profesor:")
      val nuevoNombre = StdIn.readLine
      val profesorModificado = new ProfesorDoctorado(nuevoNombre)

      val modificacion = profesoresDoctorado.modificar(profesorEncontrado.get, profesorModificado)

      if (modificacion) {
        println("El profesor ha sido modificado exitosamente.")
      } else {
        println("Hubo un error; el curso solicitado no pudo ser modificado.")
      }
    }
  }

  def listarProfesoresDoctorado(): Unit = {
    val iterProf = profesoresDoctorado.iteradorPreorden()
    while (iterProf.hasNext) {
      println(iterProf.next.nombre)
    }
    println("\n")
  }

  def mostrarErrorSeleccion(): Unit = {
    println("Error: Debe seleccionar una operación válida.")
  }

  // Funciones de la operación Listar
  def op4(): Unit = { //Función de la operación modificar
    var cerrarMenu = false

    while (!cerrarMenu) {
      println("Introduzca el número de la operación que desea realizar")
      println("1. Listar Cursos de Bachillerato")
      println("2. Listar Cursos de Doctorado")
      println("3. Listar Profesores")
      println("4. Listar Profesores con Doctorado")
      println("5. Regresar al Menú")

      var opcion: Option[Int] = None

      try {
        val input = StdIn.readInt
        opcion = Option[Int](input)
      } catch {
        case e: Exception => mostrarErrorSeleccion()
      }

      if (opcion.isDefined) {
        val opcionRecibida = opcion.get
        opcionRecibida match {
          case 1 => op4_cursosBachillerato()
          case 2 => op4_cursosPosgrado()
          case 3 => op4_profesores()
          case 4 => op4_profesoresDoctorado()
          case 5 => cerrarMenu = true
          case _ => mostrarErrorSeleccion()
        }
      }
    }
  }

  def op4_cursosBachillerato(): Unit = {
    var cerrarMenu = false

    while (!cerrarMenu) {
      println("Introduzca el número del orden en que desea listar los cursos")
      println("1. In-orden")
      println("2. Pre-orden")
      println("3. Post-orden")
      println("4. Regresar al Menú")

      var opcion: Option[Int] = None
      var iterCursos: Option[Iterator[Curso]] = None

      try {
        val input = StdIn.readInt
        opcion = Option[Int](input)
      } catch {
        case e: Exception => mostrarErrorSeleccion()
      }

      if (opcion.isDefined) {
        val opcionRecibida = opcion.get
        opcionRecibida match {
          case 1 => iterCursos = Option[Iterator[Curso]](cursosBachillerato.iteradorInorden())
          case 2 => iterCursos = Option[Iterator[Curso]](cursosBachillerato.iteradorPreorden())
          case 3 => iterCursos = Option[Iterator[Curso]](cursosBachillerato.iteradorPostorden())
          case 4 => cerrarMenu = true
          case _ => mostrarErrorSeleccion()
        }

        if (iterCursos.isDefined) {
          val iteradorExistente = iterCursos.get
          while (iteradorExistente.hasNext) {
            println(iteradorExistente.next.toString)
          }
        }
      }
    }
  }

  def op4_cursosPosgrado(): Unit = {
    var cerrarMenu = false

    while (!cerrarMenu) {
      println("Introduzca el número del orden en que desea listar los cursos")
      println("1. In-orden")
      println("2. Pre-orden")
      println("3. Post-orden")
      println("4. Regresar al Menú")

      var opcion: Option[Int] = None
      var iterCursos: Option[Iterator[CursoPosgrado]] = None

      try {
        val input = StdIn.readInt
        opcion = Option[Int](input)
      } catch {
        case e: Exception => mostrarErrorSeleccion()
      }

      if (opcion.isDefined) {
        val opcionRecibida = opcion.get
        opcionRecibida match {
          case 1 => iterCursos = Option[Iterator[CursoPosgrado]](cursosPosgrado.iteradorInorden())
          case 2 => iterCursos = Option[Iterator[CursoPosgrado]](cursosPosgrado.iteradorPreorden())
          case 3 => iterCursos = Option[Iterator[CursoPosgrado]](cursosPosgrado.iteradorPostorden())
          case 4 => cerrarMenu = true
          case _ => mostrarErrorSeleccion()
        }

        if (iterCursos.isDefined) {
          val iteradorExistente = iterCursos.get
          while (iteradorExistente.hasNext) {
            println(iteradorExistente.next.toString)
          }
        }
      }
    }
  }

  def op4_profesores(): Unit = {
    var cerrarMenu = false

    while (!cerrarMenu) {
      println("Introduzca el número del orden en que desea listar los profesores")
      println("1. In-orden")
      println("2. Pre-orden")
      println("3. Post-orden")
      println("4. Regresar al Menú")

      var opcion: Option[Int] = None
      var iterProfesores: Option[Iterator[Profesor]] = None

      try {
        val input = StdIn.readInt
        opcion = Option[Int](input)
      } catch {
        case e: Exception => mostrarErrorSeleccion()
      }

      if (opcion.isDefined) {
        val opcionRecibida = opcion.get
        opcionRecibida match {
          case 1 => iterProfesores = Option[Iterator[Profesor]](profesores.iteradorInorden())
          case 2 => iterProfesores = Option[Iterator[Profesor]](profesores.iteradorPreorden())
          case 3 => iterProfesores = Option[Iterator[Profesor]](profesores.iteradorPostorden())
          case 4 => cerrarMenu = true
          case _ => mostrarErrorSeleccion()
        }

        if (iterProfesores.isDefined) {
          val iteradorExistente = iterProfesores.get
          while (iteradorExistente.hasNext) {
            println(iteradorExistente.next.toString)
          }
        }
      }
    }
  }

  def op4_profesoresDoctorado(): Unit = {
    var cerrarMenu = false

    while (!cerrarMenu) {
      println("Introduzca el número del orden en que desea listar los profesores")
      println("1. In-orden")
      println("2. Pre-orden")
      println("3. Post-orden")
      println("4. Regresar al Menú")

      var opcion: Option[Int] = None
      var iterProfesores: Option[Iterator[ProfesorDoctorado]] = None

      try {
        val input = StdIn.readInt
        opcion = Option[Int](input)
      } catch {
        case e: Exception => mostrarErrorSeleccion()
      }

      if (opcion.isDefined) {
        val opcionRecibida = opcion.get
        opcionRecibida match {
          case 1 => iterProfesores =
            Option[Iterator[ProfesorDoctorado]](profesoresDoctorado.iteradorInorden())
          case 2 => iterProfesores =
            Option[Iterator[ProfesorDoctorado]](profesoresDoctorado.iteradorPreorden())
          case 3 => iterProfesores =
            Option[Iterator[ProfesorDoctorado]](profesoresDoctorado.iteradorPostorden())
          case 4 => cerrarMenu = true
          case _ => mostrarErrorSeleccion()
        }

        if (iterProfesores.isDefined) {
          val iteradorExistente = iterProfesores.get
          while (iteradorExistente.hasNext) {
            println(iteradorExistente.next.toString)
          }
        }
      }
    }
  }
}