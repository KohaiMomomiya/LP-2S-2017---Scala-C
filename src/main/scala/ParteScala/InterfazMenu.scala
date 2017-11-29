package ParteScala

import PackArbolDosTres._

class InterfazMenu {

  def menu(): Unit = { //Funcion que despliega el menu
    println("Bienvenido al Menú")
    println("Introduzca el número de la operación desea realizar")
    println("1. Insertar")
    println("2. Eliminar")
    println("3. Modificar")
    println("4. Listar")

    val opcion = readInt() //Lee el número que el usuario ingresa

    if (opcion == 1) {
      op1()
    }
    else
    if (opcion == 2){
      op2()
    }
    else
    if (opcion == 3){
      op3()
    }
    else
    if (opcion == 4){
      op4()
    }
    else{
      println("ERROR: El número ingresado es incorrecto, intentelo de nuevo")
      menu()
    }
  }

  def regresarMenu(): Unit = { // Funcion que se asegura de que el usuario quiera ir al menu
    println("Desea realizar otra operación? S/N")
    val op = readLine() //Lee el string que el usuario ingresa
    if (op == "S"){
      menu()
    }
    else
    if (op == "N"){
      println("Hasta luego!")
    }
    else{
      println("ERROR: Tiene que escribir S si desea seguir o N si no desea seguir")
      regresarMenu()
    }
  }

  def op1(): Unit = { //Funcion de la operacion insertar

    println("Introduzca el número de la operación que desea realizar")
    println("1. Insertar Curso")
    println("2. Insertar Curso de Posgrado")
    println("3. Insertar Profesor")
    println("4. Insertar Profesor de Posgrado")
    println("5. Regresar al Menú")

    val opInsert = readInt()

    if (opInsert == 1) {
      println("Introduzca el curso")
      val curs = readLine()
      new Curso(curs)
      //val cursos: ArbolDosTres[Curso] = new ArbolDosTres[Curso]()
      //val nuevoCurso = new Curso(curs)
      //cursos.insertar(nuevoCurso)
      //inserta al arbol
      op1()
    }
    else if (opInsert == 2) {
      println("Introduzca el curso de posgrado")
      val cursP = readLine()
      new CursoDoctorado(cursP)
      //falta insertarlo al arbol
      op1()
    }
    else if (opInsert == 3) {
      println("Introduzca el nombre del profesor")
      val nomProf = readLine()
      new Profesor(nomProf)
      //falta insertarlo al arbol
      op1()
    }
    else if (opInsert == 4) {
      println("Introduzca el nombre del profesor de Posgrado")
      val nomProfP = readLine()
      new ProfesorDoctorado(nomProfP)
      //falta insertarlo al arbol
      op1()
    }
    else if (opInsert == 5) {
      menu()
    }
    else{
      println("ERROR: Ingrese el número correcto de la operación que desea realizar")
      op1()
    }
  }

  def op2(): Unit = { //Función de la operación eliminar

    println("Introduzca el número de la operación que desea realizar")
    println("1. Eliminar Curso")
    println("2. Eliminar Curso de Posgrado")
    println("3. Eliminar Profesor")
    println("4. Eliminar Profesor de Posgrado")
    println("5. Regresar al Menú")

    val opDelete = readInt()

    if (opDelete == 1) {
      //Aquí deberían mostrarse los cursos que hay (podría ser una opción)
      println("Introduzca el curso que desea eliminar")
      val curs = readLine()
      //Nota: Aquí se hace la operación
      op2()
    }
    else if (opDelete == 2) {
      //Aquí deberían mostrarse los cursos que hay (podría ser una opción)
      println("Introduzca el curso de posgrado que desea eliminar")
      val cursP = readLine()
      //Nota: Aquí se hace la operación
      op2()
    }
    else if (opDelete == 3) {
      //Aquí deberían mostrarse los cursos que hay (podría ser una opción)
      println("Introduzca el nombre del profesor que desea eliminar")
      val nomProf = readLine()
      //Nota: Aquí se hace la operación
      op2()
    }
    else if (opDelete == 4) {
      //Aquí deberían mostrarse los cursos que hay (podría ser una opción)
      println("Introduzca el nombre del profesor de Posgrado que desea eliminar")
      val nomProfP = readLine()
      //Nota: Aquí se hace la operación
      op2()
    }
    else if (opDelete == 5) {
      menu()
    }
    else{
      println("ERROR: Ingrese el número correcto de la operación que desea realizar")
      op2()
    }
  }

  def op3(): Unit = { //Función de la operación modificar
    println("Introduzca el número de la operación que desea realizar")
    println("1. Modificar Curso")
    println("2. Modificar Curso de Posgrado")
    println("3. Modificar Profesor")
    println("4. Modificar Profesor de Posgrado")
    println("5. Regresar al Menú")

    val opModify = readInt()

    if (opModify == 1) {
      //Aquí deberían mostrarse los cursos que hay (podría ser una opción)
      println("Introduzca el curso que desea modificar")
      val curs = readLine()
      //Nota: Aquí se hace la operación
      op3()
    }
    else if (opModify == 2) {
      //Aquí deberían mostrarse los cursos que hay (podría ser una opción)
      println("Introduzca el curso de posgrado que desea modificar")
      val cursP = readLine()
      //Nota: Aquí se hace la operación
      op3()
    }
    else if (opModify == 3) {
      //Aquí deberían mostrarse los cursos que hay (podría ser una opción)
      println("Introduzca el nombre del profesor que desea modificar")
      val nomProf = readLine()
      //Nota: Aquí se hace la operación
      op3()
    }
    else if (opModify == 4) {
      //Aquí deberían mostrarse los cursos que hay (podría ser una opción)
      println("Introduzca el nombre del profesor de Posgrado que desea modificar")
      val nomProfP = readLine()
      //Nota: Aquí se hace la operación
      op3()
    }
    else if (opModify == 5) {
      menu()
    }
    else{
      println("ERROR: Ingrese el número correcto de la operación que desea realizar")
      op3()
    }
  }

  def op4(): Unit = { //Función de la operación modificar
    println("Introduzca el número de la operación que desea realizar")
    println("1. Listar Cursos")
    println("2. Listar Cursos de Posgrado")
    println("3. Listar Profesores")
    println("4. Listar Profesores de Posgrado")
    println("5. Regresar al Menú")

    val opList = readInt()

    if (opList == 1) {
      listCurso()
    }
    else if (opList == 2) {
      listCursoP()
    }
    else if (opList == 3) {
      listProf()
    }
    else if (opList == 4) {
      listProfPos()
    }
    else if (opList == 5) {
      menu()
    }
    else{
      println("ERROR: Ingrese el número correcto de la operación que desea realizar")
      op4()
    }
  }

  def listCurso() : Unit = {
    println("Introduzca el número del orden en que desea listar los cursos")
    println("1. InOrder")
    println("2. PreOrder")
    println("3. PostOrder")
    println("4. Regresar al Menú")

    val tipOpe = readInt()

    if (tipOpe == 1) {
      //Se realiza la operacion
    }
    else if (tipOpe == 2) {
      //Se realiza la operacion
    }
    else if (tipOpe == 3) {
      //Se realiza la operacion
    }
    else if (tipOpe == 4) {
      menu()
    }
    else{
      println("ERROR: Ingrese el número correcto de la operación que desea realizar")
      listCurso()
    }
  }

  def listCursoP() : Unit = {
    println("Introduzca el número del orden en que desea listar los cursos")
    println("1. InOrder")
    println("2. PreOrder")
    println("3. PostOrder")
    println("4. Regresar al Menú")

    val tipOpe = readInt()

    if (tipOpe == 1) {
      //Se realiza la operacion
    }
    else if (tipOpe == 2) {
      //Se realiza la operacion
    }
    else if (tipOpe == 3) {
      //Se realiza la operacion
    }
    else if (tipOpe == 4) {
      menu()
    }
    else{
      println("ERROR: Ingrese el número correcto de la operación que desea realizar")
      listCursoP()
    }
  }

  def listProf() : Unit = {
    println("Introduzca el número del orden en que desea listar los cursos")
    println("1. InOrder")
    println("2. PreOrder")
    println("3. PostOrder")
    println("4. Regresar al Menú")

    val tipOpe = readInt()

    if (tipOpe == 1) {
      //Se realiza la operacion
    }
    else if (tipOpe == 2) {
      //Se realiza la operacion
    }
    else if (tipOpe == 3) {
      //Se realiza la operacion
    }
    else if (tipOpe == 4) {
      menu()
    }
    else{
      println("ERROR: Ingrese el número correcto de la operación que desea realizar")
      listProf()
    }
  }

  def listProfPos(): Unit = {
    println("Introduzca el número del orden en que desea listar los cursos")
    println("1. InOrder")
    println("2. PreOrder")
    println("3. PostOrder")
    println("4. Regresar al Menú")

    val tipOpe = readInt()

    if (tipOpe == 1) {
      //Se realiza la operacion
    }
    else if (tipOpe == 2) {
      //Se realiza la operacion
    }
    else if (tipOpe == 3) {
      //Se realiza la operacion
    }
    else if (tipOpe == 4) {
      menu()
    }
    else{
      println("ERROR: Ingrese el número correcto de la operación que desea realizar")
      listProfPos()
    }
  }

}