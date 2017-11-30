package PackArbolDosTres

import scala.collection.mutable.ListBuffer

class ArbolDosTres[T <: Ordered[T]](private var raiz: Option[NodoDosTres[T]]) {
  private var tamaño: Int = 0
  private var nuevoValorInsertado: Boolean = false


  def this() {
    this(None)
  }

  def Tamaño: Int = tamaño

  private def limpiarArbol(): Unit = {
    this.tamaño = 0
    this.raiz = None    // El GC borra los nodos.
  }

  override def clone(): ArbolDosTres[T] = {
    val clon = new ArbolDosTres[T](this.raiz)
    if (this.esVacio()) {
      clonarNodos(this.raiz, clon)
    }
    clon
  }

  def insertar(nuevoElemento: T): Boolean = {
    nuevoValorInsertado = false

    if (raiz.isEmpty || raiz.get.raizIzquierda.isEmpty) {
      if (raiz.isEmpty) {
        val nuevaRaiz: NodoDosTres[T] = new NodoDosTres[T](nuevoElemento)
        raiz = Option[NodoDosTres[T]](nuevaRaiz)
      } else {
        this.raiz.get.raizIzquierda = Option[T](nuevoElemento)
      }
      tamaño += 1
      nuevoValorInsertado = true
    }
    else {
      val nuevaRaiz: Option[NodoDosTres[T]] = insertarEnArbol(raiz, nuevoElemento)
      if (nuevaRaiz.isDefined) {
        raiz = nuevaRaiz
      }
      if (nuevoValorInsertado) {
        tamaño += 1
      }
    }

    nuevoValorInsertado
  }

  def insertarLista(listaElementos: List[T]): Unit = {
    for (elemento <- listaElementos){
      insertar(elemento)
      if (!nuevoValorInsertado) {
        println(s"No se pudo insertar el valor: ${elemento.toString} en el árbol.\n")
      }
    }
  }

  def insertarEnArbol(nodoActual: Option[NodoDosTres[T]],
                      nuevoElemento: T): Option[NodoDosTres[T]] = {
    var nuevoNodoRaiz: Option[NodoDosTres[T]] = None

    // No se ha llegado a un nodo hoja.
    if (!nodoActual.get.esHoja) {
      // El nuevo dato ya se encuentra registrado.
      if (nodoActual.get.raizIzquierda.get == nuevoElemento ||
          (nodoActual.get.esNodo3 && nodoActual.get.raizDerecha.get == nuevoElemento)) {
        println("El dato ingresado ya se encuentra registrado.")
        return None
      }

      // El nuevo elemento debe ir antes que la raíz izquierda del nodo actual.
      else if (nuevoElemento < nodoActual.get.raizIzquierda.get) {
        nuevoNodoRaiz = insertar_subramaIzquierda(nodoActual.get, nuevoElemento)
      }

      // El nuevo elemento debe ir entre las raíces izquierda y derecha del nodo actual.
      else if (nodoActual.get.esNodo2 ||
        (nodoActual.get.esNodo3 && nodoActual.get.raizDerecha.get > nuevoElemento)) {
        nuevoNodoRaiz = insertar_subramaCentro(nodoActual.get, nuevoElemento)
      }

      // El nuevo elemento debe ir después que la raíz derecha del nodo actual.
      else if (nodoActual.get.esNodo3 || (nodoActual.get.raizDerecha.get < nuevoElemento)) {
        nuevoNodoRaiz = insertar_subramaDerecha(nodoActual.get, nuevoElemento)
      }
    }
      // El nodo actual es una hoja.
    else {
      nuevoNodoRaiz = insertar_NodoHoja(nodoActual.get, nuevoElemento)
    }
    nuevoNodoRaiz
  }

  def modificar(elementoAnterior: T, elementoNuevo: T): Boolean = {
    if (contiene(elementoAnterior)) {
      borrar(elementoAnterior)
      insertar(elementoNuevo)
      return true
    }
    false
  }

  def contiene(elementoBuscado: T): Boolean = {
    val encontrado: Option[T] = encontrar(elementoBuscado)
    encontrado.isDefined
  }

  def encontrar(elementoBuscado: T): Option[T] = {
    encontrarEnArbol(raiz, elementoBuscado)
  }

  // Elimina un elemento del árbol.
  // Se retorna un Boolean indicando si el nodo pudo ser borrado exitosamente.
  def borrar(elemento: T): Boolean = {
    var elementoBorrado: Boolean = borrarEnArbol(raiz, elemento)
    raiz.get.balancear() // Rebalancear raíz de árbol.

    if (elementoBorrado) {
      if (raiz.get.raizIzquierda.isEmpty) {
        raiz = None
      }
      tamaño -= 1
    }

    elementoBorrado
  }

  // Iterador pre-orden
  def iteradorPreorden(): Iterator[T] = {
    generarListaPreorden().iterator
  }

  // Genera una lista pre-orden de los elementos del árbol.
  def generarListaPreorden(): List[T] = {
    if (this.esVacio()) {
      println("No contiene elementos.")
      List[T]()
    } else {
      val bufferPreorden: ListBuffer[T] = ListBuffer[T]()
      recorrerPreorden(this.raiz, bufferPreorden)
      bufferPreorden.toList
    }
  }

  // Iterador in-orden
  def iteradorInorden(): Iterator[T] = {
    generarListaInorden().iterator
  }

  // Genera una lista in-orden de los elementos del árbol.
  def generarListaInorden(): List[T] = {
    if (this.esVacio()) {
      println("No contiene elementos.")
      List[T]()
    } else {
      val bufferInorden: ListBuffer[T] = ListBuffer[T]()
      recorrerInorden(this.raiz, bufferInorden)
      bufferInorden.toList
    }
  }

  // Iterador post-orden
  def iteradorPostorden(): Iterator[T] = {
    generarListaPostorden().iterator
  }

  // Genera una lista post-orden de los elementos del árbol.
  def generarListaPostorden(): List[T] = {
    if (this.esVacio()) {
      println("No contiene elementos.")
      List[T]()
    } else {
      val bufferPostorden: ListBuffer[T] = ListBuffer[T]()
      recorrerPostorden(this.raiz, bufferPostorden)
      bufferPostorden.toList
    }
  }

  def esVacio(): Boolean = {
    raiz.isEmpty || raiz.get.raizIzquierda.isEmpty
  }

  private def clonarNodos(nodoActual: Option[NodoDosTres[T]], clon: ArbolDosTres[T]): Unit = {
    if (nodoActual.isDefined) {
      if (nodoActual.get.esHoja) {
        clon.insertar(nodoActual.get.raizIzquierda.get)
        if (nodoActual.get.raizDerecha.isDefined) {
          clon.insertar(nodoActual.get.raizDerecha.get)
        }
      } else {
        clonarNodos(nodoActual.get.subramaIzquierda, clon)
        clon.insertar(nodoActual.get.raizIzquierda.get)

        clonarNodos(nodoActual.get.subramaCentro, clon)

        if (nodoActual.get.raizDerecha.isDefined) {
          if (!nodoActual.get.esHoja) {
            clon.insertar(nodoActual.get.raizDerecha.get)
          }
          clonarNodos(nodoActual.get.subramaDerecha, clon)
        }
      }
    }
  }

  // FIXME: Error al insertar en sub-árbol izquierdo.
  private def insertar_subramaIzquierda
  (nodoActual: NodoDosTres[T], nuevoElemento: T): Option[NodoDosTres[T]] = {

    val nodoElevado: Option[NodoDosTres[T]] =
      insertarEnArbol(nodoActual.subramaIzquierda, nuevoElemento)

    if (nodoElevado.isDefined) {
      val nodoElevado_ = nodoElevado.get

      if (nodoActual.esNodo2) {
        nodoActual.raizDerecha = nodoActual.raizIzquierda
        nodoActual.raizIzquierda = nodoElevado_.raizIzquierda

        nodoActual.subramaDerecha = nodoActual.subramaCentro
        nodoActual.subramaCentro = nodoElevado_.subramaCentro
        nodoActual.subramaIzquierda = nodoElevado_.subramaIzquierda
      } else {
        val copiaRamaDerecha = Option[NodoDosTres[T]](new NodoDosTres[T]
        (nodoActual.raizDerecha.get, None, nodoActual.subramaCentro, nodoActual.subramaDerecha))

        Option[NodoDosTres[T]](
          new NodoDosTres[T](nodoActual.raizIzquierda.get, None, nodoElevado, copiaRamaDerecha))
      }
    }
    None
  }

  private def insertar_subramaCentro
  (nodoActual: NodoDosTres[T], nuevoElemento: T): Option[NodoDosTres[T]] = {
    val nodoElevado: Option[NodoDosTres[T]] =
      insertarEnArbol(nodoActual.subramaCentro, nuevoElemento)

    if (nodoElevado.isDefined) {
      val nodoElevado_ = nodoElevado.get

      if (nodoActual.esNodo2) {
        nodoActual.raizDerecha = nodoElevado_.raizIzquierda
        nodoActual.subramaDerecha = nodoElevado_.subramaCentro
        nodoActual.subramaCentro = nodoActual.subramaIzquierda
      } else {
        val nuevoNodoIzq = Option[NodoDosTres[T]](new NodoDosTres[T]
        (nodoActual.raizIzquierda.get, None,
          nodoActual.subramaIzquierda, nodoElevado_.subramaIzquierda))
        val nuevoNodoDer = Option[NodoDosTres[T]](new NodoDosTres[T]
        (nodoActual.raizDerecha.get, None,
          nodoElevado_.subramaCentro, nodoActual.subramaDerecha))
        return Option[NodoDosTres[T]](
          new NodoDosTres[T](nodoElevado_.raizIzquierda.get, None,
            nodoActual.subramaIzquierda, nodoActual.subramaCentro))
      }
    }
    None
  }

  private def insertar_subramaDerecha
  (nodoActual: NodoDosTres[T], nuevoElemento: T): Option[NodoDosTres[T]] = {
    val nodoElevado: Option[NodoDosTres[T]] =
      insertarEnArbol(nodoActual.subramaDerecha, nuevoElemento)

    if (nodoElevado.isDefined) {
      val nodoElevado_ = nodoElevado.get

      val copiaRamaIzquierda = Option[NodoDosTres[T]](new NodoDosTres[T]
      (nodoActual.raizIzquierda.get, None,
        nodoActual.subramaIzquierda, nodoActual.subramaCentro))

      return Option[NodoDosTres[T]](
        new NodoDosTres[T](nodoActual.raizDerecha.get, None, copiaRamaIzquierda, nodoElevado))
    }
    None
  }

  private def insertar_NodoHoja
  (nodoActual: NodoDosTres[T], nuevoElemento: T): Option[NodoDosTres[T]] = {
    nuevoValorInsertado = true

    if ((nodoActual.raizIzquierda.get == nuevoElemento) ||
        (nodoActual.esNodo3 && nodoActual.raizDerecha.get == nuevoElemento)) {
      nuevoValorInsertado = false
      Option[NodoDosTres[T]](nodoActual)
    }
    // El nodo actual sólo tiene la raíz izquierda.
    else if (nodoActual.esNodo2) {
      if (nodoActual.raizIzquierda.get > nuevoElemento) {
        nodoActual.raizDerecha = nodoActual.raizIzquierda
        nodoActual.raizIzquierda = Option[T](nuevoElemento)
      } else if (nodoActual.raizIzquierda.get < nuevoElemento) {
        nodoActual.raizDerecha = Option[T](nuevoElemento)
      }
      Option[NodoDosTres[T]](nodoActual)
    }
    // El nodo actual ya tiene dos raíces. Se debe dividirNodo el nodo actual.
    else {
      dividirNodo(nodoActual, nuevoElemento)
    }
  }

  private def dividirNodo(nodoActual: NodoDosTres[T], nuevoElemento: T): Option[NodoDosTres[T]] = {

    // El nuevo elemento debe ir antes que la raíz izquierda del nodo actual.
    if (nodoActual.raizIzquierda.get > nuevoElemento) {
      val nuevaSubramaDerecha = Option[NodoDosTres[T]](new NodoDosTres[T](nuevoElemento))
      val nuevaSubramaIzquierda =
        Option[NodoDosTres[T]](new NodoDosTres[T](nodoActual.raizDerecha.get))

      return Option[NodoDosTres[T]](new NodoDosTres[T](nodoActual.raizIzquierda.get, None,
          nuevaSubramaIzquierda, nuevaSubramaDerecha))
    }

    // El nuevo elemento debe ir después que la raíz izquierda del nodo actual.
    else if (nodoActual.raizIzquierda.get < nuevoElemento) {

      // El nuevo elemento debe ir entre la raíz izquierda y derecha. El nuevo elemento es elevado.
      if (nodoActual.raizDerecha.get > nuevoElemento) {
        val nuevaSubramaIzq = Option[NodoDosTres[T]](new NodoDosTres[T](nodoActual.raizIzquierda))
        val nuevaSubramaDer = Option[NodoDosTres[T]](new NodoDosTres[T](nodoActual.raizDerecha))

        return Option[NodoDosTres[T]](
          new NodoDosTres[T](nuevoElemento, None, nuevaSubramaIzq, nuevaSubramaDer))
      }

      // El nuevo elemento debe ir después que la raíz derecha. La raíz derecha es elevada.
      else {
        val nuevaSubramaIzq = Option[NodoDosTres[T]](new NodoDosTres[T](nodoActual.raizIzquierda))
        val nuevaSubramaDer = Option[NodoDosTres[T]](new NodoDosTres[T](nuevoElemento))

        return Option[NodoDosTres[T]](
          new NodoDosTres[T](nodoActual.raizDerecha.get, None, nuevaSubramaIzq, nuevaSubramaDer)
        )
      }
    }

    None
  }

  private def encontrarEnArbol
  (nodoActual: Option[NodoDosTres[T]], elementoBuscado: T): Option[T] = {
    if (nodoActual.isDefined) {
      val nodoActual_ = nodoActual.get

      if (nodoActual_.raizIzquierda.isDefined &&
          (nodoActual_.raizIzquierda.get == elementoBuscado)) {
        return nodoActual.get.raizIzquierda
      } else {
        if (nodoActual_.raizDerecha.isDefined && (nodoActual_.raizDerecha.get == elementoBuscado)) {
          return nodoActual_.raizDerecha
        } else {
          if (nodoActual_.raizIzquierda.get > elementoBuscado) {
            return encontrarEnArbol(nodoActual_.subramaIzquierda, elementoBuscado)
          }
          else if (nodoActual_.subramaDerecha.isEmpty &&
            (nodoActual_.raizDerecha.get > elementoBuscado)) {
            return encontrarEnArbol(nodoActual_.subramaCentro, elementoBuscado)
          }
          else if (nodoActual_.raizDerecha.get < elementoBuscado) {
            return encontrarEnArbol(nodoActual_.subramaDerecha, elementoBuscado)
          }
          else
            return None
        }
      }
    }

    None
  }

  private def borrarEnArbol(nodoActual: Option[NodoDosTres[T]], elemento: T): Boolean = {
    var fueBorrado: Boolean = true

    // Se ha llegado a un nodo inexistente, por tanto no se encontró el elemento.
    if (nodoActual.isEmpty) {
      false
    }

    // Se busca y elimina el elemento a borrar.
    else {
      // Elemento a borrar no corresponde a raíz izquierda del nodo actual.
      // Se busca recursivamente en la raíz derecha o los subárboles.
      if (nodoActual.get.raizIzquierda.get != elemento) {

        // Nodo actual es Nodo 2 o elemento a borrar está antes de la rama derecha.
        if (nodoActual.get.raizDerecha.isEmpty || nodoActual.get.raizDerecha.get > elemento) {

          // Elemento a borrar está dentro de la rama izquierda.
          if (nodoActual.get.raizIzquierda.get > elemento) {
            fueBorrado = borrarEnArbol(nodoActual.get.subramaIzquierda, elemento)
          }
          // Elemento a borrar está en la rama central.
          else {
            fueBorrado = borrarEnArbol(nodoActual.get.subramaCentro, elemento)
          }
        }
        // Elemento a borrar está después de la rama central.
        else {
          // Elemento a borrar está dentro de la rama derecha.
          if (nodoActual.get.raizDerecha.get != elemento) {
            fueBorrado = borrarEnArbol(nodoActual.get.subramaDerecha, elemento)
          }
          // Elemento a borrar es la raíz derecha.
          else {
            if (nodoActual.get.esHoja) {
              nodoActual.get.raizDerecha = None
            } else {
              val reemplazo = nodoActual.get.subramaDerecha.get.cambiarMinimo()
              nodoActual.get.raizIzquierda = reemplazo
            }
          }
        }
      }
      // Elemento a borrar corresponde a raíz izquierda del nodo actual.
      else {
        if (nodoActual.get.esHoja) {
          if (nodoActual.get.raizDerecha.isDefined) {
            nodoActual.get.raizIzquierda = nodoActual.get.raizDerecha
            nodoActual.get.raizDerecha = None
          } else {
            nodoActual.get.raizIzquierda = None
            true   // Se debe rebalancear el árbol.
          }
        } else {
          val maximoSubramaIzq = nodoActual.get.subramaIzquierda.get.cambiarMaximo()
          nodoActual.get.raizIzquierda = maximoSubramaIzq
        }
      }
    }

    // Luego de buscar y borrar el elemento del árbol, se rebalancea el sub-árbol cuya raíz es el
    // nodo actual.
    rebalancearSubArbol(nodoActual)
    fueBorrado
  }

  // Sub-rutina para balancear sub-árbol cuya raíz es el nodo actual.
  private def rebalancearSubArbol(nodoActual: Option[NodoDosTres[T]]): Unit = {
    if (nodoActual.isDefined && !nodoActual.get.estaBalanceado()) {
      nodoActual.get.balancear()
    } else if (nodoActual.isDefined && !nodoActual.get.esHoja) {
      var balanceado: Boolean = false

      // Se balanceará tantas veces como sea necesario.
      while (!balanceado) {
        if (nodoActual.get.subramaDerecha.isEmpty) {
          if (nodoActual.get.subramaIzquierda.get.esHoja &&
            !nodoActual.get.subramaCentro.get.esHoja){
            val reemplazo = nodoActual.get.subramaCentro.get.cambiarMinimo()
            val elementoReinsercion = nodoActual.get.raizIzquierda.get

            nodoActual.get.raizIzquierda = reemplazo
            insertar(elementoReinsercion)
          }
          else if
          (!nodoActual.get.subramaIzquierda.get.esHoja &&
              nodoActual.get.subramaCentro.get.esHoja) {
            if (nodoActual.get.raizDerecha.isEmpty) {
              val reemplazo = nodoActual.get.subramaIzquierda.get.cambiarMaximo()
              val elementoReinsercion = nodoActual.get.raizIzquierda.get

              nodoActual.get.raizIzquierda = reemplazo
              insertar(elementoReinsercion)
            }
          }
        }
        if (nodoActual.get.subramaDerecha.isDefined) {
          if (nodoActual.get.subramaCentro.get.esHoja &&
            !nodoActual.get.subramaDerecha.get.esHoja) {
            nodoActual.get.subramaDerecha.get.balancear()
          }
          if (nodoActual.get.subramaCentro.get.esHoja &&
            !nodoActual.get.subramaDerecha.get.esHoja) {
            val reemplazo = nodoActual.get.subramaDerecha.get.cambiarMinimo()
            val elementoReinsercion = nodoActual.get.raizDerecha.get

            nodoActual.get.raizDerecha = reemplazo
            insertar(elementoReinsercion)
          } else {
            balanceado = true
          }
        }
        if (nodoActual.get.estaBalanceado()) {
          balanceado = true
        }
      }
    }
  }

  // Recorre el árbol en pre-orden y agrega los elementos al búfer.
  private def recorrerPreorden
  (nodoActual: Option[NodoDosTres[T]], bufferPreorden: ListBuffer[T]): Unit = {
    if (nodoActual.isDefined) {
      bufferPreorden.append(nodoActual.get.raizIzquierda.get)

      recorrerPreorden(nodoActual.get.subramaIzquierda, bufferPreorden)
      recorrerPreorden(nodoActual.get.subramaCentro, bufferPreorden)

      if (nodoActual.get.raizDerecha.isDefined) {
        bufferPreorden.append(nodoActual.get.raizDerecha.get)

        recorrerPreorden(nodoActual.get.subramaDerecha, bufferPreorden)
      }
    }
  }

  // Recorre el árbol en in-orden y agrega los elementos al búfer.
  private def recorrerInorden
  (nodoActual: Option[NodoDosTres[T]], bufferInorden: ListBuffer[T]): Unit = {
    if (nodoActual.isDefined) {
      if (nodoActual.get.esHoja) {
        bufferInorden.append(nodoActual.get.raizIzquierda.get)

        if (nodoActual.get.raizDerecha.isDefined) {
          bufferInorden.append(nodoActual.get.raizDerecha.get)
        }
      }
      else {
        recorrerInorden(nodoActual.get.subramaIzquierda, bufferInorden)

        bufferInorden.append(nodoActual.get.raizIzquierda.get)

        recorrerInorden(nodoActual.get.subramaCentro, bufferInorden)

        if (nodoActual.get.raizDerecha.isDefined) {
          if (!nodoActual.get.esHoja) {
            bufferInorden.append(nodoActual.get.raizDerecha.get)
          }

          recorrerInorden(nodoActual.get.subramaDerecha, bufferInorden)
        }
      }
    }
  }

  // Recorre el árbol en post-orden y agrega los elementos al búfer.
  private def recorrerPostorden
  (nodoActual: Option[NodoDosTres[T]], bufferPostorden: ListBuffer[T]): Unit = {
    if (nodoActual.isDefined) {
      if (nodoActual.get.esHoja) {
        bufferPostorden.append(nodoActual.get.raizIzquierda.get)

        if (nodoActual.get.raizDerecha.isDefined) {
          bufferPostorden.append(nodoActual.get.raizDerecha.get)
        }
      }
    } else {
      recorrerPostorden(nodoActual.get.subramaIzquierda, bufferPostorden)
      recorrerPostorden(nodoActual.get.subramaCentro, bufferPostorden)

      bufferPostorden.append(nodoActual.get.raizIzquierda.get)

      if (nodoActual.get.raizDerecha.isDefined){
        recorrerPostorden(nodoActual.get.subramaDerecha, bufferPostorden)

        if (!nodoActual.get.esHoja){
          bufferPostorden.append(nodoActual.get.raizDerecha.get)
        }
      }
    }
  }

  // TODO Probar
  // TODO Incorporar contravarianza
}