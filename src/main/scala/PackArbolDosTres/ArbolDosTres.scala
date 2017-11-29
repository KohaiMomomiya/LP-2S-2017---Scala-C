package PackArbolDosTres

import scala.collection.mutable.ListBuffer



class ArbolDosTres[-A <: Ordered[A]](private var raiz: Option[NodoDosTres[A]]) {

  type B >: A

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

  override def clone(): ArbolDosTres[A] = {
    val clon = new ArbolDosTres[A](this.raiz)
    if (this.esVacio()) {
      clonarNodos(this.raiz.asInstanceOf[Option[NodoDosTres[B]]],
          clon.asInstanceOf[ArbolDosTres[B]])
    }
    clon
  }

  private def clonarNodos(nodoActual: Option[NodoDosTres[B]], clon: ArbolDosTres[B]): Unit = {
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


  def insertar(nuevoElemento: A): Boolean = {
    nuevoValorInsertado = false

    if (raiz.isEmpty || raiz.get.raizIzquierda.isEmpty) {
      if (raiz.isEmpty) {
        val nuevaRaiz: NodoDosTres[A] = new NodoDosTres[A](Option[A](nuevoElemento))
        raiz = Option[NodoDosTres[A]](nuevaRaiz)
      } else {
        this.raiz.get.raizIzquierda = Option[A](nuevoElemento)
      }
      tamaño += 1
      nuevoValorInsertado = true
    }
    else {
      val nuevaRaiz: Option[NodoDosTres[A]] =
          insertarEnArbol(raiz.asInstanceOf[Option[NodoDosTres[B]]],
              nuevoElemento).asInstanceOf[Option[NodoDosTres[A]]]
      if (nuevaRaiz.isDefined) {
        raiz = nuevaRaiz
      }
      if (nuevoValorInsertado) {
        tamaño += 1
      }
    }

    nuevoValorInsertado
  }

  def insertarLista(listaElementos: List[A]): Unit = {
    for (elemento <- listaElementos){
      insertar(elemento)
      if (!nuevoValorInsertado) {
        println(s"No se pudo insertar el valor: ${elemento.toString} en el árbol.\n")
      }
    }
  }


  private def insertarEnArbol(nodoActual: Option[NodoDosTres[B]],
                              nuevoElemento: A): Option[NodoDosTres[B]] = {
    var nuevoNodoRaiz: Option[NodoDosTres[A]] = None

    // No se ha llegado a un nodo hoja.
    if (!nodoActual.get.esHoja) {
      // El nuevo dato ya se encuentra registrado.
      if (nodoActual.get.raizIzquierda.get == nuevoElemento ||
          (nodoActual.get.esNodo3 && nodoActual.get.raizDerecha.get == nuevoElemento)) {
        println("El dato ingresado ya se encuentra registrado.")
        return None
      }

      // El nuevo elemento debe ir antes que la raíz izquierda del nodo actual.
      else if (nuevoElemento < nodoActual.get.raizIzquierda.get.asInstanceOf[A]) {
        nuevoNodoRaiz =
          insertar_subramaIzquierda(nodoActual.get, nuevoElemento)
            .asInstanceOf[Option[NodoDosTres[A]]]
      }

      // El nuevo elemento debe ir entre las raíces izquierda y derecha del nodo actual.
      else if (nodoActual.get.esNodo2 ||
          (nodoActual.get.esNodo3 &&
            (nodoActual.get.raizDerecha.get.asInstanceOf[A] > nuevoElemento))) {
        nuevoNodoRaiz =
          insertar_subramaCentro(nodoActual.get, nuevoElemento)
            .asInstanceOf[Option[NodoDosTres[A]]]
      }

      // El nuevo elemento debe ir después que la raíz derecha del nodo actual.
      else if (nodoActual.get.esNodo3 ||
          (nodoActual.get.raizDerecha.get.asInstanceOf[A] < nuevoElemento)) {
        nuevoNodoRaiz =
          insertar_subramaDerecha(nodoActual.get.asInstanceOf[NodoDosTres[A]], nuevoElemento)
            .asInstanceOf[Option[NodoDosTres[A]]]
      }
    }
      // El nodo actual es una hoja.
    else {
      nuevoNodoRaiz = insertar_NodoHoja(nodoActual.get.asInstanceOf[NodoDosTres[A]], nuevoElemento)
        .asInstanceOf[Option[NodoDosTres[A]]]
    }
    nuevoNodoRaiz.asInstanceOf[Option[NodoDosTres[B]]]
  }


  // FIXME: Error al insertar en sub-árbol izquierdo.
  private def insertar_subramaIzquierda
      (nodoActual: NodoDosTres[B], nuevoElemento: A): Option[NodoDosTres[B]] = {

    val nodoElevado: Option[NodoDosTres[B]] =
      insertarEnArbol(nodoActual.subramaIzquierda.asInstanceOf[Option[NodoDosTres[B]]],
          nuevoElemento)

    if (nodoElevado.isDefined) {
      val nodoElevado_ = nodoElevado.get

      if (nodoActual.esNodo2) {
        nodoActual.raizDerecha = nodoActual.raizIzquierda
        nodoActual.raizIzquierda = nodoElevado_.raizIzquierda

        nodoActual.subramaDerecha = nodoActual.subramaCentro
        nodoActual.subramaCentro = nodoElevado_.subramaCentro
        nodoActual.subramaIzquierda =
            nodoElevado_.subramaIzquierda
      } else {
        val copiaRamaDerecha = Option[NodoDosTres[A]](new NodoDosTres[A]
          (Option[A](nodoActual.raizDerecha.get.asInstanceOf[A]), None,
            nodoActual.subramaCentro.asInstanceOf[Option[NodoDosTres[A]]],
            nodoActual.subramaDerecha.asInstanceOf[Option[NodoDosTres[A]]]
          ))

        Option[NodoDosTres[A]](
          new NodoDosTres[A](Option[A](nodoActual.raizIzquierda.get.asInstanceOf[A]), None,
              nodoElevado.asInstanceOf[Option[NodoDosTres[A]]], copiaRamaDerecha))
      }
    }
    None
  }

  private def insertar_subramaCentro
      (nodoActual: NodoDosTres[B], nuevoElemento: A): Option[NodoDosTres[B]] = {
    val nodoElevado: Option[NodoDosTres[B]] =
      insertarEnArbol(nodoActual.subramaCentro, nuevoElemento)

    if (nodoElevado.isDefined) {
      val nodoElevado_ = nodoElevado.get

      if (nodoActual.esNodo2) {
        nodoActual.raizDerecha = nodoElevado_.raizIzquierda
        nodoActual.subramaDerecha = nodoElevado_.subramaCentro
        nodoActual.subramaCentro = nodoActual.subramaIzquierda
      } else {
        val nuevoNodoIzq = Option[NodoDosTres[A]](new NodoDosTres[A]
            (nodoActual.raizIzquierda.asInstanceOf[Option[A]], None,
              nodoActual.subramaIzquierda.asInstanceOf[Option[NodoDosTres[A]]],
              nodoElevado_.subramaIzquierda.asInstanceOf[Option[NodoDosTres[A]]]))
        val nuevoNodoDer = Option[NodoDosTres[A]](new NodoDosTres[A]
            (nodoActual.raizDerecha.asInstanceOf[Option[A]], None,
              nodoElevado_.subramaCentro.asInstanceOf[Option[NodoDosTres[A]]],
              nodoActual.subramaDerecha.asInstanceOf[Option[NodoDosTres[A]]]))
        return Option[NodoDosTres[B]](
          new NodoDosTres[B](nodoElevado_.raizIzquierda.asInstanceOf[Option[B]], None,
            nodoActual.subramaIzquierda,
            nodoActual.subramaCentro))
      }
    }
    None
  }

  private def insertar_subramaDerecha
      (nodoActual: NodoDosTres[A], nuevoElemento: A) : Option[NodoDosTres[B]] = {
    val nodoElevado: Option[NodoDosTres[A]] =
      insertarEnArbol(nodoActual.subramaDerecha.asInstanceOf[], nuevoElemento)

    if (nodoElevado.isDefined) {
      val nodoElevado_ = nodoElevado.get

      val copiaRamaIzquierda = Option[NodoDosTres[A]](new NodoDosTres[A]
        (nodoActual.raizIzquierda, None,
          nodoActual.subramaIzquierda,
          nodoActual.subramaCentro))

      return Option[NodoDosTres[B]](
        new NodoDosTres[B](nodoActual.raizDerecha.asInstanceOf[Option[B]], None,
          copiaRamaIzquierda, nodoElevado))
    }
    None
  }

  private def insertar_NodoHoja
      (nodoActual: NodoDosTres[A], nuevoElemento: A): Option[NodoDosTres[B]] = {
    nuevoValorInsertado = true

    if ((nodoActual.raizIzquierda.get == nuevoElemento) ||
        (nodoActual.esNodo3 && nodoActual.raizDerecha.get == nuevoElemento)) {
      nuevoValorInsertado = false
      Option[NodoDosTres[A]](nodoActual)
    }
    // El nodo actual sólo tiene la raíz izquierda.
    else if (nodoActual.esNodo2) {
      if (nodoActual.raizIzquierda.get.asInstanceOf[A] > nuevoElemento) {
        nodoActual.raizDerecha = nodoActual.raizIzquierda
        nodoActual.raizIzquierda = Option[A](nuevoElemento)
      } else if (nodoActual.raizIzquierda.get.asInstanceOf[A] < nuevoElemento) {
        nodoActual.raizDerecha = Option[A](nuevoElemento)
      }
      Option[NodoDosTres[A]](nodoActual)
    }
    // El nodo actual ya tiene dos raíces. Se debe dividirNodo el nodo actual.
    else {
      dividirNodo(nodoActual.asInstanceOf[NodoDosTres[A]], nuevoElemento)
    }
  }

  private def dividirNodo(nodoActual: NodoDosTres[A], nuevoElemento: A): Option[NodoDosTres[B]] = {

    // El nuevo elemento debe ir antes que la raíz izquierda del nodo actual.
    if (nodoActual.raizIzquierda.get > nuevoElemento) {
      val nuevaSubramaDerecha = Option[NodoDosTres[A]](new NodoDosTres[A](Option[A](nuevoElemento)))
      val nuevaSubramaIzquierda =
        Option[NodoDosTres[A]](new NodoDosTres[A](Option[A](nodoActual.raizDerecha.get)))

      return Option[NodoDosTres[A]](new NodoDosTres[A](nodoActual.raizIzquierda, None,
          nuevaSubramaIzquierda, nuevaSubramaDerecha))
    }

    // El nuevo elemento debe ir después que la raíz izquierda del nodo actual.
    else if (nodoActual.raizIzquierda.get < nuevoElemento) {

      // El nuevo elemento debe ir entre la raíz izquierda y derecha. El nuevo elemento es elevado.
      if (nodoActual.raizDerecha.get > nuevoElemento) {
        val nuevaSubramaIzq = Option[NodoDosTres[A]](new NodoDosTres[A](nodoActual.raizIzquierda))
        val nuevaSubramaDer = Option[NodoDosTres[A]](new NodoDosTres[A](nodoActual.raizDerecha))

        return Option[NodoDosTres[B]](
          new NodoDosTres[B](Option[B](nuevoElemento.asInstanceOf[B]), None,
            nuevaSubramaIzq.asInstanceOf[Option[NodoDosTres[B]]],
            nuevaSubramaDer.asInstanceOf[Option[NodoDosTres[B]]]))
      }

      // El nuevo elemento debe ir después que la raíz derecha. La raíz derecha es elevada.
      else {
        val nuevaSubramaIzq = Option[NodoDosTres[A]](new NodoDosTres[A](nodoActual.raizIzquierda))
        val nuevaSubramaDer = Option[NodoDosTres[A]](new NodoDosTres[A](Option[A](nuevoElemento)))

        return Option[NodoDosTres[B]](
          new NodoDosTres[B](Option[B](nodoActual.raizDerecha.get.asInstanceOf[B]), None,
            nuevaSubramaIzq.asInstanceOf[Option[NodoDosTres[B]]],
            nuevaSubramaDer.asInstanceOf[Option[NodoDosTres[B]]])
        )
      }
    }

    None
  }


  def modificar(elementoAnterior: A, elementoNuevo: A): Boolean = {
    if (contiene(elementoAnterior)){
      borrar(elementoAnterior)
      insertar(elementoNuevo)
      return true
    }
    false
  }


  def contiene(elementoBuscado: A) : Boolean = {
    val encontrado: Option[B] = encontrar(elementoBuscado)
    encontrado.isDefined
  }

  def encontrar(elementoBuscado: A) : Option[B] = {
    encontrarEnArbol(raiz.asInstanceOf[Option[NodoDosTres[B]]], elementoBuscado)
  }

  private def encontrarEnArbol
      (nodoActual: Option[NodoDosTres[B]], elementoBuscado: A): Option[B] = {
    if (nodoActual.isDefined) {
      val nodoActual_ = nodoActual.get

      if (nodoActual_.raizIzquierda.isDefined &&
          (nodoActual_.raizIzquierda.get == elementoBuscado)) {
        return nodoActual.get.raizIzquierda
      } else {
        if (nodoActual_.raizDerecha.isDefined && (nodoActual_.raizDerecha.get == elementoBuscado)) {
          return nodoActual_.raizDerecha
        } else {
          if (nodoActual_.raizIzquierda.get.asInstanceOf[A] > elementoBuscado) {
            return encontrarEnArbol(nodoActual_.subramaIzquierda, elementoBuscado)
          }
          else if (nodoActual_.subramaDerecha.isEmpty &&
              (nodoActual_.raizDerecha.get.asInstanceOf[A] > elementoBuscado)) {
            return encontrarEnArbol(nodoActual_.subramaCentro, elementoBuscado)
          }
          else if (nodoActual_.raizDerecha.get.asInstanceOf[A] < elementoBuscado) {
            return encontrarEnArbol(nodoActual_.subramaDerecha, elementoBuscado)
          }
          else
            return None
        }
      }
    }

    None
  }

  def esVacio() : Boolean = {
    raiz.isEmpty || raiz.get.raizIzquierda.isEmpty
  }


  // Elimina un elemento del árbol.
  // Se retorna un Boolean indicando si el nodo pudo ser borrado exitosamente.
  def borrar(elemento: A): Boolean = {
    var elementoBorrado: Boolean = borrarEnArbol(raiz.asInstanceOf[Option[NodoDosTres[B]]], elemento)
    raiz.get.balancear()    // Rebalancear raíz de árbol.

    if (elementoBorrado) {
      if (raiz.get.raizIzquierda.isEmpty) {
        raiz = None
      }
      tamaño -= 1
    }

    elementoBorrado
  }

  private def borrarEnArbol(nodoActual: Option[NodoDosTres[B]], elemento: A): Boolean = {
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
        if (nodoActual.get.raizDerecha.isEmpty || nodoActual.get.raizDerecha.get.asInstanceOf[A] > elemento) {

          // Elemento a borrar está dentro de la rama izquierda.
          if (nodoActual.get.raizIzquierda.get.asInstanceOf[A] > elemento) {
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
              val reemplazo =
                nodoActual.get.subramaDerecha.get.cambiarMinimo().asInstanceOf[Option[B]]
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
          val maximoSubramaIzq =
            nodoActual.get.subramaIzquierda.get.cambiarMaximo().asInstanceOf[Option[B]]
          nodoActual.get.raizIzquierda = maximoSubramaIzq
        }
      }
    }

    // Luego de buscar y borrar el elemento del árbol, se rebalancea el sub-árbol cuya raíz es el
    // nodo actual.
    rebalancearSubArbol(nodoActual.asInstanceOf[Option[NodoDosTres[B]]])
    fueBorrado
  }

  // Sub-rutina para balancear sub-árbol cuya raíz es el nodo actual.
  private def rebalancearSubArbol(nodoActual: Option[NodoDosTres[B]]): Unit = {
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
            val elementoReinsercion = nodoActual.get.raizIzquierda.get.asInstanceOf[A]

            nodoActual.get.raizIzquierda = reemplazo.asInstanceOf[Option[B]]
            insertar(elementoReinsercion)
          }
          else if
          (!nodoActual.get.subramaIzquierda.get.esHoja &&
              nodoActual.get.subramaCentro.get.esHoja) {
            if (nodoActual.get.raizDerecha.isEmpty) {
              val reemplazo = nodoActual.get.subramaIzquierda.get.cambiarMaximo()
              val elementoReinsercion = nodoActual.get.raizIzquierda.get.asInstanceOf[A]

              nodoActual.get.raizIzquierda = reemplazo.asInstanceOf[Option[B]]
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
            val elementoReinsercion = nodoActual.get.raizDerecha.get.asInstanceOf[A]

            nodoActual.get.raizDerecha = reemplazo.asInstanceOf[Option[B]]
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



  // Iterador pre-orden
  def iteradorPreorden(): Iterator[B] = {
    generarListaPreorden().iterator
  }

  // Genera una lista pre-orden de los elementos del árbol.
  def generarListaPreorden(): List[B] = {
    if (this.esVacio()) {
      println("No contiene elementos.")
      List[B]()
    } else {
      val bufferPreorden: ListBuffer[B] = ListBuffer[B]()
      recorrerPreorden(Option[NodoDosTres[B]](this.raiz.asInstanceOf[NodoDosTres[B]]), bufferPreorden)
      bufferPreorden.toList
    }
  }

  // Recorre el árbol en pre-orden y agrega los elementos al búfer.
  private def recorrerPreorden
    (nodoActual: Option[NodoDosTres[B]], bufferPreorden: ListBuffer[B]): Unit = {
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

  // Iterador in-orden
  def iteradorInorden(): Iterator[B] = {
    generarListaInorden().iterator
  }

  // Genera una lista in-orden de los elementos del árbol.
  def generarListaInorden(): List[B] = {
    if (this.esVacio()) {
      println("No contiene elementos.")
      List[B]()
    } else {
      val bufferInorden: ListBuffer[B] = ListBuffer[B]()
      recorrerInorden(this.raiz.asInstanceOf[Option[NodoDosTres[B]]], bufferInorden)
      bufferInorden.toList
    }
  }

  // Recorre el árbol en in-orden y agrega los elementos al búfer.
  private def recorrerInorden
    (nodoActual: Option[NodoDosTres[B]], bufferInorden: ListBuffer[B]): Unit = {
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


  // Iterador post-orden
  def iteradorPostorden(): Iterator[B] = {
    generarListaPostorden().iterator
  }

  // Genera una lista post-orden de los elementos del árbol.
  def generarListaPostorden(): List[B] = {
    if (this.esVacio()) {
      println("No contiene elementos.")
      List[B]()
    } else {
      val bufferPostorden: ListBuffer[B] = ListBuffer[B]()
      recorrerPostorden(this.raiz.asInstanceOf[Option[NodoDosTres[B]]], bufferPostorden)
      bufferPostorden.toList
    }
  }

  // Recorre el árbol en post-orden y agrega los elementos al búfer.
  private def recorrerPostorden
      (nodoActual: Option[NodoDosTres[B]], bufferPostorden: ListBuffer[B]): Unit = {
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