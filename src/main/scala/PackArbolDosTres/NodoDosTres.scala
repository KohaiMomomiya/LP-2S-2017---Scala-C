package PackArbolDosTres

class NodoDosTres[T](
                      private[PackArbolDosTres] var raizIzquierda: Option[T]
                    ) extends Balanceable[T]{
  private[PackArbolDosTres] var raizDerecha: Option[T] = None

  private[PackArbolDosTres] var subramaIzquierda: Option[NodoDosTres[T]] = None
  private[PackArbolDosTres] var subramaCentro: Option[NodoDosTres[T]] = None
  private[PackArbolDosTres] var subramaDerecha: Option[NodoDosTres[T]] = None


  def this(raizIzquierda: T){
    this(Option[T](raizIzquierda))
  }

  def this(raizIzquierda: T, raizDerecha: Option[T] = None,
            subRamaIzq: Option[NodoDosTres[T]] = None,
            subramaCentro: Option[NodoDosTres[T]] = None) {
    this(raizIzquierda)
    this.raizDerecha = raizDerecha

    this.subramaIzquierda = subRamaIzq
    this.subramaCentro = subramaCentro
  }

  // Getters de valores en raíz:
  def RaizIzquierda : Option[T] = this.raizIzquierda

  def RaizDerecha : Option[T] = this.raizDerecha    // Puede ser None o contener el valor.

  // Getters de ramas del nodo (en contenedores Option[T]):
  def SubramaIzquierda : Option[NodoDosTres[T]] = this.subramaIzquierda

  def SubramaCentro : Option[NodoDosTres[T]] = this.subramaCentro

  def SubramaDerecha : Option[NodoDosTres[T]] = this.subramaDerecha

  // Setters de valores en raíz:
  private[PackArbolDosTres] def RaizIzquierda_(nuevaRaizIzq: T): Unit = {
    RaizIzquierda_(Option[T](nuevaRaizIzq))
  }

  private[PackArbolDosTres] def RaizIzquierda_(nuevaRaizIzq: Option[T]): Unit = {
    this.raizIzquierda = nuevaRaizIzq
  }

  private[PackArbolDosTres] def RaizDerecha_(nuevaRaizDer: T): Unit = {
    RaizDerecha_(Option[T](nuevaRaizDer))
  }

  private[PackArbolDosTres] def RaizDerecha_(nuevaRaizDer: Option[T]): Unit = {
    this.raizDerecha = nuevaRaizDer
  }


  // Setters de ramas del nodo:
  private def SubRamaIzquierda_(nuevaSubIzq: NodoDosTres[T]): Unit = {
    SubRamaIzquierda_(Option[NodoDosTres[T]](nuevaSubIzq))
  }

  private def SubRamaIzquierda_(nuevaSubIzq: Option[NodoDosTres[T]]): Unit = {
    subramaIzquierda = nuevaSubIzq
  }

  private def SubRamaCentro_(nuevaSubCentro: NodoDosTres[T]): Unit = {
    SubRamaCentro_(Option[NodoDosTres[T]](nuevaSubCentro))
  }

  private def SubRamaCentro_(nuevaSubCentro: Option[NodoDosTres[T]]): Unit = {
    subramaCentro = nuevaSubCentro
  }

  private def SubRamaDerecha_(nuevaSubDer: NodoDosTres[T]): Unit = {
    SubRamaDerecha_(Option[NodoDosTres[T]](nuevaSubDer))
  }

  private def SubRamaDerecha_(nuevaSubDer: Option[NodoDosTres[T]]): Unit = {
    subramaDerecha = nuevaSubDer
  }


  // Revisa si el nodo es una hoja, nodo 2 (con 2 sub-ramas) o nodo 3 (con 3 sub-ramas):
  def esHoja : Boolean = {
    subramaIzquierda.isEmpty && subramaCentro.isEmpty && subramaDerecha.isEmpty
  }

  def esNodo2 : Boolean = {
    raizDerecha.isEmpty
  }

  def esNodo3 : Boolean = {
    raizDerecha.isDefined
  }


  // Mecanismos para balancear nodo.
  override def estaBalanceado(): Boolean = {
    if (esHoja) {
      return true
    }
    else if (subramaIzquierda.get.raizIzquierda.isDefined &&
      subramaCentro.get.raizIzquierda.isDefined) {

      // Nodo 3
      if (raizDerecha.isDefined) {
        if (subramaDerecha.get.raizIzquierda.isDefined) {
          return true
        }
        return false
      }

      // Nodo 2
      else {
        return true
      }
    }
    false
  }

  override protected[PackArbolDosTres] def balancear(): Unit = {
    while(!estaBalanceado()) {
      if (subramaIzquierda.get.raizIzquierda.isEmpty) {
        balancearSubramaIzquierda()
      } else if (subramaCentro.get.raizIzquierda.isEmpty) {
        balancearSubramaCentro()
      } else if (subramaDerecha.isDefined && subramaDerecha.get.raizIzquierda.isEmpty) {
        balancearSubramaDerecha()
      }
    }
  }

  private[PackArbolDosTres] def cambiarMaximo(): Option[T] = {
    var maximo: Option[T] = None

    if (!esHoja){
      if (raizDerecha.isDefined) {
        maximo = subramaDerecha.get.cambiarMaximo()
      } else {
        maximo = subramaCentro.get.cambiarMaximo()
      }
    } else {
      if (raizDerecha.isDefined) {
        maximo = raizDerecha
        raizDerecha = None
      } else {
        maximo = raizIzquierda
        raizIzquierda = None
      }
    }

    if (!estaBalanceado()) {
      balancear()
    }

    maximo
  }

  private[PackArbolDosTres] def cambiarMinimo(): Option[T] = {
    var minimo: Option[T] = None

    if (!esHoja) {
      minimo = subramaIzquierda.get.cambiarMinimo()
    } else {
      minimo = raizIzquierda

      if (raizDerecha != null) {
        raizIzquierda = raizDerecha
        raizDerecha = None
      } else {
        raizIzquierda = None
      }
    }

    if (!estaBalanceado()) {
      balancear()
    }

    minimo
  }

  private def balancearSubramaIzquierda() : Unit = {
    subramaIzquierda.get.raizIzquierda = raizIzquierda
    raizIzquierda = subramaCentro.get.raizIzquierda

    if (subramaCentro.get.raizDerecha.isDefined) {
      subramaCentro.get.raizIzquierda = subramaCentro.get.raizDerecha
      subramaCentro.get.raizDerecha = None
    } else {
      subramaCentro.get.raizIzquierda = None
    }
  }

  private def balancearSubramaCentro() : Unit = {
    if (raizDerecha.isEmpty) {
      if (subramaIzquierda.get.raizIzquierda.isDefined && subramaIzquierda.get.raizDerecha.isEmpty
          && subramaCentro.get.raizIzquierda.isEmpty) {
        raizDerecha = raizIzquierda
        raizIzquierda = subramaIzquierda.get.raizIzquierda
        cortarSubramas()
      } else {
        subramaCentro.get.raizIzquierda = raizIzquierda

        if (subramaIzquierda.get.raizDerecha.isEmpty) {
          raizIzquierda = subramaIzquierda.get.raizIzquierda
          subramaIzquierda.get.raizIzquierda = None
        } else {
          raizIzquierda = subramaIzquierda.get.raizDerecha
          subramaIzquierda.get.raizDerecha = None
        }

        if (subramaIzquierda.get.raizIzquierda.isEmpty && subramaCentro.get.raizIzquierda.isEmpty) {
          cortarSubramas()
        }
      }
    } else {
      subramaCentro.get.raizIzquierda = raizDerecha
      raizDerecha = subramaDerecha.get.raizIzquierda

      if (subramaDerecha.get.raizDerecha.isDefined) {
        subramaDerecha.get.raizIzquierda = subramaDerecha.get.raizDerecha
        subramaDerecha.get.raizDerecha = None
      } else {
        subramaDerecha.get.raizIzquierda = None
      }
    }
  }

  private def balancearSubramaDerecha() : Unit = {
    if (subramaCentro.get.raizDerecha.isDefined) {
      subramaDerecha.get.raizIzquierda = raizDerecha
      raizDerecha = subramaCentro.get.raizDerecha
      subramaCentro.get.raizDerecha = None
    } else {
      subramaCentro.get.raizDerecha = raizDerecha
      raizDerecha = None
    }
  }

  private def cortarSubramas() : Unit = {
    subramaIzquierda = None
    subramaCentro = None
    subramaDerecha = None
  }
}

