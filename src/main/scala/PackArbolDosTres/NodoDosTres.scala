package PackArbolDosTres


class NodoDosTres[-A](
                      private[PackArbolDosTres] var raizIzquierda: Option[A] = None,
                      private[PackArbolDosTres] var raizDerecha: Option[A] = None,

                      private[PackArbolDosTres] var subramaIzquierda: Option[NodoDosTres[A]] = None,
                      private[PackArbolDosTres] var subramaCentro: Option[NodoDosTres[A]] = None,
                      private[PackArbolDosTres] var subramaDerecha: Option[NodoDosTres[A]] = None,
                     ) extends Balanceable[A]{

  type B >: A


  // Getters de valores en raíz:
  def RaizIzquierda : Option[B] = this.raizIzquierda.asInstanceOf[Option[B]]

  def RaizDerecha : Option[B] = this.raizDerecha.asInstanceOf[Option[B]]

  // Getters de ramas del nodo (en contenedores Option[A]):
  def SubramaIzquierda : Option[NodoDosTres[B]] =
    this.subramaIzquierda.asInstanceOf[Option[NodoDosTres[B]]]

  def SubramaCentro : Option[NodoDosTres[B]] =
    this.subramaCentro.asInstanceOf[Option[NodoDosTres[B]]]

  def SubramaDerecha : Option[NodoDosTres[B]] =
    this.subramaDerecha.asInstanceOf[Option[NodoDosTres[B]]]

  // Setters de valores en raíz:
  private[PackArbolDosTres] def RaizIzquierda_(nuevaRaizIzq: A): Unit = {
    RaizIzquierda_(nuevaRaizIzq)
  }

  private[PackArbolDosTres] def RaizIzquierda_(nuevaRaizIzq: Option[A]): Unit = {
    this.raizIzquierda = nuevaRaizIzq
  }

  private[PackArbolDosTres] def RaizDerecha_(nuevaRaizDer: A): Unit = {
    RaizDerecha_(nuevaRaizDer)
  }

  private[PackArbolDosTres] def RaizDerecha_(nuevaRaizDer: Option[A]): Unit = {
    this.raizDerecha = nuevaRaizDer
  }


  // Setters de ramas del nodo:
  private def SubramaIzquierda_(nuevaSubIzq: NodoDosTres[B]): Unit = {
    SubramaIzquierda_(Option[NodoDosTres[B]](nuevaSubIzq))
  }

  private def SubramaIzquierda_(nuevaSubIzq: Option[NodoDosTres[B]]): Unit = {
    subramaIzquierda = nuevaSubIzq.asInstanceOf[Option[NodoDosTres[A]]]
  }

  private def SubramaCentro_(nuevaSubCentro: NodoDosTres[B]): Unit = {
    SubramaCentro_(Option[NodoDosTres[B]](nuevaSubCentro))
  }

  private def SubramaCentro_(nuevaSubCentro: Option[NodoDosTres[B]]): Unit = {
    subramaCentro = nuevaSubCentro.asInstanceOf[Option[NodoDosTres[A]]]
  }

  private def SubramaDerecha_(nuevaSubDerecha: NodoDosTres[B]): Unit = {
    SubramaDerecha_(Option[NodoDosTres[B]](nuevaSubDerecha))
  }

  private def SubramaDerecha_(nuevaSubDerecha: Option[NodoDosTres[B]]): Unit = {
    subramaCentro = nuevaSubDerecha.asInstanceOf[Option[NodoDosTres[A]]]
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

  private[PackArbolDosTres] def cambiarMaximo(): Option[B] = {
    var maximo: Option[B] = None

    if (!esHoja){
      if (raizDerecha.isDefined) {
        maximo = subramaDerecha.get.cambiarMaximo()
      } else {
        maximo = subramaCentro.get.cambiarMaximo()
      }
    } else {
      if (raizDerecha.isDefined) {
        maximo = raizDerecha.asInstanceOf[Option[B]]
        raizDerecha = None
      } else {
        maximo = raizIzquierda.asInstanceOf[Option[B]]
        raizIzquierda = None
      }
    }

    if (!estaBalanceado()) {
      balancear()
    }

    maximo
  }

  private[PackArbolDosTres] def cambiarMinimo(): Option[B] = {
    var minimo: Option[B] = None

    if (!esHoja) {
      minimo = subramaIzquierda.get.cambiarMinimo()
    } else {
      minimo = raizIzquierda.asInstanceOf[Option[B]]

      if (raizDerecha != null) {
        raizIzquierda = raizDerecha.asInstanceOf[Option[A]]
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

