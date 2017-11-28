package PackArbolDosTres

trait Balanceable[T] {
  protected def estaBalanceado() : Boolean
  protected def balancear() : Unit
}
