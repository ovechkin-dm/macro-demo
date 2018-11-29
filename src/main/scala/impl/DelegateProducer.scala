package impl

trait DelegateProducer[T] {

  def delegate(t: T): T

}
