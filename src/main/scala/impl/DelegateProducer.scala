package impl

import impl.DelegateProducer.MethodInfo

import scala.annotation.implicitNotFound

@implicitNotFound("You should annotate instance with @Delegated annotation")
trait DelegateProducer[T] {

  def delegate(t: T)(f: (MethodInfo, () => Any) => Any): T

}

object DelegateProducer {

  case class MethodInfo(methodName: String, count: Int, args: List[Any])

}
