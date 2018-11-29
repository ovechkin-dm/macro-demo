package demo

import impl.{DelegateProducer, Delegated}
import impl.DelegateProducer.MethodInfo

object Main extends App {

  @Delegated
  trait SimpleTrait extends scala.AnyRef {
    def test(i: Int): Int
  }

  val ev = implicitly[DelegateProducer[SimpleTrait]]

  val simple = new SimpleTrait {
    def test(i: Int): Int = i
  }

  val stopWatch: (MethodInfo, () => Any) => Any = { (info, func) =>
    val start = System.currentTimeMillis()
    val result = func()
    val end = System.currentTimeMillis()
    val time = end - start
    println(s"Executed method ${info.methodName}. Args: ${info.args} Took $time ms.")
    result
  }

  val del = ev.delegate(simple)(stopWatch)
  println(del.test(10))


}
