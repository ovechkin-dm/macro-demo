package demo

import impl.DelegateProducer

object Main extends App {

  val ev = implicitly[DelegateProducer[SimpleTrait]]
  val simple = new SimpleTrait {
    def test(i: Int): Unit = ()
  }
  val del = ev.delegate(simple)
  del.test(10)

}
