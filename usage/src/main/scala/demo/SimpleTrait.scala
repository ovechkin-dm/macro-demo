package demo

import impl.Delegated

@Delegated
trait SimpleTrait {

  def test(i: Int): Unit

}
