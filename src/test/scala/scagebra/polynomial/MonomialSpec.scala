package scagebra
package polynomial

import org.scalatest._

import Monomial.Implicits._
import Rational.Implicits._

class MonomialSpec extends FunSuite {

  test("Monomial has order") {
    val x = "x"
    val y = "y"
    val z = "z"
    assert(v(x ^ 1, y ^ 1) > v(x ^ 1))
    assert(v(x ^ 1, y ^ 1) > v(x ^ 1, z ^ 1))
    assert(v(x ^ 0, y ^ 0, z ^ 0) < v(x ^ 0, z ^ 1))
    assert(v(x ^ 2) > v(x ^ 1, y ^ 1))
    assert(v(x ^ 1, z ^ 1) < v(x ^ 1, y ^ 1))
    assert(v(x ^ 1) < v(x ^ 1, y ^ 1))
    assert(v(x ^ 1) < v(x ^ 2))
    assert(v(x ^ 1, y ^ 2, z ^ 3) == v(x ^ 1, y ^ 2, z ^ 3))
    assert(v(y ^ 2, x ^ 1, z ^ 3) == v(x ^ 1, y ^ 2, z ^ 3))
    assert(v(z ^ 3, y ^ 2, x ^ 1) == v(x ^ 1, y ^ 2, z ^ 3))
  }
}
