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
    assert(m(x ^ 1, y ^ 1) > m(x ^ 1))
    assert(m(x ^ 1, y ^ 1) > m(x ^ 1, z ^ 1))
    assert(m(x ^ 0, y ^ 0, z ^ 0) < m(x ^ 0, z ^ 1))
    assert(m(x ^ 2) > m(x ^ 1, y ^ 1))
    assert(m(x ^ 1, z ^ 1) < m(x ^ 1, y ^ 1))
    assert(m(x ^ 1) < m(x ^ 1, y ^ 1))
    assert(m(x ^ 1) < m(x ^ 2))
    assert(m(x ^ 1, y ^ 2, z ^ 3) == m(x ^ 1, y ^ 2, z ^ 3))
    assert(m(y ^ 2, x ^ 1, z ^ 3) == m(x ^ 1, y ^ 2, z ^ 3))
    assert(m(z ^ 3, y ^ 2, x ^ 1) == m(x ^ 1, y ^ 2, z ^ 3))
  }
}
