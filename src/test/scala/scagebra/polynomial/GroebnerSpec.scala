package scagebra
package polynomial

import org.scalatest._

import Rational.Implicits._
import Monomial.Implicits._
import Polynomial.Implicits._
import Groebner._

class GroebnerSpec extends FunSuite {

  val x = "x"
  val y = "y"
  val z = "z"

  test("Polynomial has LT") {
    // LT(x^1) == x^1
    assert(
      p(m(1, v(x ^ 1))).LT == m(1, v(x ^ 1))
    )
    // LT(2 x + x^2) == x^2
    assert(
      p(m(2, v(x ^ 1)), m(1, v(x ^ 2))).LT
        == m(1, v(x ^ 2))
    )
    // LT(2 x y^2 - 2 x y) == 2 x y^2
    assert(
      p(m(2, v(x ^ 1, y ^ 2)), m(-2, v(x ^ 1, y ^ 1))).LT
        == m(2, v(x ^ 1, y ^ 2))
    )
    // LT(2 x + y z) == 2 x
    assert(
      p(m(2, v(x ^ 1)), m(1, v(y ^ 1, z ^ 1))).LT
        == m(2, v(x ^ 1))
    )
    // LT(2 y + y z) = y z
    assert(
      p(m(2, v(y ^ 1)), m(1, v(y ^ 1, z ^ 1))).LT
        == m(1, v(y ^ 1, z ^ 1))
    )
  }
}
