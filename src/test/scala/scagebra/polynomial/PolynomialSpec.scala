package scagebra
package polynomial

import org.scalatest._

import Variables.Implicits._
import Rational.Implicits._
import Polynomial.Implicits._

class PolynomialSpec extends FunSuite {

  val x = "x"
  val y = "y"
  val z = "z"

  test("Variables has order") {
    // x + 2x == 3x
    assert(
      p(m(1, v(x ^ 1))) + p(m(2, v(x ^ 1)))
        == p(m(3, v(x ^ 1)))
    )
    // 2 x + x^2 == 2 x + x^2
    assert(
      p(m(2, v(x ^ 1))) + p(m(1, v(x ^ 2)))
        == p(m(2, v(x ^ 1)), m(1, v(x ^ 2)))
    )
    // 2 x y - 2 x y == 0
    assert(
      p(m(2, v(x ^ 1, y ^ 1))) - p(m(2, v(x ^ 1, y ^ 1)))
        == p(m(0, v[String]()))
    )
  }
}
