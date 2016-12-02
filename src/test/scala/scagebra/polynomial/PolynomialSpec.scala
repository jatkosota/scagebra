package scagebra
package polynomial

import org.scalatest._

import Monomial.Implicits._
import Rational.Implicits._
import Polynomial.Implicits._

class PolynomialSpec extends FunSuite {

  val x = "x"
  val y = "y"
  val z = "z"

  test("Polynomial has tree arithmetic operations") {
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
        == p(Term[String](0))
    )

    // 2 x y - 3 x y == - x y
    assert(
      p(m(2, v(x ^ 1, y ^ 1))) - p(m(3, v(x ^ 1, y ^ 1)))
        == p(m(-1, v(x ^ 1, y ^ 1)))
    )

    // 2 x y - x y == x y
    assert(
      p(m(2, v(x ^ 1, y ^ 1))) - p(m(1, v(x ^ 1, y ^ 1)))
        == p(m(1, v(x ^ 1, y ^ 1)))
    )

    // 2 x y^2 - 2 x y == 2 x y^2 - 2 x y
    assert(
      p(m(2, v(x ^ 1, y ^ 2))) - p(m(2, v(x ^ 1, y ^ 1)))
        == p(m(2, v(x ^ 1, y ^ 2)), m(-2, v(x ^ 1, y ^ 1)))
    )

    // 2 x y * 0 x z == 0
    assert(
      p(m(2, v(x ^ 1, y ^ 1))) * p(m(0, v(x ^ 1, z ^ 1)))
        === p(Term[String](0))
    )
    // 0 x z * 2 x y == 0
    assert(
      p(m(0, v(x ^ 1, z ^ 1))) * p(m(2, v(x ^ 1, y ^ 1)))
        === p(Term[String](0))
    )
    // 2 x * 3 x y^2 == 6 x^2 y^2
    assert(
      p(m(2, v(x ^ 1))) * p(m(3, v(x ^ 1, y ^ 2)))
        == p(m(6, v(x ^ 2, y ^ 2)))
    )
    // (2 x + 3 x y^2) * 4 x y z == 8 x^2 y z + 12 x^2 y^3 z
    assert(
      (p(m(2, v(x ^ 1))) + p(m(3, v(x ^ 1, y ^ 2)))) * p(m(4, v(x ^ 1, y ^ 1, z ^ 1)))
        == p(m(8, v(x ^ 2, y ^ 1, z ^ 1)), m(12, v(x ^ 2, y ^ 3, z ^ 1)))
    )
    // (x + y) * (x - y) == x^2 - y^2
    assert(
      (p(m(1, v(x ^ 1))) + p(m(1, v(y ^ 1)))) * (p(m(1, v(x ^ 1))) - p(m(1, v(y ^ 1))))
        == p(m(1, v(x ^ 2)), m(-1, v(y ^ 2)))
    )
    // (x + y)(x^2 - x y + y^2) == x^3 + y^3
    assert(
      (p(m(1, v(x ^ 1))) + p(m(1, v(y ^ 1)))) * (p(m(1, v(x ^ 2))) - p(m(1, v(x ^ 1, y ^ 1))) + p(m(1, v(y ^ 2))))
        == p(m(1, v(x ^ 3)), m(1, v(y ^ 3)))
    )
    // (x - y)(x^2 + x y + y^2) == x^3 - y^3
    assert(
      (p(m(1, v(x ^ 1))) - p(m(1, v(y ^ 1)))) * (p(m(1, v(x ^ 2))) + p(m(1, v(x ^ 1, y ^ 1))) + p(m(1, v(y ^ 2))))
        == p(m(1, v(x ^ 3)), m(-1, v(y ^ 3)))
    )
  }

  test("Polynomial has negate") {
    // -(8 x^2 y z + 12 x^2 y^3 z) = (-8 x^2 y z) + (-12 x^2 y^3 z)
    assert(
      -p(m(8, v(x ^ 2, y ^ 1, z ^ 1)), m(12, v(x ^ 2, y ^ 3, z ^ 1)))
        == p(m(-8, v(x ^ 2, y ^ 1, z ^ 1)), m(-12, v(x ^ 2, y ^ 3, z ^ 1)))
    )
    // -(0) = (-0)
    assert(
      -p(Term[String](0)) == p(Term[String](0))
    )
  }
}
