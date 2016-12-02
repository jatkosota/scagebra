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

  test("Polynomial can be divided by polynomials") {
    // x y^2 + 1 / [ x y + 1, y + 1 ] == [ y, -1 ]
    assert(
      p(m(1, v(x ^ 1, y ^ 2)), Monomial[String](1)) / List(
        p(m(1, v(x ^ 1, y ^ 1)), Monomial[String](1)),
        p(m(1, v(y ^ 1)), Monomial[String](1))
      )
        === List(p(m(1, v(y ^ 1))), p(Monomial[String](-1)))
    )
    // x y^2 + 1 % [ x y + 1, y + 1 ] == 2
    assert(
      p(m(1, v(x ^ 1, y ^ 2)), Monomial[String](1)) % List(
        p(m(1, v(x ^ 1, y ^ 1)), Monomial[String](1)),
        p(m(1, v(y ^ 1)), Monomial[String](1))
      )
        === p(Monomial[String](2))
    )

    // x^2 y + x y^2 + y^2 / [ x y - 1, y^2 - 1 ] == [ x + y, 1 ]
    assert(
      p(m(1, v(x ^ 2, y ^ 1)), m(1, v(x ^ 1, y ^ 2)), m(1, v(y ^ 2))) / List(
        p(m(1, v(x ^ 1, y ^ 1)), Monomial[String](-1)),
        p(m(1, v(y ^ 2)), Monomial[String](-1))
      )
        === List(p(m(1, v(x ^ 1)), m(1, v(y ^ 1))), p(Monomial[String](1)))
    )

    // x^2 y + x y^2 + y^2 % [ x y - 1, y^2 - 1 ] == x + y + 1
    assert(
      p(m(1, v(x ^ 2, y ^ 1)), m(1, v(x ^ 1, y ^ 2)), m(1, v(y ^ 2))) % List(
        p(m(1, v(x ^ 1, y ^ 1)), Monomial[String](-1)),
        p(m(1, v(y ^ 2)), Monomial[String](-1))
      )
        === p(m(1, v(x ^ 1)), m(1, v(y ^ 1)), Monomial[String](1))
    )

    // 3 x^3 + x^2 y - x + 2 / [ 2 x^2 + y, x y - x ] == [ (3/2) x + (1/2) y, -(3/2) ]
    assert(
      p(m(3, v(x ^ 3)), m(1, v(x ^ 2, y ^ 1)), m(-1, v(x ^ 1)), Monomial[String](2)) / List(
        p(m(2, v(x ^ 2)), m(1, v(y ^ 1))),
        p(m(1, v(x ^ 1, y ^ 1)), m(-1, v(x ^ 1)))
      )
        === List(
          p(m(Rational(3, 2), v(x ^ 1)), m(Rational(1, 2), v(y ^ 1))),
          p(m(-Rational(3, 2), v[String]()))
        )
    )

    // 3 x^3 + x^2 y - x + 2 % [ 2 x^2 + y, x y - x ] == -(5/2) x - (1/2) y^2 + 2
    assert(
      p(m(3, v(x ^ 3)), m(1, v(x ^ 2, y ^ 1)), m(-1, v(x ^ 1)), Monomial[String](2)) % List(
        p(m(2, v(x ^ 2)), m(1, v(y ^ 1))),
        p(m(1, v(x ^ 1, y ^ 1)), m(-1, v(x ^ 1)))
      )
        === p(m(-Rational(5, 2), v(x ^ 1)), m(-Rational(1, 2), v(y ^ 2)), Monomial[String](2))
    )
  }

  test("Two polynomials have LCM") {
    // LCM(x^3 y^2 - x^2 y^3 + x, 3 x^4 y + y^2)
    assert(LCM(
      p(m(1, v(x ^ 3, y ^ 2)), m(-1, v(x ^ 2, y ^ 3)), m(1, v(x ^ 1))),
      p(m(3, v(x ^ 4, y ^ 1)), m(1, v(y ^ 2)))
    ) == m(1, v(x ^ 4, y ^ 2)))
    // TODO add more tests
  }

  test("Two polynomials have S-polynomial") {
    // S(x - z^2, y - z^3) = - y z^2 + x z^3
    assert(S(
      p(m(1, v(x ^ 1)), m(-1, v(z ^ 2))),
      p(m(1, v(y ^ 1)), m(-1, v(z ^ 3)))
    ) == p(m(-1, v(y ^ 1, z ^ 2)), m(1, v(x ^ 1, z ^ 3))))
  }

  test("The remainder of S-polynomial divided by polynomials") {
    // S(x - z^2, y - z^3) = - y z^2 + x z^3
    assert(S_G(
      p(m(1, v(x ^ 1)), m(-1, v(z ^ 2))),
      p(m(1, v(y ^ 1)), m(-1, v(z ^ 3)))
    )(List(p(m(1, v(x ^ 1)), m(-1, v(z ^ 2))), p(m(1, v(y ^ 1)), m(-1, v(z ^ 3)))))
      == Polynomial.zero[String])
  }

  test("Groebner Basis") {
    // ReducedGroebnerBasis(x^3 + x y, x ^ 2 y - 2 y^2 + x) == { y^3, x - 2 y }
    assert(groebner(Set(
      p(m(1, v(x ^ 3)), m(-2, v(x ^ 1, y ^ 1))),
      p(m(1, v(x ^ 2, y ^ 1)), m(-2, v(y ^ 2)), m(1, v(x ^ 1)))
    ))(implicitly[scala.math.Ordering[String]], Syzygy).reduced
      == GroebnerBasis(Set(p(m(1, v(y ^ 3))), p(m(1, v(x ^ 1)), m(-2, v(y ^ 2)))))
    )
    // ReducedGroebnerBasis[Syzygy] == ReducedGroebnerBasis[Bucberger]
    assert(groebner(Set(
      p(m(1, v(x ^ 3)), m(-2, v(x ^ 1, y ^ 1))),
      p(m(1, v(x ^ 2, y ^ 1)), m(-2, v(y ^ 2)), m(1, v(x ^ 1)))
    ))(implicitly[scala.math.Ordering[String]], Buchberger).reduced
      == groebner(Set(
        p(m(1, v(x ^ 3)), m(-2, v(x ^ 1, y ^ 1))),
        p(m(1, v(x ^ 2, y ^ 1)), m(-2, v(y ^ 2)), m(1, v(x ^ 1)))
      ))(implicitly[scala.math.Ordering[String]], Syzygy).reduced
    )
    
    // MinimalGroebnerBasis(x^2 + y^2 + z^2 - 1, x y - z + 2, z^2 - 2 x + 3 y)
    // == { 1024 - 832 z - 215 z^2 + 156 z^3 - 25 z^4 + 24 z^5 + 13 z^6 + z^8,
    //      -11552 + 2560 y + 2197 z + 2764 z^2 + 443 z^3 + 728 z^4 + 169 z^5 + 32 z^6 + 13 z^7,
    //      -34656 + 5120 x 6591 z + 5732 z^2 + 1329 z^3 + 2184 z^4 + 507 z^5 + 96 z^6 + 39 z^7
    //    }
    // This test is passed, but it takes so long time.
    // assert(ReducedGroebnerBasis(Set(
    //   p(m(1, v(x ^ 2)), m(1, v(y ^ 2)), m(1, v(z ^ 2)), Monomial[String](-1)),
    //   p(m(1, v(x ^ 1, y ^ 1)), m(-1, v(z ^ 1)), Monomial[String](2)),
    //   p(m(1, v(z ^ 2)), m(-2, v(x ^ 1)), m(3, v(y ^ 1)))
    // ))
    //   == Set(:above:))
    // )

    assert(groebner(Set(
      p(m(1, v(x ^ 2)), m(1, v(y ^ 2)), m(1, v(z ^ 2)), Monomial[String](-1)),
      p(m(1, v(x ^ 1, y ^ 1)), m(-1, v(z ^ 1)), Monomial[String](2)),
      p(m(1, v(z ^ 2)), m(-2, v(x ^ 1)), m(3, v(y ^ 1)))
    ))(implicitly[scala.math.Ordering[String]], Buchberger).reduced
      == groebner(Set(
        p(m(1, v(x ^ 2)), m(1, v(y ^ 2)), m(1, v(z ^ 2)), Monomial[String](-1)),
        p(m(1, v(x ^ 1, y ^ 1)), m(-1, v(z ^ 1)), Monomial[String](2)),
        p(m(1, v(z ^ 2)), m(-2, v(x ^ 1)), m(3, v(y ^ 1)))
      ))(implicitly[scala.math.Ordering[String]], Syzygy).reduced
    )

    assert(groebner(Set(
      p(m(1, v(x ^ 1)), m(1, v(y ^ 1))),
      p(m(1, v(x ^ 2)), Monomial[String](-1)),
      p(m(1, v(y ^ 2)), m(-2, v(x ^ 1)))
    ))(implicitly[scala.math.Ordering[String]], Syzygy).reduced
      == GroebnerBasis(Set(p(Monomial[String](1))))
    )

    assert(groebner(Set(
      p(m(1, v(x ^ 1)), m(1, v(y ^ 1))),
      p(m(1, v(x ^ 2)), Monomial[String](-1)),
      p(m(1, v(y ^ 2)), m(-2, v(x ^ 1)))
    ))(implicitly[scala.math.Ordering[String]], Buchberger).reduced
      == groebner(Set(
        p(m(1, v(x ^ 1)), m(1, v(y ^ 1))),
        p(m(1, v(x ^ 2)), Monomial[String](-1)),
        p(m(1, v(y ^ 2)), m(-2, v(x ^ 1)))
      ))(implicitly[scala.math.Ordering[String]], Syzygy).reduced
    )
  }
}
