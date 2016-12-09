package scagebra
package polynomial

import org.scalatest._

import Rational.Implicits._
import Term.Implicits._
import Polynomial.Implicits._
import Groebner._

class GroebnerSpec extends FunSuite {

  val x = "x"
  val y = "y"
  val z = "z"

  test("Polynomial has LT") {
    // LT(x^1) == x^1
    assert(
      p(t(1, m(x ^ 1))).LT == t(1, m(x ^ 1))
    )
    // LT(2 x + x^2) == x^2
    assert(
      p(t(2, m(x ^ 1)), t(1, m(x ^ 2))).LT
        == t(1, m(x ^ 2))
    )
    // LT(2 x y^2 - 2 x y) == 2 x y^2
    assert(
      p(t(2, m(x ^ 1, y ^ 2)), t(-2, m(x ^ 1, y ^ 1))).LT
        == t(2, m(x ^ 1, y ^ 2))
    )
    // LT(2 x + y z) == 2 x
    assert(
      p(t(2, m(x ^ 1)), t(1, m(y ^ 1, z ^ 1))).LT
        == t(2, m(x ^ 1))
    )
    // LT(2 y + y z) = y z
    assert(
      p(t(2, m(y ^ 1)), t(1, m(y ^ 1, z ^ 1))).LT
        == t(1, m(y ^ 1, z ^ 1))
    )
  }

  test("Polynomial can be divided by polynomials") {
    // x y^2 + 1 / [ x y + 1, y + 1 ] == [ y, -1 ]
    assert(
      p(t(1, m(x ^ 1, y ^ 2)), Term[String](1)) / List(
        p(t(1, m(x ^ 1, y ^ 1)), Term[String](1)),
        p(t(1, m(y ^ 1)), Term[String](1))
      )
        === List(p(t(1, m(y ^ 1))), p(Term[String](-1)))
    )
    // x y^2 + 1 % [ x y + 1, y + 1 ] == 2
    assert(
      p(t(1, m(x ^ 1, y ^ 2)), Term[String](1)) % List(
        p(t(1, m(x ^ 1, y ^ 1)), Term[String](1)),
        p(t(1, m(y ^ 1)), Term[String](1))
      )
        === p(Term[String](2))
    )

    // x^2 y + x y^2 + y^2 / [ x y - 1, y^2 - 1 ] == [ x + y, 1 ]
    assert(
      p(t(1, m(x ^ 2, y ^ 1)), t(1, m(x ^ 1, y ^ 2)), t(1, m(y ^ 2))) / List(
        p(t(1, m(x ^ 1, y ^ 1)), Term[String](-1)),
        p(t(1, m(y ^ 2)), Term[String](-1))
      )
        === List(p(t(1, m(x ^ 1)), t(1, m(y ^ 1))), p(Term[String](1)))
    )

    // x^2 y + x y^2 + y^2 % [ x y - 1, y^2 - 1 ] == x + y + 1
    assert(
      p(t(1, m(x ^ 2, y ^ 1)), t(1, m(x ^ 1, y ^ 2)), t(1, m(y ^ 2))) % List(
        p(t(1, m(x ^ 1, y ^ 1)), Term[String](-1)),
        p(t(1, m(y ^ 2)), Term[String](-1))
      )
        === p(t(1, m(x ^ 1)), t(1, m(y ^ 1)), Term[String](1))
    )

    // 3 x^3 + x^2 y - x + 2 / [ 2 x^2 + y, x y - x ] == [ (3/2) x + (1/2) y, -(3/2) ]
    assert(
      p(t(3, m(x ^ 3)), t(1, m(x ^ 2, y ^ 1)), t(-1, m(x ^ 1)), Term[String](2)) / List(
        p(t(2, m(x ^ 2)), t(1, m(y ^ 1))),
        p(t(1, m(x ^ 1, y ^ 1)), t(-1, m(x ^ 1)))
      )
        === List(
          p(t(Rational(3, 2), m(x ^ 1)), t(Rational(1, 2), m(y ^ 1))),
          p(t(-Rational(3, 2), m[String]()))
        )
    )

    // 3 x^3 + x^2 y - x + 2 % [ 2 x^2 + y, x y - x ] == -(5/2) x - (1/2) y^2 + 2
    assert(
      p(t(3, m(x ^ 3)), t(1, m(x ^ 2, y ^ 1)), t(-1, m(x ^ 1)), Term[String](2)) % List(
        p(t(2, m(x ^ 2)), t(1, m(y ^ 1))),
        p(t(1, m(x ^ 1, y ^ 1)), t(-1, m(x ^ 1)))
      )
        === p(t(-Rational(5, 2), m(x ^ 1)), t(-Rational(1, 2), m(y ^ 2)), Term[String](2))
    )
  }

  test("Two polynomials have LCM") {
    // LCM(x^3 y^2 - x^2 y^3 + x, 3 x^4 y + y^2)
    assert(LCM(
      p(t(1, m(x ^ 3, y ^ 2)), t(-1, m(x ^ 2, y ^ 3)), t(1, m(x ^ 1))),
      p(t(3, m(x ^ 4, y ^ 1)), t(1, m(y ^ 2)))
    ) == t(1, m(x ^ 4, y ^ 2)))
    // TODO add more tests
  }

  test("Two polynomials have S-polynomial") {
    // S(x - z^2, y - z^3) = - y z^2 + x z^3
    assert(S(
      p(t(1, m(x ^ 1)), t(-1, m(z ^ 2))),
      p(t(1, m(y ^ 1)), t(-1, m(z ^ 3)))
    ) == p(t(-1, m(y ^ 1, z ^ 2)), t(1, m(x ^ 1, z ^ 3))))
  }

  test("The remainder of S-polynomial dimided by polynomials") {
    // S(x - z^2, y - z^3) = - y z^2 + x z^3
    assert(S_G(
      p(t(1, m(x ^ 1)), t(-1, m(z ^ 2))),
      p(t(1, m(y ^ 1)), t(-1, m(z ^ 3)))
    )(List(p(t(1, m(x ^ 1)), t(-1, m(z ^ 2))), p(t(1, m(y ^ 1)), t(-1, m(z ^ 3)))))
      == Polynomial.zero[String])
  }

  test("Groebner Basis") {
    // ReducedGroebnerBasis(x^3 + x y, x ^ 2 y - 2 y^2 + x) == { y^3, x - 2 y }
    assert(groebner(Set(
      p(t(1, m(x ^ 3)), t(-2, m(x ^ 1, y ^ 1))),
      p(t(1, m(x ^ 2, y ^ 1)), t(-2, m(y ^ 2)), t(1, m(x ^ 1)))
    ))(implicitly[scala.math.Ordering[String]], Syzygy).reduced
      == GroebnerBasis(Set(p(t(1, m(y ^ 3))), p(t(1, m(x ^ 1)), t(-2, m(y ^ 2)))))
    )
    // ReducedGroebnerBasis[Syzygy] == ReducedGroebnerBasis[Bucberger]
    assert(groebner(Set(
      p(t(1, m(x ^ 3)), t(-2, m(x ^ 1, y ^ 1))),
      p(t(1, m(x ^ 2, y ^ 1)), t(-2, m(y ^ 2)), t(1, m(x ^ 1)))
    ))(implicitly[scala.math.Ordering[String]], Buchberger).reduced
      == groebner(Set(
        p(t(1, m(x ^ 3)), t(-2, m(x ^ 1, y ^ 1))),
        p(t(1, m(x ^ 2, y ^ 1)), t(-2, m(y ^ 2)), t(1, m(x ^ 1)))
      ))(implicitly[scala.math.Ordering[String]], Syzygy).reduced
    )
    
    // MinimalGroebnerBasis(x^2 + y^2 + z^2 - 1, x y - z + 2, z^2 - 2 x + 3 y)
    // == { 1024 - 832 z - 215 z^2 + 156 z^3 - 25 z^4 + 24 z^5 + 13 z^6 + z^8,
    //      -11552 + 2560 y + 2197 z + 2764 z^2 + 443 z^3 + 728 z^4 + 169 z^5 + 32 z^6 + 13 z^7,
    //      -34656 + 5120 x 6591 z + 5732 z^2 + 1329 z^3 + 2184 z^4 + 507 z^5 + 96 z^6 + 39 z^7
    //    }
    // This test is passed, but it takes so long time.
    // assert(ReducedGroebnerBasis(Set(
    //   p(t(1, m(x ^ 2)), t(1, m(y ^ 2)), t(1, m(z ^ 2)), Term[String](-1)),
    //   p(t(1, m(x ^ 1, y ^ 1)), t(-1, m(z ^ 1)), Term[String](2)),
    //   p(t(1, m(z ^ 2)), t(-2, m(x ^ 1)), t(3, m(y ^ 1)))
    // ))
    //   == Set(:abome:))
    // )

    assert(groebner(Set(
      p(t(1, m(x ^ 2)), t(1, m(y ^ 2)), t(1, m(z ^ 2)), Term[String](-1)),
      p(t(1, m(x ^ 1, y ^ 1)), t(-1, m(z ^ 1)), Term[String](2)),
      p(t(1, m(z ^ 2)), t(-2, m(x ^ 1)), t(3, m(y ^ 1)))
    ))(implicitly[scala.math.Ordering[String]], Buchberger).reduced
      == groebner(Set(
        p(t(1, m(x ^ 2)), t(1, m(y ^ 2)), t(1, m(z ^ 2)), Term[String](-1)),
        p(t(1, m(x ^ 1, y ^ 1)), t(-1, m(z ^ 1)), Term[String](2)),
        p(t(1, m(z ^ 2)), t(-2, m(x ^ 1)), t(3, m(y ^ 1)))
      ))(implicitly[scala.math.Ordering[String]], Syzygy).reduced
    )

    assert(groebner(Set(
      p(t(1, m(x ^ 1)), t(1, m(y ^ 1))),
      p(t(1, m(x ^ 2)), Term[String](-1)),
      p(t(1, m(y ^ 2)), t(-2, m(x ^ 1)))
    ))(implicitly[scala.math.Ordering[String]], Syzygy).reduced
      == GroebnerBasis(Set(p(Term[String](1))))
    )

    assert(groebner(Set(
      p(t(1, m(x ^ 1)), t(1, m(y ^ 1))),
      p(t(1, m(x ^ 2)), Term[String](-1)),
      p(t(1, m(y ^ 2)), t(-2, m(x ^ 1)))
    ))(implicitly[scala.math.Ordering[String]], Buchberger).reduced
      == groebner(Set(
        p(t(1, m(x ^ 1)), t(1, m(y ^ 1))),
        p(t(1, m(x ^ 2)), Term[String](-1)),
        p(t(1, m(y ^ 2)), t(-2, m(x ^ 1)))
      ))(implicitly[scala.math.Ordering[String]], Syzygy).reduced
    )
  }
}
