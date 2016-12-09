package scagebra
package polynomial

import org.scalatest._

import scala.collection.immutable.TreeMap

import Rational.Implicits._
import Term.Implicits._

class TermSpec extends FunSuite {

  val x = "x"
  val y = "y"
  val z = "z"

  test("Term has four arithmetic oparations") {
    assert(t(1, m(x ^ 1)) + t(2, m(x ^ 1)) == t(3, m(x ^ 1)))
    assert(t(1, m(x ^ 1)) + t(2, m(x ^ 2)) == t(1, m(x ^ 1)))
    assert(t(2, m(x ^ 2)) + t(1, m(x ^ 1)) == t(2, m(x ^ 2)))
    assert(t(0, m(x ^ 2)) == 0)
    assert(t(0, m(x ^ 2)) == 0.0)

    assert(t(1, m(x ^ 1)) - t(2, m(x ^ 1)) == t(-1, m(x ^ 1)))
    assert(t(1, m(x ^ 1)) - t(2, m(x ^ 2)) == t(1, m(x ^ 1)))
    assert(t(2, m(x ^ 2)) - t(1, m(x ^ 1)) == t(2, m(x ^ 2)))
    assert(t(2, m(x ^ 2)) - t(2, m(x ^ 2)) == t(0, m[String]()))
    
    assert(t(2, m(x ^ 2)) * t(1, m(x ^ 1)) == t(2, m(x ^ 3)))
    assert(t(2, m(x ^ 2)) * t(2, m(x ^ 1)) == t(4, m(x ^ 3)))
    assert(t(1, m(x ^ 1, y ^ 2)) * t(3, m(z ^ 3)) == t(3, m(x ^ 1, y ^ 2, z ^ 3)))
    assert(t(1, m(x ^ 1, y ^ 2)) * t(3, m(x ^ 2, z ^ 3)) == t(3, m(x ^ 3, y ^ 2, z ^ 3)))
    assert(t(2, m(x ^ 2)) * t(0, m(x ^ 1)) == t(0, m[String]()))

    assert(t(2, m(x ^ 2)) / t(1, m(x ^ 1)) == t(2, m(x ^ 1)))
    assert(t(2, m(x ^ 2)) / t(2, m(x ^ 1)) == t(1, m(x ^ 1)))
    assert(t(1, m(x ^ 1, y ^ 2)) / t(3, m(y ^ 2)) == t(Rational(1, 3), m(x ^ 1)))
    assert(t(1, m(x ^ 3, y ^ 2, z ^ 4)) / t(3, m(x ^ 2, z ^ 3)) == t(Rational(1, 3), m(x ^ 1, y ^ 2, z ^ 1)))
    assert(t(2, m(x ^ 2)) / t(1, m(x ^ 2)) == t(2, m[String]()))
  }

  test("Term can reduce") {
    assert(t(2, m(x ^ 1, y ^ 0)).reduce == t(2, m(x ^ 1)))
    assert(t(0, m(x ^ 1, y ^ 2, z ^ 0)).reduce == Term[String](0))
    assert(t(2, m(x ^ 0, y ^ 0)).reduce == Term[String](2))
  }

  test("Term has relational operators") {
    assert(t(1, m(x ^ 1)) < t(1, m(x ^ 2)))
    assert(t(1, m(x ^ 1, y ^ 1)) < t(1, m(x ^ 2)))
    assert(t(1, m(x ^ 1)) < t(1, m(x ^ 1, y ^ 1)))
    assert(t(1, m(x ^ 1)) < t(2, m(x ^ 1, y ^ 1)))
    assert(t(1, m(x ^ 1)) < t(2, m(x ^ 1)))

    assert(t(1, m(x ^ 1, y ^ 1)) > t(1, m(x ^ 1)))
    assert(t(1, m(x ^ 2, y ^ 1)) > t(1, m(x ^ 1)))
    assert(t(1, m(x ^ 1, y ^ 1)) > t(1, m(x ^ 1, z ^ 1)))

    assert(t(1, m(x ^ 1, y ^ 0)) == t(1, m(x ^ 1)))
    assert(t(1, m(x ^ 0, y ^ 0)) == t(1, m(z ^ 0)))
  }

  // TODO add test for compare
}
