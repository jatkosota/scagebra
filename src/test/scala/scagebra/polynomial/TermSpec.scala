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
    assert(m(1, v(x ^ 1)) + m(2, v(x ^ 1)) == m(3, v(x ^ 1)))
    assert(m(1, v(x ^ 1)) + m(2, v(x ^ 2)) == m(1, v(x ^ 1)))
    assert(m(2, v(x ^ 2)) + m(1, v(x ^ 1)) == m(2, v(x ^ 2)))
    assert(m(0, v(x ^ 2)) == 0)
    assert(m(0, v(x ^ 2)) == 0.0)

    assert(m(1, v(x ^ 1)) - m(2, v(x ^ 1)) == m(-1, v(x ^ 1)))
    assert(m(1, v(x ^ 1)) - m(2, v(x ^ 2)) == m(1, v(x ^ 1)))
    assert(m(2, v(x ^ 2)) - m(1, v(x ^ 1)) == m(2, v(x ^ 2)))
    assert(m(2, v(x ^ 2)) - m(2, v(x ^ 2)) == m(0, v[String]()))
    
    assert(m(2, v(x ^ 2)) * m(1, v(x ^ 1)) == m(2, v(x ^ 3)))
    assert(m(2, v(x ^ 2)) * m(2, v(x ^ 1)) == m(4, v(x ^ 3)))
    assert(m(1, v(x ^ 1, y ^ 2)) * m(3, v(z ^ 3)) == m(3, v(x ^ 1, y ^ 2, z ^ 3)))
    assert(m(1, v(x ^ 1, y ^ 2)) * m(3, v(x ^ 2, z ^ 3)) == m(3, v(x ^ 3, y ^ 2, z ^ 3)))
    assert(m(2, v(x ^ 2)) * m(0, v(x ^ 1)) == m(0, v[String]()))

    assert(m(2, v(x ^ 2)) / m(1, v(x ^ 1)) == m(2, v(x ^ 1)))
    assert(m(2, v(x ^ 2)) / m(2, v(x ^ 1)) == m(1, v(x ^ 1)))
    assert(m(1, v(x ^ 1, y ^ 2)) / m(3, v(y ^ 2)) == m(Rational(1, 3), v(x ^ 1)))
    assert(m(1, v(x ^ 3, y ^ 2, z ^ 4)) / m(3, v(x ^ 2, z ^ 3)) == m(Rational(1, 3), v(x ^ 1, y ^ 2, z ^ 1)))
    assert(m(2, v(x ^ 2)) / m(1, v(x ^ 2)) == m(2, v[String]()))
  }

  test("Term can reduce") {
    assert(m(2, v(x ^ 1, y ^ 0)).reduce == m(2, v(x ^ 1)))
    assert(m(0, v(x ^ 1, y ^ 2, z ^ 0)).reduce == Term[String](0))
    assert(m(2, v(x ^ 0, y ^ 0)).reduce == Term[String](2))
  }

  test("Term has relational operators") {
    assert(m(1, v(x ^ 1)) < m(1, v(x ^ 2)))
    assert(m(1, v(x ^ 1, y ^ 1)) < m(1, v(x ^ 2)))
    assert(m(1, v(x ^ 1)) < m(1, v(x ^ 1, y ^ 1)))
    assert(m(1, v(x ^ 1)) < m(2, v(x ^ 1, y ^ 1)))
    assert(m(1, v(x ^ 1)) < m(2, v(x ^ 1)))

    assert(m(1, v(x ^ 1, y ^ 1)) > m(1, v(x ^ 1)))
    assert(m(1, v(x ^ 2, y ^ 1)) > m(1, v(x ^ 1)))
    assert(m(1, v(x ^ 1, y ^ 1)) > m(1, v(x ^ 1, z ^ 1)))

    assert(m(1, v(x ^ 1, y ^ 0)) == m(1, v(x ^ 1)))
    assert(m(1, v(x ^ 0, y ^ 0)) == m(1, v(z ^ 0)))
  }

  // TODO add test for compare
}
