package scagebra
package polynomial

import org.scalatest._

import scala.collection.immutable.TreeMap

import Rational.Implicits._
import Monomial.Implicits._

class MonomialSpec extends FunSuite {

  val x = "x"
  val y = "y"
  val z = "z"

  test("Monomial has three arithmetic oparations") {
    assert(m(1, v(x ^ 1)) + m(2, v(x ^ 1)) == m(3, v(x ^ 1)))
    assert(m(1, v(x ^ 1)) + m(2, v(x ^ 2)) == m(1, v(x ^ 1)))
    assert(m(2, v(x ^ 2)) + m(1, v(x ^ 1)) == m(2, v(x ^ 2)))

    assert(m(1, v(x ^ 1)) - m(2, v(x ^ 1)) == m(-1, v(x ^ 1)))
    assert(m(1, v(x ^ 1)) - m(2, v(x ^ 2)) == m(1, v(x ^ 1)))
    assert(m(2, v(x ^ 2)) - m(1, v(x ^ 1)) == m(2, v(x ^ 2)))
    assert(m(2, v(x ^ 2)) - m(2, v(x ^ 2)) == m(0, v[String]()))
    
    assert(m(2, v(x ^ 2)) * m(1, v(x ^ 1)) == m(2, v(x ^ 3)))
    assert(m(2, v(x ^ 2)) * m(2, v(x ^ 1)) == m(4, v(x ^ 3)))
    assert(m(1, v(x ^ 1, y ^ 2)) * m(3, v(z ^ 3)) == m(3, v(x ^ 1, y ^ 2, z ^ 3)))
    assert(m(1, v(x ^ 1, y ^ 2)) * m(3, v(x ^ 2, z ^ 3)) == m(3, v(x ^ 3, y ^ 2, z ^ 3)))
    assert(m(2, v(x ^ 2)) * m(0, v(x ^ 1)) == m(0, v[String]()))
  }

  // TODO add test for compare
}
