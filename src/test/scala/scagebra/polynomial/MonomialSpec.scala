package scagebra
package polynomial

import org.scalatest._

import scala.collection.immutable.TreeMap

import RationalImplicits._
import MonomialImplicits._
import scala.math.Numeric.Implicits._

class MonomialSpec extends FunSuite {

  test("Monomial has three arithmetic oparations") {
    val x = "x"
    val y = "y"
    val z = "z"
    assert(m(1, v(x ^ 1)) + m(2, v(x ^ 1)) == m(3, v(x ^ 1)))
    assert(m(1, v(x ^ 1)) + m(2, v(x ^ 2)) == m(1, v(x ^ 1)))
    assert(m(2, v(x ^ 2)) + m(1, v(x ^ 1)) == m(2, v(x ^ 2)))

    assert(m(1, v(x ^ 1)) - m(2, v(x ^ 1)) == m(-1, v(x ^ 1)))
    assert(m(1, v(x ^ 1)) - m(2, v(x ^ 2)) == m(1, v(x ^ 1)))
    assert(m(2, v(x ^ 2)) - m(1, v(x ^ 1)) == m(2, v(x ^ 2)))
    
    assert(m(2, v(x ^ 2)) * m(1, v(x ^ 1)) == m(2, v(x ^ 3)))
    assert(m(2, v(x ^ 2)) * m(2, v(x ^ 1)) == m(4, v(x ^ 3)))
    assert(m(1, v(x ^ 1, y ^ 2)) * m(3, v(z ^ 3)) == m(3, v(x ^ 1, y ^ 2, z ^ 3)))
    assert(m(1, v(x ^ 1, y ^ 2)) * m(3, v(x ^ 2, z ^ 3)) == m(3, v(x ^ 3, y ^ 2, z ^ 3)))
  }

  // TODO add test for compare
}
