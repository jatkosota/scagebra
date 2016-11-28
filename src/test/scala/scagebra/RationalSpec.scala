package scagebra

import org.scalatest._

import RationalImplicits._

class RationalSpec extends FunSuite {

  test("Rational can reduce themselves") {
    assert(Rational(2, 4).reduce.numer == 1)
    assert(Rational(2, 4).reduce.denom == 2)
    assert(Rational(4, 2).reduce == Rational(2, 1))
    assert(Rational(-2, 4).reduce == Rational(-1, 2))
    assert(Rational(2, -4).reduce == Rational(-1, 2))
  }

  // Fractional

  test("Rational has reciprocal") {
    assert(Rational(1, 2).reciprocal == Rational(2))
    assert(Rational(2).reciprocal == Rational(1, 2))
    assert(Rational(1, -3).reciprocal == Rational(-3))
    assert(Rational(2, 4).reciprocal.numer == 4)
    assert(Rational(2, 4).reciprocal.denom == 2)
  }

  test("Rational has equality operator") {
    assert(Rational(2) == Rational(2))
    assert(Rational(4, 2) == Rational(2))
    assert(Rational(1, -2) == Rational(1, -2))
    assert(Rational(1, -2) == Rational(-1, 2))
    assert(Rational(-1, 2) == Rational(-1, 2))
    assert(Rational(1) == 1)
    assert(Rational(0) == 0)
    assert(Rational(0, -4) == 0)
    assert(Rational(1, 2) == 0.5)
    assert(Rational(3, 9) == (1/3.0))
  }

  test("Rational has relatinal operators") {
    assert(Rational(2) < Rational(4))
    assert(Rational(2) < Rational(2) == false)
    assert(Rational(2, 3) > Rational(1, 3))
    assert(Rational(2, 3) < Rational(4, 3))
    assert(Rational(-2, 3) < 0)
    assert(Rational(-1, 3) > Rational(-2, 3))
    assert(Rational(-2, -3) > 0)
    assert(Rational(1, -3) < 0)
  }

  test("Rational has negate") {
    assert(-Rational(2) == Rational(-2))
    assert(-(-Rational(2)) == Rational(2))
  }

  test("Rational has four arithmetic operations") {
    assert(Rational(2, 3) + Rational(3, 2) == Rational(13, 6))
    assert(Rational(2, 3) - Rational(3, 2) == -Rational(5, 6))
    assert(Rational(2, 3) * Rational(3, 2) == Rational(1))
    assert(Rational(2, 3) * Rational(6, 4) == Rational(1))
    assert(Rational(2, 3) * Rational(-3, 2) == -Rational(1))
    assert(Rational(2, 3) / Rational(3, 2) == Rational(4, 9))
    assert(Rational(2, 3) / Rational(-3, 2) == -Rational(4, 9))
  }
}
