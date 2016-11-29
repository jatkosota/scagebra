package scagebra
package polynomial

import Rational.Implicits._

case class Monomial[T](coefficient: Rational, variables: Variables[T])(implicit ord: Ordering[T], ordVar: Ordering[Variables[T]]) {

  override def equals(other: Any): Boolean = other match {
    case that: Monomial[T] =>
      Monomial.MonomialIsNumeric(ord, ordVar).compare(this, that) == 0
    case _ => false // TODO Int, Long, BigInt, Double, Float
  }
}

object Monomial {

  def apply[T](m: (Variables[T], Rational))(implicit ord: Ordering[T]): Monomial[T] =
    this(m._2, m._1)

  trait ExtraImplicits {

    implicit def infixNumericMonomialOps[T](x: Monomial[T])(implicit num: Numeric[Monomial[T]]): Numeric[Monomial[T]]#Ops = new num.Ops(x)
  }

  object Implicits extends ExtraImplicits

  implicit def MonomialIsNumeric[T](implicit ord: Ordering[T], ordVar: Ordering[Variables[T]]): Numeric[Monomial[T]] =
    new Numeric[Monomial[T]] {

      private def plusMinus(x: Monomial[T], y: Monomial[T])(f: (Rational, Rational) => Rational): Monomial[T] =
        if(x.variables == y.variables) {
          val newCoefficient = f(x.coefficient, y.coefficient)
          if(newCoefficient == 0)
            Monomial(0, Variables.empty)
          else
            x.copy(coefficient = newCoefficient)
        } else x

      /** Plus on monomials. This is counterintuitive.
        * If 'variables' which is the part of monomial except the coefficient is different, returns self.
        * If not, returns a [[scagebra.polynomial.Monomial]] whose coefficient is the sum of `x`'s coefficient and `y`'s coefficient.
        * Notice, this doesn't satisfy the commutative property, so `x plus y == y plus x` isn't always satisfied.
        */
      def plus(x: Monomial[T], y: Monomial[T]): Monomial[T] =
        plusMinus(x, y)(_ + _)

      def minus(x: Monomial[T], y: Monomial[T]): Monomial[T] =
        plusMinus(x, y)(_ - _)

      def negate(x: Monomial[T]): Monomial[T] =
        x.copy(coefficient = -x.coefficient)

      def times(x: Monomial[T], y: Monomial[T]): Monomial[T] =
        Monomial(x.coefficient*y.coefficient, y.variables.foldLeft(x.variables) { (vs, v) =>
          val (variable, e1) = v
          vs.get(v._1) match {
            case Some(e2) =>
              val newE = e1 + e2
              if(newE == 0)
                vs - variable
              else
                vs + (variable -> newE)
            case None    => vs + v
          }
        })

      def toInt(x: Monomial[T]): Int =
        if(x.variables.isEmpty) x.coefficient.toInt
        else throw new IllegalStateException(s"$x cannot convert to Int")

      def toFloat(x: Monomial[T]): Float =
        if(x.variables.isEmpty) x.coefficient.toFloat
        else throw new IllegalStateException(s"$x cannot convert to Float")

      def toDouble(x: Monomial[T]): Double =
        if(x.variables.isEmpty) x.coefficient.toDouble
        else throw new IllegalStateException(s"$x cannot convert to Double")

      def toLong(x: Monomial[T]): Long =
        if(x.variables.isEmpty) x.coefficient.toLong
        else throw new IllegalStateException(s"$x cannot convert to Long")

      def fromInt(x: Int): Monomial[T] =
        Monomial(x, Variables.empty[T])

      def compare(x: Monomial[T], y: Monomial[T]): Int =
        if(x.variables == y.variables) {
          if(x.coefficient < y.coefficient) -1
          else if(x.coefficient > y.coefficient) 1
          else 0
        } else if(x.coefficient == 0 && y.coefficient == 0) 0
        else ordVar.compare(x.variables, y.variables)
    }
}

