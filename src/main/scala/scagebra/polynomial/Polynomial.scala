package scagebra
package polynomial

import scala.collection.immutable.TreeMap
import Rational.Implicits._
import Monomial.Implicits._

case class Polynomial[T](monomials: TreeMap[Variables[T], Rational])(implicit ord: Ordering[T], ordVar: Ordering[Variables[T]]) {

  override def equals(other: Any): Boolean = other match {
    case that: Polynomial[T] =>
      Polynomial.PolynomialIsNumeric(ord, ordVar).compare(this, that) == 0
    case _ => false // TODO Int, Long, BigInt, Double, Float
  }
}

object Polynomial {

  trait ExtraImplicits {

    implicit def infixNumericPolynomialOps[T](x: Polynomial[T])(implicit num: Numeric[Polynomial[T]]): Numeric[Polynomial[T]]#Ops = new num.Ops(x)
  }

  object Implicits extends ExtraImplicits

  implicit def PolynomialIsNumeric[T](implicit ord: Ordering[T], ordVar: Ordering[Variables[T]]): Numeric[Polynomial[T]] =
    new Numeric[Polynomial[T]] {

      private def plusMinus(x: Polynomial[T], y: Polynomial[T])(f: (Rational, Rational) => Rational): Polynomial[T] =
        Polynomial(y.monomials.foldLeft(x.monomials) { (ms, m) =>
          val (variables, coefficient) = m
          val update = ms.get(variables) match {
            case Some(coef) => variables -> f(coefficient, coef)
            case None => variables -> coefficient
          }
          ms + update
        })

      def plus(x: Polynomial[T], y: Polynomial[T]): Polynomial[T] =
        plusMinus(x, y)(_ + _)

      def minus(x: Polynomial[T], y: Polynomial[T]): Polynomial[T] =
        plusMinus(x, y)(_ - _)

      def negate(x: Polynomial[T]): Polynomial[T] =
        Polynomial(x.monomials.map { case (vars, coef) => vars -> -coef })

      def times(x: Polynomial[T], y: Polynomial[T]): Polynomial[T] =
        Polynomial(for {
          xm <- x.monomials
          ym <- y.monomials
          m1 = Monomial(xm)
          m2 = Monomial(ym)
          m = m1 * m2
        } yield m.variables -> m.coefficient)

      def toInt(x: Polynomial[T]): Int =
        if(x.monomials.forall(_._1.isEmpty))
          x.monomials.head._2.toInt
        else throw new IllegalStateException(s"$x cannot convert to Int.")

      def toLong(x: Polynomial[T]): Long =
        if(x.monomials.forall(_._1.isEmpty))
          x.monomials.head._2.toLong
        else throw new IllegalStateException(s"$x cannot convert to Long.")
        
      def toFloat(x: Polynomial[T]): Float =
        if(x.monomials.forall(_._1.isEmpty))
          x.monomials.head._2.toFloat
        else throw new IllegalStateException(s"$x cannot convert to Float.")

      def toDouble(x: Polynomial[T]): Double =
        if(x.monomials.forall(_._1.isEmpty))
          x.monomials.head._2.toDouble
        else throw new IllegalStateException(s"$x cannot convert to Double.")

      def fromInt(x: Int): Polynomial[T] =
        Polynomial(Monomial(x, Variables.empty[T]))

      def compare(x: Polynomial[T], y: Polynomial[T]) =
        (x.monomials zip y.monomials).dropWhile(t => t._1 == t._2).headOption match {
          case Some((xm, ym)) =>
            Monomial.MonomialIsNumeric(ord, ordVar).compare(Monomial(xm), Monomial(ym))
          case None =>
            if(x.monomials.size < y.monomials.size) -1
            else if(x.monomials.size > y.monomials.size) 1
            else 0
        }
    }

  def apply[T](monomials: Monomial[T]*)(implicit ord: Ordering[T]): Polynomial[T] =
    Polynomial(TreeMap(monomials.map(m => m.variables -> m.coefficient): _*))
}
