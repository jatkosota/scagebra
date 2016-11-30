package scagebra
package polynomial

import scala.collection.immutable.TreeMap
import Rational.Implicits._
import Monomial.Implicits._

case class Polynomial[T](monomials: TreeMap[Variables[T], Rational])(implicit ord: Ordering[T], ordVar: Ordering[Variables[T]]) {

  def reduce: Polynomial[T] =
    Polynomial(monomials.filter { case (vs, r) => r != 0 })

  // TODO canEqual

  override def toString =
    monomials.toList.map(Monomial(_)).mkString(" + ")

  override def equals(other: Any): Boolean = other match {
    case that: Polynomial[T] =>
      Polynomial.PolynomialIsNumeric(ord, ordVar).compare(this, that) == 0
    case that: Int =>
      (this.reduce.monomials.size == 1 && Monomial(this.reduce.monomials.head) == that) ||
        (this.reduce.monomials.isEmpty && that == 0)
    case _ => false // TODO Int, Long, BigInt, Double, Float
  }
}

object Polynomial {

  implicit def MonomialToPolynomial[T](monomial: Monomial[T])(implicit ord: Ordering[T]): Polynomial[T] =
    Polynomial(monomial)

  trait ExtraImplicits {

    implicit def infixNumericPolynomialOps[T](x: Polynomial[T])(implicit num: Numeric[Polynomial[T]]): Numeric[Polynomial[T]]#Ops = new num.Ops(x)
  }

  object Implicits extends ExtraImplicits

  implicit def PolynomialIsNumeric[T](implicit ord: Ordering[T], ordVar: Ordering[Variables[T]]): Numeric[Polynomial[T]] =
    new Numeric[Polynomial[T]] {

      private def plusMinus(x: Polynomial[T], y: Polynomial[T])(f: (Rational, Rational) => Rational): Polynomial[T] =
        Polynomial(y.monomials.foldLeft(x.monomials) { (ms, m) =>
          val (yvs, yc) = m
          ms.get(yvs) match {
            case Some(xc) =>
              val nc = f(xc, yc)
              if(nc == 0) ms - yvs
              else        ms + (yvs -> nc)
            case None =>
              if(yc == 0) ms
              else        ms + (yvs -> f(0, yc))
          }
        })

      def plus(x: Polynomial[T], y: Polynomial[T]): Polynomial[T] =
        plusMinus(x, y)(_ + _)

      def minus(x: Polynomial[T], y: Polynomial[T]): Polynomial[T] =
        plusMinus(x, y)(_ - _)

      def negate(x: Polynomial[T]): Polynomial[T] =
        Polynomial(x.monomials.map { case (vars, coef) => vars -> -coef })

      def times(x: Polynomial[T], y: Polynomial[T]): Polynomial[T] = {
        val ms = for {
          xm <- x.monomials.iterator
          ym <- y.monomials.iterator
          m1 = Monomial(xm)
          m2 = Monomial(ym)
          m = m1 * m2
        } yield m.variables -> m.coefficient
        ms.foldLeft(Polynomial[T]())((p, m)=> p + Polynomial(Monomial(m)))
      }

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

      def compare(x: Polynomial[T], y: Polynomial[T]) = {
        val xrm = x.reduce.monomials
        val yrm = y.reduce.monomials
        (xrm zip yrm).dropWhile(t => t._1 == t._2).headOption match {
          case Some((xm, ym)) =>
            Monomial.MonomialIsFractional(ord, ordVar).compare(Monomial(xm), Monomial(ym))
          case None =>
            if(xrm.size < yrm.size) -1
            else if(xrm.size > yrm.size) 1
            else 0
        }
      }
    }

  def apply[T](monomials: Monomial[T]*)(implicit ord: Ordering[T]): Polynomial[T] =
    Polynomial(TreeMap(monomials.map(m => m.variables -> m.coefficient): _*))

  def empty[T](implicit ord: Ordering[T]): Polynomial[T] =
    apply()

  def zero[T](implicit ord: Ordering[T]): Polynomial[T] =
    empty[T]
}
