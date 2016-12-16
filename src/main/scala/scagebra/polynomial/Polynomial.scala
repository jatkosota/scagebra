package scagebra
package polynomial

import scala.collection.immutable.TreeMap
import Rational.Implicits._
import Term.Implicits._

case class Polynomial[T](terms: TreeMap[Monomial[T], Rational])(implicit ord: Ordering[T], ordMon: Ordering[Monomial[T]]) {

  lazy val reduce: Polynomial[T] =
    Polynomial(terms.filter { case (vs, r) => r != 0 })

  /** Substitutes the rational value for the variable.
    * This method return Rational. If there is any variables cannot be substituted by the Map which given by a argument, throws exception which is NoSuchElementException.
    * Note: for all variables should be substituted.
    */
  def substituteAll(sub: Map[T, Rational]): Rational =
    terms.map {
      case (monomial, coefficient) =>
        coefficient * (monomial.map {
          case (variable, e) =>
            Rational.pow(sub(variable), e)
        }).foldLeft(Rational(1))(_ * _)
    }.sum

  /** Substitutes the rational value for the variable.
    * This method return Polynomial.
    */
  def substitute(sub: Map[T, Rational]): Polynomial[T] =
    terms.toSeq.map {
      case (monomial, coefficient) =>
        val (rsR, monsL) = monomial.map {
          case (variable, e) =>
            sub.get(variable) match {
              case Some(v) =>
                Right(Rational.pow(sub(variable), e))
              case None =>
                Left((variable, e))
            }
        }.partition(_.isRight)
        val rs = rsR.collect { case Right(v) => v }
        val mons = monsL.collect { case Left(v) => v }
        Polynomial(
          Term(coefficient * rs.foldLeft(Rational(1))(_ * _),
            TreeMap(mons.toSeq: _*))(ord, ordMon)
        )
    }.sum

  def pow(e: Int): Polynomial[T] = {
    import Polynomial.Implicits._

    @scala.annotation.tailrec
    def loop(e: Int, acc: Polynomial[T]): Polynomial[T] =
      if(e <= 1)
        acc
      else {
        val ps = for {
          term <- acc.terms
          source = Polynomial(TreeMap(term))
        } yield source * this
        loop(e - 1, ps.sum)
      }
    loop(e, this)
  }

  /** Return this polynomial is const or not. */
  lazy val isConst: Boolean =
    reduce.terms.isEmpty ||
      (reduce.terms.size == 1 && reduce.terms.head._1.isEmpty)

  /** Convert to Rational. 
    * If this polynomial is const, returns rational wrapped Option.
    * If not returns None.
    */
  def toRational: Option[Rational] =
    if(isConst)
      if(reduce.terms.isEmpty)
        Some(Rational(0))
      else
        reduce.terms.headOption.map(_._2)
    else
      None

  // TODO canEqual

  override def toString =
    if(terms.isEmpty)
      "0"
    else 
      terms.toList.map(Term(_)).mkString(" + ")

  override def equals(other: Any): Boolean = other match {
    case that: Polynomial[T] =>
      Polynomial.PolynomialIsNumeric(ord, ordMon).compare(this, that) == 0
    case that: Int =>
      (this.reduce.terms.size == 1 && Term(this.reduce.terms.head) == that) ||
        (this.reduce.terms.isEmpty && that == 0)
    case _ => false // TODO Int, Long, BigInt, Double, Float
  }
}

object Polynomial {

  implicit def TermToPolynomial[T](monomial: Term[T])(implicit ord: Ordering[T]): Polynomial[T] =
    Polynomial(monomial)

  trait ExtraImplicits {

    implicit def infixNumericPolynomialOps[T](x: Polynomial[T])(implicit num: Numeric[Polynomial[T]]): Numeric[Polynomial[T]]#Ops = new num.Ops(x)
  }

  object Implicits extends ExtraImplicits

  implicit def PolynomialIsNumeric[T](implicit ord: Ordering[T], ordMon: Ordering[Monomial[T]]): Numeric[Polynomial[T]] =
    new Numeric[Polynomial[T]] {

      private def plusMinus(x: Polynomial[T], y: Polynomial[T])(f: (Rational, Rational) => Rational): Polynomial[T] =
        Polynomial(y.terms.foldLeft(x.terms) { (ms, m) =>
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
        Polynomial(x.terms.map { case (vars, coef) => vars -> -coef })

      def times(x: Polynomial[T], y: Polynomial[T]): Polynomial[T] = {
        val ms = for {
          xm <- x.terms.iterator
          ym <- y.terms.iterator
          m1 = Term(xm)
          m2 = Term(ym)
          m = m1 * m2
        } yield m.monomial -> m.coefficient
        ms.foldLeft(Polynomial[T]())((p, m)=> p + Polynomial(Term(m)))
      }

      def toInt(x: Polynomial[T]): Int =
        if(x.terms.isEmpty) 0
        else if(x.terms.forall(_._1.isEmpty))
          x.terms.head._2.toInt
        else throw new IllegalStateException(s"$x cannot convert to Int.")

      def toLong(x: Polynomial[T]): Long =
        if(x.terms.isEmpty) 0
        else if(x.terms.forall(_._1.isEmpty))
          x.terms.head._2.toLong
        else throw new IllegalStateException(s"$x cannot convert to Long.")
        
      def toFloat(x: Polynomial[T]): Float =
        if(x.terms.isEmpty) 0
        else if(x.terms.forall(_._1.isEmpty))
          x.terms.head._2.toFloat
        else throw new IllegalStateException(s"$x cannot convert to Float.")

      def toDouble(x: Polynomial[T]): Double =
        if(x.terms.isEmpty) 0
        else if(x.terms.forall(_._1.isEmpty))
          x.terms.head._2.toDouble
        else throw new IllegalStateException(s"$x cannot convert to Double.")

      def fromInt(x: Int): Polynomial[T] =
        Polynomial(Term(x, Monomial.empty[T]))

      def compare(x: Polynomial[T], y: Polynomial[T]) = {
        val xrm = x.reduce.terms
        val yrm = y.reduce.terms
        (xrm zip yrm).dropWhile(t => t._1 == t._2).headOption match {
          case Some((xm, ym)) =>
            Term.TermIsFractional(ord, ordMon).compare(Term(xm), Term(ym))
          case None =>
            if(xrm.size < yrm.size) -1
            else if(xrm.size > yrm.size) 1
            else 0
        }
      }
    }

  def apply[T](terms: Term[T]*)(implicit ord: Ordering[T]): Polynomial[T] =
    Polynomial(TreeMap(terms.map(m => m.monomial -> m.coefficient): _*))

  def empty[T](implicit ord: Ordering[T]): Polynomial[T] =
    apply()

  def zero[T](implicit ord: Ordering[T]): Polynomial[T] =
    empty[T]
}
