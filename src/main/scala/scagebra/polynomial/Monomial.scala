package scagebra
package polynomial

import Rational.Implicits._

case class Monomial[T](coefficient: Rational, variables: Variables[T])(implicit ord: Ordering[T], ordVar: Ordering[Variables[T]]) {

  lazy val reduce: Monomial[T] =
    if(coefficient == 0)
      Monomial(0, Variables.empty[T])
    else Monomial(coefficient, variables.filter { case (v, e) => e != 0 })

  private def variablesToString =
    reduce.variables.map { case (v, e) => if(e == 1) s"$v" else s"$v^$e" }mkString(" ")

  override def toString =
    if(reduce.variables.isEmpty)
      reduce.coefficient.toString
    else if(reduce.coefficient == 1)
      variablesToString
    else if(reduce.coefficient == -1)
      "(- " + variablesToString + ")"
    else if(reduce.coefficient > 0)
      s"${reduce.coefficient} $variablesToString"
    else if(reduce.coefficient < 0)
      s"(${reduce.coefficient}) $variablesToString"
    else "0"

  override def equals(other: Any): Boolean = other match {
    case that: Monomial[T] =>
      Monomial.MonomialIsFractional(ord, ordVar).compare(this, that) == 0
    case that: Int =>
      ((coefficient == 0) || variables.isEmpty) && this.coefficient == that
    case that: Long =>
      ((coefficient == 0) || variables.isEmpty) && this.coefficient == that
    case that: BigInt =>
      ((coefficient == 0) || variables.isEmpty) && this.coefficient == that
    case that: Float =>
      ((coefficient == 0) || variables.isEmpty) && this.coefficient == that
    case that: Double =>
      ((coefficient == 0) || variables.isEmpty) && this.coefficient == that
    case _ => false 
  }
}

object Monomial {

  def apply[T](m: (Variables[T], Rational))(implicit ord: Ordering[T]): Monomial[T] =
    this(m._2, m._1)

  def apply[T](c: Rational)(implicit ord: Ordering[T]): Monomial[T] =
    this(c, Variables.empty[T])

  trait ExtraImplicits {

    implicit def infixFractionalMonomialOps[T](x: Monomial[T])(implicit frac: Fractional[Monomial[T]]): Fractional[Monomial[T]]#FractionalOps = new frac.FractionalOps(x)

    implicit def infixOrderingMonomialOps[T](x: Monomial[T])(implicit ord: Ordering[Monomial[T]]): Ordering[Monomial[T]]#Ops = new ord.Ops(x)
  }

  object Implicits extends ExtraImplicits

  implicit def MonomialIsFractional[T](implicit ord: Ordering[T], ordVar: Ordering[Variables[T]]): Fractional[Monomial[T]] =
    new Fractional[Monomial[T]] {

      private def plusMinus(x: Monomial[T], y: Monomial[T])(f: (Rational, Rational) => Rational): Monomial[T] =
        if(x.variables == y.variables) {
          val nc = f(x.coefficient, y.coefficient)
          if(nc == 0)
            Monomial(0, Variables.empty)
          else
            x.copy(coefficient = nc)
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

      private def timesDiv(x: Monomial[T], y: Monomial[T])(f: (Rational, Rational) => (Rational), g: (Int, Int) => Int): Monomial[T] =
        Monomial(f(x.coefficient, y.coefficient), y.variables.foldLeft(x.variables) { (vs, v) =>
          val (yv, ye) = v
          vs.get(yv) match {
            case Some(xe) =>
              val e = g(xe, ye)
              if(e == 0)
                vs - yv
              else
                vs + (yv -> e)
            case None => vs + (yv -> g(0, ye))
          }
        })

      def times(x: Monomial[T], y: Monomial[T]): Monomial[T] =
        timesDiv(x, y)(_ * _, _ + _)
 
      def div(x: Monomial[T], y: Monomial[T]): Monomial[T] =
        timesDiv(x, y)(_ / _, _ - _)

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

