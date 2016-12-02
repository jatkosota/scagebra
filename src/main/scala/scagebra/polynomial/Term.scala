package scagebra
package polynomial

import Rational.Implicits._

case class Term[T](coefficient: Rational, variables: Monomial[T])(implicit ord: Ordering[T], ordVar: Ordering[Monomial[T]]) {

  lazy val reduce: Term[T] =
    if(coefficient == 0)
      Term(0, Monomial.empty[T])
    else Term(coefficient, variables.filter { case (v, e) => e != 0 })

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
    case that: Term[T] =>
      Term.TermIsFractional(ord, ordVar).compare(this, that) == 0
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

object Term {

  def apply[T](m: (Monomial[T], Rational))(implicit ord: Ordering[T]): Term[T] =
    this(m._2, m._1)

  def apply[T](c: Rational)(implicit ord: Ordering[T]): Term[T] =
    this(c, Monomial.empty[T])

  trait ExtraImplicits {

    implicit def infixFractionalTermOps[T](x: Term[T])(implicit frac: Fractional[Term[T]]): Fractional[Term[T]]#FractionalOps = new frac.FractionalOps(x)

    implicit def infixOrderingTermOps[T](x: Term[T])(implicit ord: Ordering[Term[T]]): Ordering[Term[T]]#Ops = new ord.Ops(x)
  }

  object Implicits extends ExtraImplicits

  implicit def TermIsFractional[T](implicit ord: Ordering[T], ordVar: Ordering[Monomial[T]]): Fractional[Term[T]] =
    new Fractional[Term[T]] {

      private def plusMinus(x: Term[T], y: Term[T])(f: (Rational, Rational) => Rational): Term[T] =
        if(x.variables == y.variables) {
          val nc = f(x.coefficient, y.coefficient)
          if(nc == 0)
            Term(0, Monomial.empty)
          else
            x.copy(coefficient = nc)
        } else x

      /** Plus on monomials. This is counterintuitive.
        * If 'variables' which is the part of monomial except the coefficient is different, returns self.
        * If not, returns a [[scagebra.polynomial.Term]] whose coefficient is the sum of `x`'s coefficient and `y`'s coefficient.
        * Notice, this doesn't satisfy the commutative property, so `x plus y == y plus x` isn't always satisfied.
        */
      def plus(x: Term[T], y: Term[T]): Term[T] =
        plusMinus(x, y)(_ + _)

      def minus(x: Term[T], y: Term[T]): Term[T] =
        plusMinus(x, y)(_ - _)

      def negate(x: Term[T]): Term[T] =
        x.copy(coefficient = -x.coefficient)

      private def timesDiv(x: Term[T], y: Term[T])(f: (Rational, Rational) => (Rational), g: (Int, Int) => Int): Term[T] =
        Term(f(x.coefficient, y.coefficient), y.variables.foldLeft(x.variables) { (vs, v) =>
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

      def times(x: Term[T], y: Term[T]): Term[T] =
        timesDiv(x, y)(_ * _, _ + _)
 
      def div(x: Term[T], y: Term[T]): Term[T] =
        timesDiv(x, y)(_ / _, _ - _)

      def toInt(x: Term[T]): Int =
        if(x.variables.isEmpty) x.coefficient.toInt
        else throw new IllegalStateException(s"$x cannot convert to Int")

      def toFloat(x: Term[T]): Float =
        if(x.variables.isEmpty) x.coefficient.toFloat
        else throw new IllegalStateException(s"$x cannot convert to Float")

      def toDouble(x: Term[T]): Double =
        if(x.variables.isEmpty) x.coefficient.toDouble
        else throw new IllegalStateException(s"$x cannot convert to Double")

      def toLong(x: Term[T]): Long =
        if(x.variables.isEmpty) x.coefficient.toLong
        else throw new IllegalStateException(s"$x cannot convert to Long")

      def fromInt(x: Int): Term[T] =
        Term(x, Monomial.empty[T])

      def compare(x: Term[T], y: Term[T]): Int =
        if(x.variables == y.variables) {
          if(x.coefficient < y.coefficient) -1
          else if(x.coefficient > y.coefficient) 1
          else 0
        } else if(x.coefficient == 0 && y.coefficient == 0) 0
        else ordVar.compare(x.variables, y.variables)
    }
}

