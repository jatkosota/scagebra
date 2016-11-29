package scagebra
package polynomial

import Monomial.Implicits._
import Polynomial.Implicits._

object Groebner {

  implicit class GroebnerPolynomial[T](self: Polynomial[T])(implicit ord: Ordering[T], ordVar: Ordering[Variables[T]]) {

    def div(base: List[Polynomial[T]]) = {
      @scala.annotation.tailrec
      def rec(fs: List[Polynomial[T]], as: List[Polynomial[T]], r: Polynomial[T], p: Polynomial[T]): (List[Polynomial[T]], Polynomial[T]) =
        if(p == 0) {
          (as, r)
        } else {
          val (nas, np, divisionoccurred) = subWhile(fs, as, List.empty, p)
          if(divisionoccurred)
            rec(fs, nas, r, np)
          else
            rec(fs, nas, r + np.LT, p - np.LT)
        }

      @scala.annotation.tailrec
      def subWhile(fs: List[Polynomial[T]], as: List[Polynomial[T]], nas: List[Polynomial[T]], p: Polynomial[T]): (List[Polynomial[T]], Polynomial[T], Boolean) =
        (fs, as) match {
          case (Nil, Nil) => (nas.reverse, p, false)
          case ((fh::ft), (ah::at)) =>
            val div = p.LT / fh.LT
            if(div.variables.forall(_._2 >= 0)) {
              val ai = ah + div
              // Fixme want to write like p - div * fh
              val np = p - Polynomial(div) * fh
              (nas.reverse ++ (ai::at), np, true)
            } else subWhile(ft, at, ah::nas, p)
          case _ => throw new IllegalStateException("Here will not be reached.")
        }

      rec(base, List.fill(base.size)(Polynomial.zero), Polynomial.zero, self)
    }

    /** leading coefficient */
    def LC: Rational = LT.coefficient
    /** leading monomial */
    def LM: Variables[T] = LT.variables
    /** leading term */
    def LT: Monomial[T] =
      if(self.monomials.isEmpty)
        Monomial(0)
    else
      Monomial(self.monomials.max)
  }
}
