package scagebra
package polynomial

object Groebner {

  implicit class GroebnerPolynomial[T](self: Polynomial[T])(implicit ord: Ordering[T], ordVar: Ordering[Variables[T]]) {

    /** leading coefficient */
    def LC: Rational = LT.coefficient
    /** leading monomial */
    def LM: Variables[T] = LT.variables
    /** leading term */
    def LT: Monomial[T] = Monomial(self.monomials.max)
  }
}
