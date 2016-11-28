package scagebra
package polynomial

case class Polynomial[T](monomials: Monomial[T]*)(implicit ord: Ordering[T]) {

  def plus(that: Polynomial[T]): Polynomial[T] = {
    ???
  }
}
    
