package scagebra
package polynomial

import Monomial.Implicits._
import Term.Implicits._
import Polynomial.Implicits._
import Groebner._

case class GroebnerBasis[T](bases: Set[Polynomial[T]])(implicit ord: Ordering[T]) {

  def minimal: GroebnerBasis[T] = {
    val lt1 = bases.map { p => p * Term(p.LC.reciprocal)(ord) }

    def eliminate(gs: List[Polynomial[T]], ms: Set[Polynomial[T]]): Set[Polynomial[T]] =
      gs match {
        case Nil => ms
        case hd::tl =>
          if(ms(hd)) {
            val canElim = ms.filter(g => (g.LT/hd.LT).monomial.forall(_._2 >= 0))
            eliminate(tl, (ms &~ canElim) + hd)
          } else eliminate(tl, ms)
      }

    GroebnerBasis(eliminate(lt1.toList.sorted, lt1))
  }

  def reduced: GroebnerBasis[T] = {
    val mini = minimal

    def reduce(gs: List[Polynomial[T]], gg: Set[Polynomial[T]]): Set[Polynomial[T]] =
      gs match {
        case Nil => gg
        case g::tl =>
          val gg_g = gg - g
          val g_ = g % gg_g.toList
          reduce(tl, gg_g + g_)
      }

    GroebnerBasis(reduce(mini.bases.toList, mini.bases))
  }
}

