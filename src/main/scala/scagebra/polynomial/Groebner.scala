package scagebra
package polynomial

import Variables.Implicits._
import Monomial.Implicits._
import Polynomial.Implicits._

object Groebner {

  implicit class GroebnerPolynomial[T](self: Polynomial[T])(implicit ord: Ordering[T], ordVar: Ordering[Variables[T]]) {

    def div(fs: List[Polynomial[T]]): List[Polynomial[T]] = divMod(fs)._1

    def / = div _

    def % = mod _

    def mod(fs: List[Polynomial[T]]): Polynomial[T] = divMod(fs)._2

    def /% = divMod _

    def divMod(fs: List[Polynomial[T]]) = {
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

      rec(fs, List.fill(fs.size)(Polynomial.zero), Polynomial.zero, self)
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

    def multideg: Variables[T] = LM
  }

  /** S-polynomial. */
  def S[T](f: Polynomial[T], g: Polynomial[T])(implicit ord: Ordering[T]): Polynomial[T] = {
    val lcm = LCM(f, g)
    f * (lcm/f.LT) - g * (lcm/g.LT) 
  }

  /** The remainder of S-polynomial divided by polynomials */
  def S_G[T](f: Polynomial[T], g: Polynomial[T])(gs: List[Polynomial[T]])(implicit ord: Ordering[T]): Polynomial[T] = 
    S(f, g) % gs

  /** least common multiple. 
    * 
    * multideg(f) = α, multideg(g) = β.
    * γ_i = max(α_i, β_i), γ = (γ_1, ..., γ_n).
    * x^γ^ is a least common multiple, x^γ^ = LCM(LM(f), LM(g)).
    */
  def LCM[T](f: Polynomial[T], g: Polynomial[T])(implicit ord: Ordering[T]): Monomial[T] =
    Monomial(1, g.LM.foldLeft(f.LM) { (vs, vg) =>
      val (ng, eg) = vg
      vs.get(ng) match {
        case Some(ef) =>
          vs + (ng -> eg.max(ef))
        case None =>
          vs + vg
      }
    })

  /** @see [[scagebra.polynomial.Groebner#LCM]] */
  def γ[T](f: Polynomial[T], g: Polynomial[T])(implicit ord: Ordering[T]): Variables[T] =
    LCM(f, g).variables

  def GroebnerBasis[T](gs: Set[Polynomial[T]])(implicit ord: Ordering[T]): Set[Polynomial[T]] = {
    val ss = for {
      List(p, q) <- gs.toList.combinations(2)
      if LCM(p, q) != Monomial(1, p.LM) * Monomial(1, q.LM)
      s = S_G(p, q)(gs.toList)
      if s != 0
    } yield s
    val newGs = gs ++ ss
    if(gs == newGs)
      gs
    else
      GroebnerBasis(newGs)
  }

  def MinimalGroebnerBasis[T](gs: Set[Polynomial[T]])(implicit ord: Ordering[T]): Set[Polynomial[T]] = {
    val groebnerBasis = GroebnerBasis(gs)
    val lt1 = groebnerBasis.map { p => p * Monomial(p.LC.reciprocal)(ord) }

    def eliminate(gs: List[Polynomial[T]], ms: Set[Polynomial[T]]): Set[Polynomial[T]] =
      gs match {
        case Nil => ms
        case hd::tl =>
          if(ms(hd)) {
            val canElim = ms.filter(g => (g.LT/hd.LT).variables.forall(_._2 >= 0))
            eliminate(tl, (ms &~ canElim) + hd)
          } else eliminate(tl, ms)
      }
    eliminate(lt1.toList.sorted, lt1)
  }

  def ReducedGroebnerBasis[T](gs: Set[Polynomial[T]])(implicit ord: Ordering[T]): Set[Polynomial[T]] = {
    val minGroebnerBasis = MinimalGroebnerBasis(gs)

    def reduce(gs: List[Polynomial[T]], gg: Set[Polynomial[T]]): Set[Polynomial[T]] =
      gs match {
        case Nil => gg
        case g::tl =>
          val gg_g = gg - g
          val g_ = g % gg_g.toList
          reduce(tl, gg_g + g_)
      }
    
    reduce(minGroebnerBasis.toList, minGroebnerBasis)
  }
}
