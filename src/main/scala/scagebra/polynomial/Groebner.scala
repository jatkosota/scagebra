package scagebra
package polynomial

import Monomial.Implicits._
import Term.Implicits._
import Polynomial.Implicits._

object Groebner {

  implicit class GroebnerPolynomial[T](self: Polynomial[T])(implicit ord: Ordering[T], ordMon: Ordering[Monomial[T]]) {

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
            if(div.monomial.forall(_._2 >= 0)) {
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
    def LM: Monomial[T] = LT.monomial
    /** leading term */
    def LT: Term[T] =
      if(self.terms.isEmpty)
        Term(0)
    else
      Term(self.terms.max)

    def multideg: Monomial[T] = LM
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
  def LCM[T](f: Polynomial[T], g: Polynomial[T])(implicit ord: Ordering[T]): Term[T] =
    Term(1, g.LM.foldLeft(f.LM) { (vs, vg) =>
      val (ng, eg) = vg
      vs.get(ng) match {
        case Some(ef) =>
          vs + (ng -> eg.max(ef))
        case None =>
          vs + vg
      }
    })

  /** @see [[scagebra.polynomial.Groebner#LCM]] */
  def γ[T](f: Polynomial[T], g: Polynomial[T])(implicit ord: Ordering[T]): Monomial[T] =
    LCM(f, g).monomial

  implicit def Syzygy[T](gs: Set[Polynomial[T]])(implicit ord: Ordering[T]): GroebnerBasis[T] = {
    val B = for {
      j <- 0 until gs.size
      i <- 0 until j
    } yield (i, j)
    val G = gs
    val t = gs.size

    def pair(i: Int, j: Int): (Int, Int) =
      if(i < j) (i, j) else (j, i)

    def criterion(G: List[Polynomial[T]], ij: (Int, Int), B: Set[(Int, Int)]): Boolean =
      (0 until G.size).exists { k =>
        val (i, j) = ij
        k != i && k != j &&
          !B(pair(i, k)) && !B(pair(j, k)) &&
            (LCM(G(i), G(j)) / G(k).LT).monomial.forall(_._2 >= 0)
      }

    def iteration(B: Set[(Int, Int)], G: List[Polynomial[T]], t: Int): List[Polynomial[T]] =
      if(B.isEmpty)
        G
      else {
        val i_j = B.head
        val (i, j) = i_j
        val fi = G(i)
        val fj = G(j)
        if(LCM(fi, fj) != Term(1, fi.LM) * Term(1, fj.LM) &&
          !criterion(G, (i, j), B)) {
          val S = S_G(fi, fj)(G)
          if(S != 0) iteration(B union (0 to t).map(i => (i, t)).toSet, G:+S, t + 1)
          else iteration(B.tail, G, t)
        } else iteration(B.tail, G, t)
      }

    GroebnerBasis(iteration(B.toSet, G.toList, t).toSet)
  }

  def Buchberger[T](gs: Set[Polynomial[T]])(implicit ord: Ordering[T]): GroebnerBasis[T] = {
    def loop(gs: Set[Polynomial[T]]): Set[Polynomial[T]] = {
      val ss = for {
        List(p, q) <- gs.toList.combinations(2)
        if LCM(p, q) != Term(1, p.LM) * Term(1, q.LM)
        s = S_G(p, q)(gs.toList)
        if s != 0
      } yield s
      val newGs = gs ++ ss
      if(gs == newGs)
        gs
      else
        loop(newGs)
    }

    GroebnerBasis(loop(gs))
  }

  def groebner[T](gs: Set[Polynomial[T]])(implicit ord: Ordering[T], algorithm: Set[Polynomial[T]] => GroebnerBasis[T]): GroebnerBasis[T] =
    algorithm(gs)

}
