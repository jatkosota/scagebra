package scagebra

package object polynomial {

  import scala.collection.immutable.TreeMap

  /** Product of variables. */
  type Monomial[T] = TreeMap[T, Int]

  /** Make to write Term easy to read. 
    * This implicit class makes us to write (x ^ 1) instead (x, 1) or (x -> 1).
    */
  implicit class OrderingExp[T](self: T)(implicit ord: Ordering[T]) {
    def ^(e: Int) = (self, e)
  }

  /** Lexicographic order(lex). */
  implicit def LexOrdering[T](implicit ord: Ordering[T]): Ordering[Monomial[T]] =
    new Ordering[Monomial[T]] {
      import scala.math.Ordering.Implicits._
      def compare(x: Monomial[T], y: Monomial[T]): Int =
        (x zip y).dropWhile(t => t._1 == t._2).headOption match {
          case Some(((kl, vl), (kr, vr))) =>
            if(kl < kr) 1
            else if(kl > kr) -1
            else if(vl < vr) -1
            else if(vl > vr) 1
            else 0
          case None =>
            if(x.size == y.size) 0
            else if(x.size > y.size) 1
            else -1
        }
    }  

  object Monomial {

    trait ExtraImplicits {
      implicit def infixOrderingVariableOps[T](x: Monomial[T])(implicit ord: Ordering[Monomial[T]]): Ordering[Monomial[T]]#Ops = new ord.Ops(x)
    }

    object Implicits extends ExtraImplicits
    
    def empty[T](implicit ord: Ordering[T]): Monomial[T] =
      TreeMap.empty
  }

  def m[T](args: (T, Int)*)(implicit ord: Ordering[T]): Monomial[T] = {
    require(args.forall(_._2 >= 0))
    TreeMap(args.filter(_._2 != 0): _*)
  }

  def t[T](coef: Rational, vars: Monomial[T])(implicit ord: Ordering[T]): Term[T] =
    Term(coef, vars)

  def p[T](args: Term[T]*)(implicit ord: Ordering[T]): Polynomial[T] =
    Polynomial(args: _*)
}

