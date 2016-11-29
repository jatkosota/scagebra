package scagebra

package object polynomial {

  import scala.collection.immutable.TreeMap

  /** Product of variables. */
  type Variables[T] = TreeMap[T, Int]

  /** Make to write Monomial easy to read. 
    * This implicit class makes us to write (x ^ 1) instead (x, 1) or (x -> 1).
    */
  implicit class OrderingExp[T](self: T)(implicit ord: Ordering[T]) {
    def ^(e: Int) = (self, e)
  }

  /** Lexicographic order(lex). */
  implicit def LexOrdering[T](implicit ord: Ordering[T]): Ordering[Variables[T]] =
    new Ordering[Variables[T]] {
      import scala.math.Ordering.Implicits._
      def compare(x: Variables[T], y: Variables[T]): Int =
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

  object Variables {

    trait ExtraImplicits {
      implicit def infixOrderingVariableOps[T](x: Variables[T])(implicit ord: Ordering[Variables[T]]): Ordering[Variables[T]]#Ops = new ord.Ops(x)
    }

    object Implicits extends ExtraImplicits
    
    def empty[T](implicit ord: Ordering[T]): Variables[T] =
      TreeMap.empty
  }

  def v[T](args: (T, Int)*)(implicit ord: Ordering[T]): Variables[T] = {
    require(args.forall(_._2 >= 0))
    TreeMap(args.filter(_._2 != 0): _*)
  }

  def m[T](coef: Rational, vars: Variables[T])(implicit ord: Ordering[T]): Monomial[T] =
    Monomial(coef, vars)

  def p[T](args: Monomial[T]*)(implicit ord: Ordering[T]): Polynomial[T] =
    Polynomial(args: _*)
}

