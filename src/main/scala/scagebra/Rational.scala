package scagebra

case class Rational(numer: BigInt, denom: BigInt = 1) {
  require(denom != 0, "denominator must be nonzero.")

  lazy val reduce = {
    val r = numer.abs gcd denom.abs
    if((numer < 0 && denom < 0) || (numer > 0 && denom > 0))
      Rational(numer.abs/r, denom.abs/r)
    else
      Rational(-numer.abs/r, denom.abs/r)
  }

  def reciprocal: Rational = Rational(denom, numer)

  override def canEqual(other: Any) = other match {
    case _: Rational => true
    case _: Int      => true
    case _: Long     => true
    case _: BigInt   => true
    case _: Float    => true
    case _: Double   => true
    case _           => false
  }

  import RationalImplicits._

  override def equals(other: Any) = canEqual(other) && (other match {
    case that: Rational =>
      RationalIsFractional.compare(this, that) == 0
    case that: Int =>
      this == Rational(that)
    case that: Long =>
      this == Rational(that)
    case that: BigInt =>
      this == Rational(that)
    case that: Float =>
      reduce.toFloat == that
    case that: Double =>
      reduce.toDouble == that
    case _ => false
  })

  override def hashCode: Int = (31 * reduce.numer + reduce.denom).toInt

  override def toString =
    s"$numer" + (if(denom == 1) "" else s"/$denom")
}

object RationalImplicits extends scala.math.Ordering.ExtraImplicits
    with scala.math.Fractional.ExtraImplicits {

  implicit def IntToRational(self: Int) = new Rational(self)

  implicit def LongToRational(self: Long) = new Rational(self)

  implicit def BigIntToRational(self: BigInt) = new Rational(self)

  implicit val RationalIsFractional: Fractional[Rational] = new Fractional[Rational] {

    def compare(x: Rational, y: Rational): Int =
      (x.reduce.numer*y.reduce.denom) compare (x.reduce.denom*y.reduce.numer)

    def plus(x: Rational, y: Rational): Rational =
      Rational(
        x.reduce.numer*y.reduce.denom + y.reduce.numer*x.reduce.denom,
        x.reduce.denom*y.reduce.denom
      )

    def minus(x: Rational, y: Rational): Rational =
      plus(x, -y)

    def negate(x: Rational): Rational =
      x.copy(numer = -x.numer)

    def times(x: Rational, y: Rational): Rational =
      Rational(x.numer*y.numer, x.denom*y.denom)

    def div(x: Rational, y: Rational): Rational =
      times(x, y.reciprocal)

    def fromInt(x: Int): Rational =
      Rational(x)

    def toDouble(x: Rational): Double =
      x.numer.toDouble / x.denom.toDouble

    def toFloat(x: Rational): Float =
      x.numer.toFloat / x.denom.toFloat

    def toInt(x: Rational): Int =
      (x.numer / x.denom).toInt

    def toLong(x: Rational): Long =
      (x.numer / x.denom).toLong
  }
}
