package math

/**
  * Created by peyman on 11/17/2017.
  */

// stolen from https://gist.github.com/non/8ac4d82787883dbf136ef13e8d9887ee :))
import spire.algebra.{ Eq, Field }

class Mod(val residue: Long, val modulus: Long) { lhs =>

  def +(rhs: Mod): Mod = {
    require(lhs.modulus == rhs.modulus)
    Mod(lhs.residue + rhs.residue, modulus)
  }

  def unary_-(): Mod =
    Mod(-residue, modulus)

  def -(rhs: Mod): Mod = {
    require(lhs.modulus == rhs.modulus)
    Mod(lhs.residue - rhs.residue, modulus)
  }

  def *(rhs: Mod): Mod = {
    require(lhs.modulus == rhs.modulus)
    Mod(lhs.residue * rhs.residue, modulus)
  }

  def **(rhs: Int): Mod = {
    def loop(b: Long, k: Int, extra: Long): Long =
      if (k == 1) {
        (b * extra) % modulus
      } else {
        val x = if ((k & 1) == 1) (b * extra) % modulus else extra
        loop((b * b) % modulus, k >>> 1, x)
      }
    if (rhs == Int.MinValue) lhs.pow(rhs / 2).pow(2)
    else if (rhs < 0) lhs.reciprocal.pow(-rhs)
    else if (rhs == 0) Mod.one(modulus)
    else if (rhs == 1) lhs
    else Mod(loop(residue, rhs - 1, residue), modulus)
  }

  def pow(rhs: Int): Mod =
    lhs ** rhs

  def reciprocal: Mod = {
    def loop(t0: Long, t1: Long, r0: Long, r1: Long): Mod =
      if (r1 == 0L) Mod(t0, modulus) else {
        val q = r0 / r1
        loop(t1, t0 - q * t1, r1, r0 - q * r1)
      }
    if (residue == 0L) {
      throw new IllegalArgumentException("/0")
    } else {
      loop(0L, 1L, modulus, residue)
    }
  }

  def /(rhs: Mod): Mod =
    lhs * rhs.reciprocal

  override def equals(that: Any): Boolean =
    that match {
      case m: Mod => residue == m.residue && modulus == m.modulus
      case _ => false
    }

  def >(rhs: Mod): Boolean = {
    assert(this.modulus == rhs.modulus)
    return this.residue > rhs.residue
  }

  def <(rhs: Mod): Boolean = {
    assert(this.modulus == rhs.modulus)
    return this.residue < rhs.residue
  }


  override def hashCode(): Int =
    residue.## * modulus.##

  override def toString: String =
//    s"($residue mod $modulus)"
  s"$residue"
}

object Mod {

  final val MaxModulus = 3037000499L

  def zero(modulus: Long): Mod =
    Mod(0L, modulus)

  def one(modulus: Long): Mod =
    Mod(1L, modulus)

  def apply(n: Long, m: Long): Mod = {
    require(1 < m && m <= MaxModulus)
    val r = n % m
    new Mod(if (r >= 0) r else r + m, m)
  }

  implicit object ModHasEq extends Eq[Mod] {
    def eqv(x: Mod, y: Mod): Boolean =
      x.modulus == y.modulus && x.residue == y.residue
  }

  case class ModHasField(modulus: Long) extends Field[Mod] {
    require(modulus > 1L)
    // modulus should also be prime
    val zero: Mod = Mod.zero(modulus)
    val one: Mod = Mod.one(modulus)

    def plus(x: Mod, y: Mod): Mod = x + y

    override def negate(x: Mod): Mod = -x

    def times(x: Mod, y: Mod): Mod = x * y

    def div(x: Mod, y: Mod): Mod = x / y

    def quot(x: Mod, y: Mod): Mod = x / y

    def mod(x: Mod, y: Mod): Mod = zero

    def gcd(x: Mod, y: Mod): Mod = Mod.one(modulus)
  }
}