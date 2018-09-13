package math

import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

/**
  * Created by peyman on 1/21/2018.
  */

class Term(co: Mod, deg: Int) {
  assert(co != 0)
  assert(deg >= 0)
  val coef = co
  val degree = deg

  def +(rhs: Term): Term = {
    assert(degree == rhs.degree)
    Term(coef + rhs.coef, degree)
  }

  def *(rhs: Term): Term = {
    Term(coef * rhs.coef, degree + rhs.degree)
  }

  def unary_-(): Term = {
    Term(-coef, degree)
  }

  override def toString: String = {
    if (degree > 1 ) {
      s"$coef" + s"x^$degree"
    } else if (degree == 1) {
      s"$coef" + "x"
    } else {
      s"$coef"
    }
  }
}

object Term {
  def apply(co: Mod, deg: Int): Term = new Term(co, deg)
}

class ModPoly(list: List[Term], M: Long){
  val terms = list.sortWith(_.degree > _.degree)
  var degree = 0
  var mod: Long = M
  if (terms.length == 0) {
    degree = -1
  } else {
    degree = terms(0).degree
  }


  private def addTerm(terms: List[Term], term: Term) : List[Term] = {
    if (term.coef == 0 ) {
      return terms
    }
    if (terms.isEmpty) {
      return List(term)
    }
    if (term.degree > terms(0).degree) {
      return List(term) ++ terms
    }
    if (term.degree < terms(0).degree) {
      return List(terms(0)) ++ addTerm(terms.slice(1, terms.length), term)
    }
    val coef = term.coef + terms(0).coef
    if (coef == Mod.zero(mod)) {
      return terms.slice(1, terms.length)
    }
    return List(new Term(coef, term.degree)) ++ terms.slice(1, terms.length)
  }

  def addTerm(term: Term): ModPoly = {
    ModPoly(addTerm(terms, term), mod)
  }

  def +(rhs: ModPoly): ModPoly = {
    def add(p1: List[Term], p2: List[Term]): List[Term] = {
      if (p1.isEmpty) {
        return p2
      }
      if (p2.isEmpty) {
        return p1
      }
      return add(p1.slice(1, p1.length), addTerm(p2, p1(0)))
    }
    ModPoly(add(terms, rhs.terms), mod)
  }

  def mulTerm(term: Term): ModPoly = {
    ModPoly(terms.map(t => t * term), mod)
  }

  def *(rhs: ModPoly): ModPoly = {
    if (degree == -1 || rhs.degree == -1) {
      return ModPoly(List(), mod)
    }
    terms.map(t1 => rhs.mulTerm(t1)).reduce(_ + _)
  }

  def unary_-(): ModPoly = {
    ModPoly(terms.map(term => -term), mod)
  }

  def -(rhs:ModPoly): ModPoly = {
    this + (-rhs)
  }

  @tailrec
  private def div(dividend: ModPoly, divisor: ModPoly, quotient: ModPoly): (ModPoly, ModPoly) = {
    if (dividend.degree < divisor.degree) {
      return (quotient, dividend)
    }
    val coef = dividend.terms(0).coef / divisor.terms(0).coef
    val deg = dividend.terms(0).degree - divisor.terms(0).degree
    val term = Term(coef, deg)
    return div(dividend - divisor.mulTerm(term), divisor, quotient.addTerm(term))
  }

  def /(rhs:ModPoly): ModPoly = {
    val (qout, rem) = div(this, rhs, ModPoly(List(), mod))
    qout
  }

  def %(rhs:ModPoly): ModPoly = {
    val (qout, rem) = div(this, rhs, ModPoly(List(), mod))
    rem
  }

  def gcd(other: ModPoly): ModPoly = {
    if (other.degree == -1) {
      return this
    }
    return other.gcd(this % other)
  }

  def inverse(mod: ModPoly): ModPoly = {
    def extended(p1: ModPoly, p2: ModPoly): (ModPoly, ModPoly) = {
      if (p2.degree == -1) {
        if (p1.degree > 0) {
          return (ModPoly(List(), this.mod), ModPoly(List(), this.mod))
        }
        return (ModPoly(List(Term(Mod.one(this.mod), 0)), this.mod), p2)
      }
      val (a, b) = extended(p2, p1 % p2)
      val q = p1 / p2
      return (b, a - b * q)
    }
    val (_, beta) = extended(mod, this)
    return ModPoly(List(Term(mod.gcd(this).terms(0).coef.reciprocal, 0)), this.mod) * beta
  }

  def getCoefList(): List[Mod] = {
    val coefLists: ListBuffer[Mod] = ListBuffer.fill(degree + 1)(Mod.zero(mod))
    for(term <- terms) {
      coefLists(term.degree) = term.coef
    }
    coefLists.reverse.toList
  }

  def getCoefList(deg: Int): List[Mod] = {
    val coefLists: ListBuffer[Mod] = ListBuffer.fill(deg + 1)(Mod.zero(mod))
    for(term <- terms) {
      coefLists(deg - term.degree) = term.coef
    }
    coefLists.toList
  }

  override def toString: String = {
    if (degree == -1) {
      return "0"
    }
    return terms.mkString(" + ")
  }

}

object ModPoly {
  def apply(list: List[Term], mod: Long): ModPoly = new ModPoly(list, mod)
}