package math

import scala.collection.mutable.ListBuffer

/**
  * Created by peyman on 1/30/2018.
  */
object SquareRoot {
  def isQuad(num: Mod, p: Int): Boolean = {
    assert(num.modulus == p)
    if ( num.pow((p - 1) / 2).residue == p - 1) {
      return false
    }
    return true
  }

  private def factorCount(num: Int, fac: Int): Int = {
    var cnt = 0
    var n = num
    while ( n % fac == 0) {
      n /= fac
      cnt += 1
    }
    return cnt
  }

  private def power(num: Long, pow: Long): Long = {
    return Math.pow(num, pow).toLong
  }

  private def CRT(nums: List[Long], primes: List[Long]): Long= {
    if (nums.length == 1) return nums(0)
    var res: Long = 0
    for (i <- 0 to nums.length-1) {
      val p = primes(i)
      if ( nums(i) != 0 ) {
        val tmp = primes.filter(_ != p).reduce(_ * _)
        res += nums(i) * tmp * Mod(tmp, p).reciprocal.residue
      }
    }
    res % primes.reduce(_ * _)
  }

  private def permuattion(lst: List[List[Long]]): List[List[Long]] = {
    if (lst.length == 0 ) {
      return List(List())
    }
    if (lst.length == 1) {
      return lst(0).map(elm => List(elm))
    }
    val res = ListBuffer[List[Long]]()
    permuattion(lst.slice(1, lst.length)).map(row => lst(0).map(elm => res.append(List(elm) ++ row)))
    return res.toList
  }

  private def gcd(a: Int,b: Int): Int = {
    if(b ==0) a else gcd(b, a%b)
  }

  def squareInPrimeMod(num: Mod, p: Int): List[Long] = {
    assert(num.modulus == p)
    def calculate(groupSize: Int, num: Mod, q: Mod): List[Long] = {
      if ( groupSize % 4 == 2) {
        val res = num.pow( (groupSize + 2) / 4).residue
        return List(res, p - res)
      }
      if (groupSize % 4 == 0) {
        val tmp = num.pow(groupSize / 4).residue
        if ( tmp == 1) {
          return calculate(groupSize / 2, num, q)
        }
        if ( tmp == p - 1) {
          val res = calculate(groupSize / 2, num * q.pow(2), q)
          return res.map(ans => (Mod(ans, p) * q.reciprocal).residue)
        }
      }
      return List()
    }
    if ( !isQuad(num, p)) {
      return List()
    }
    val q = (2 to p-1).map(n => Mod(n, p)).filter(n => !isQuad(n, p))(0)
    return calculate(p - 1, num, q)
  }


  def squareInPowPrimeMod(num: Mod, p: Int, pow: Int): List[Long] = {
    assert(num.modulus.toLong == power(p, pow))
    if ( num.residue == 0) {
      return (0 to (power(p, (pow - 1) / 2) - 1).toInt).map(alpha => (alpha * power(p, (pow / 2) + 1))).toList
    }
    val facCnt = factorCount(num.residue.toInt, p)
    if (facCnt == 0) {
      if (pow == 1) {
        return squareInPrimeMod(num, p)
      }
      val res = squareInPowPrimeMod(Mod(num.residue, power(p, pow - 1)), p, pow - 1)
      val alphaLst = res.map(x => ((Mod((num.residue - power(x, 2)) / power(p, pow - 1), p)
                                                          * Mod(2, p).reciprocal * Mod(x, p).reciprocal).residue, x))
      return alphaLst.map(t => Mod(power(p, pow-1) * t._1 + t._2, power(p, pow)).residue)
    }
    if (facCnt % 2 == 1) {
      return List()
    }
    if (facCnt % 2 == 0) {
      val res = squareInPowPrimeMod(Mod((num.residue / power(p, pow - facCnt)), power(p, pow-facCnt)), p, pow - facCnt)
      val k = facCnt / 2
      val pPowK = power(p, k)
      val pPowNminusK = power(p, pow - k)
      val finalRes : ListBuffer[Long] = ListBuffer()
      (0 to (pPowK - 1).toInt).map(alpha => res.map(x => finalRes.append((alpha * pPowNminusK + x * pPowK).toLong)))
      return finalRes.toList
    }
    return List()
  }

  def squareInCompositeMod(num: Mod, mod: List[(Int, Int)]): List[Long] = {
    val n = num.residue
    val res = permuattion(mod.map(powPrime => squareInPowPrimeMod(Mod(n, power(powPrime._1.toInt, powPrime._2.toInt))
            , powPrime._1, powPrime._2)))

    val mods = mod.map(pp => power(pp._1, pp._2))
    return res.map(lst => CRT(lst, mods))
  }

  def squareEquation(a: Int, b: Int, c: Int, mod: List[(Int, Int)]): List[Long] = {
    var modulus:Long = 1
    mod.foreach(pp => modulus *= power(pp._1, pp._2))
    val d = gcd(modulus.toInt, a)
    if ( d == 1) {
      val B = (Mod(b, modulus) * Mod(a, modulus).reciprocal).residue
      val C = (Mod(c, modulus) * Mod(a, modulus).reciprocal).residue
      val beta = (Mod(B, modulus) / Mod(2, modulus)).residue
      val res = squareInCompositeMod(Mod(power(beta, 2) - C, modulus), mod)
      return res.map(elm => Mod(elm - beta, modulus).residue)
    }
    val alpha = Mod(-c * Mod(b, d).reciprocal.residue, d).residue
    val newMod = mod.map(pp => {
      var cnt = pp._2
      var d2 = d
      while ( d2 % pp._1 == 0) {
        d2 /= pp._1
        cnt -= 1
      }
      (pp._1, cnt)
    }).filter(pp => pp._2 != 0)
    val res = squareEquation(a * d, (2 * a * alpha + b).toInt,
          ((a * power(alpha, 2) + b * alpha + c) / d).toInt, newMod)
    return res.map(elm => elm * d + alpha)
  }
}
