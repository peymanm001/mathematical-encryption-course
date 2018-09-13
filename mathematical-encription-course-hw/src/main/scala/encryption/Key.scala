package encryption

/**
  * Created by peyman on 10/13/2017.
  */
import java.io.{BufferedInputStream, BufferedOutputStream, FileInputStream, FileOutputStream}
import java.nio.ByteBuffer

import math._
import org.apache.commons.math3.primes.Primes

import scala.collection.mutable.{HashMap, ListBuffer}

abstract class Key (base :Int) {
  def store(filename: String) : Boolean
}

object Key {
  val rand = scala.util.Random
  val baseBank = List(3, 5, 6, 7, 9)

}

class ModularEncryptionKey (base: Int, blockSize: Int, encTable: HashMap[Char, Char]) extends Key(base) {
  val bSize : Int = blockSize
  val enTable :HashMap[Char, Char] = encTable

  override def store(filename :String): Boolean = {
    val bos = new BufferedOutputStream(new FileOutputStream(filename))
    bos.write(base)
    bos.write(ByteBuffer.allocate(4).putInt(bSize).array())
    for((k, v) <- enTable) {
      bos.write(k)
      bos.write(v)
    }
    bos.close()
    return true
  }

  override def toString: String = s"MeKey{Base: $base, BlockSize: $blockSize, EnTable: $enTable}"
}

object ModularEncryptionKey {
  def load(filename: String): ModularEncryptionKey = {
    val bis = new BufferedInputStream(new FileInputStream(filename))
    val base :Int = bis.read()
    val buffer : Array[Byte] = new Array[Byte](4)
    bis.read(buffer)
    val blockSize :Int = ByteBuffer.wrap(buffer).getInt
    val enTable : HashMap[Char, Char] = new HashMap[Char, Char]()
    while (bis.available() > 0) {
      enTable.put(bis.read().toChar, bis.read().toChar)
    }
    bis.close()
    return new ModularEncryptionKey(base, blockSize, enTable)
  }

  def generate(): ModularEncryptionKey = {
    val base = Key.baseBank(Key.rand.nextInt(Key.baseBank.size))
    val blockSize = 1 + Key.rand.nextInt(1023)
    val lists = List.range((48 + 0).toChar, (48 + base).toChar).permutations.toList
    val enList = lists(Key.rand.nextInt(lists.size))
    val enTable : HashMap[Char, Char] = new HashMap[Char, Char]()
    var char = 48
    for (elm <- enList) {
      enTable.put(char.toChar, elm)
      char+=1
    }

    return new ModularEncryptionKey(base, blockSize, enTable)
  }
}

class BitwiseModularEncryptionKey(base :Int, encTree :Array[String]) extends Key(base) {
  val enTree :Array[String] = encTree

  override def store(filename: String): Boolean = store(filename, '\1')

  def store(filename :String, delimiter :Char = '\1'): Boolean = {
    val bos = new BufferedOutputStream(new FileOutputStream(filename))
    bos.write(base)
    for(s <- enTree) {
      bos.write(s.getBytes)
      bos.write(delimiter.toByte)
    }
    bos.close()
    return true
  }

  override def toString: String = s"BwMeKey{Base: $base, EnTree: {${enTree.mkString(",")}}"

}

object BitwiseModularEncryptionKey {
  def load(filename :String, delimiter :Char = '\1'): BitwiseModularEncryptionKey = {
    val bis = new BufferedInputStream(new FileInputStream(filename))
    val base :Int = bis.read()
    val enTree :Array[String] = new Array[String](base)
    var tmpString :String = ""
    var reader :Char = 0
    var i = 0
    while ( bis.available() > 0) {
      reader = bis.read().toChar
      if (reader == delimiter) {
        enTree(i) = tmpString
        tmpString = ""
        i += 1
      } else {
        tmpString += reader
      }
    }
    bis.close()
    return new BitwiseModularEncryptionKey(base, enTree)
  }
  def generate() : BitwiseModularEncryptionKey = {
    val base = Key.baseBank(Key.rand.nextInt(Key.baseBank.size))
    val pureList = base match {
      case 3 => List("00", "01", "1")
      case 5 => List("000", "001", "01", "10", "11")
      case 6 => List("000", "001", "010", "011", "10", "11")
      case 7 => List("000", "001", "010", "011", "100", "101", "11")
      case 9 => List("0000", "0001", "001", "010", "011", "100", "101", "110", "111")
    }
    val lists = pureList.permutations.toList
    val enTree = lists(Key.rand.nextInt(lists.size))

    return new BitwiseModularEncryptionKey(base, enTree.toArray)
  }
}

class AffineEncryptionKey(mode :Int, a: Int, b: Int) extends Key(mode) {
  val M = mode
  val A = a
  val B = b
  override def store(filename: String): Boolean = {
    val bos = new BufferedOutputStream(new FileOutputStream(filename))
    bos.write(M)
    bos.write(a)
    bos.write(b)
    bos.close()
    return true
  }


  override def toString = s"AffineKey{M: $M, A: $A, B: $B}"
}

object AffineEncryptionKey {
  def load(filename :String): AffineEncryptionKey = {
    val bis = new BufferedInputStream(new FileInputStream(filename))
    val M :Int = bis.read()
    val a :Int = bis.read()
    val b :Int = bis.read()
    bis.close()
    return new AffineEncryptionKey(M, a, b)
  }

  def generate() : AffineEncryptionKey = {
    val M = 3 + Key.rand.nextInt(253)
    var a = 2 + Key.rand.nextInt(M)
    while (gcd(M, a) != 1) {
      a = 2 + Key.rand.nextInt(M)
    }
    val b = Key.rand.nextInt(M)
    return new AffineEncryptionKey(M, a, b)
  }

  private def gcd(a: Int,b: Int): Int = {
    if(b ==0) a else gcd(b, a%b)
  }
}

class AffinePolyKey(mod: Int, xorKeyArray: Array[Byte], matSize: Int,  affineHillMatH: ModMatrix,
                    affineHillMatB: ModMatrix, polynomialMod: ModPoly, polynomialA: ModPoly,
                    polynomialB: ModPoly) extends Key(mod) {
  val M = mod
  val xorBlockSize = xorKeyArray.length
  val xorKey = xorKeyArray
  val affineBlockSize = matSize
  val affineHillH = affineHillMatH
  val affineHillB = affineHillMatB
  val polyMod = polynomialMod
  val polyA = polynomialA
  val polyB = polynomialB

  override def store(filename: String): Boolean = {
    val bos = new BufferedOutputStream(new FileOutputStream(filename))
    bos.write(ByteBuffer.allocate(4).putInt(M).array())
    bos.write(xorBlockSize)
    xorKey.map(byte => bos.write(byte))
    bos.write(affineBlockSize)
    affineHillH.rows.map(row => row.map(num => bos.write(ByteBuffer.allocate(4).putInt(num.residue.toInt).array())))
    affineHillB.rows.map(row => row.map(num => bos.write(ByteBuffer.allocate(4).putInt(num.residue.toInt).array())))
    polyMod.getCoefList(affineBlockSize).map(num => bos.write(ByteBuffer.allocate(4).putInt(num.residue.toInt).array()))
    polyA.getCoefList(affineBlockSize-1).map(num => bos.write(ByteBuffer.allocate(4).putInt(num.residue.toInt).array()))
    polyB.getCoefList(affineBlockSize-1).map(num => bos.write(ByteBuffer.allocate(4).putInt(num.residue.toInt).array()))
    bos.close()
    return true
  }
}

object AffinePolyKey {
  private final val MAXPRIME: Int = 100
  private final val MAXXORBLOCKSIZE: Int = 100
  private final val MAXMATSIZE: Int = 7
  private final val MAXPOLYDEGREE: Int = 10

  def load(filename:String): AffinePolyKey = {
    val bis = new BufferedInputStream(new FileInputStream(filename))
    val intBuffer : Array[Byte] = new Array[Byte](4)
    bis.read(intBuffer)
    val M: Int = ByteBuffer.wrap(intBuffer).getInt
    val xorBlockSize: Int = bis.read()
    val xorKey: ListBuffer[Byte] = ListBuffer()
    for ( i <- 1 to xorBlockSize) {
      xorKey.append(bis.read().toByte)
    }
    val affineBlockSize: Int = bis.read()
    val affineHillHList : ListBuffer[List[Mod]] = ListBuffer()
    for ( i <- 1 to affineBlockSize) {
      val row = ListBuffer[Mod]()
      for ( i <- 1 to affineBlockSize) {
        bis.read(intBuffer)
        row.append(Mod(ByteBuffer.wrap(intBuffer).getInt, M))
      }
      affineHillHList.append(row.toList)
    }
    val affineHillH = ModMatrix(affineHillHList.toList)
    val affineHillBList : ListBuffer[List[Mod]] = ListBuffer()
    for ( i <- 1 to affineBlockSize) {
      val row = ListBuffer[Mod]()
      bis.read(intBuffer)
      row.append(Mod(ByteBuffer.wrap(intBuffer).getInt, M))
      affineHillBList.append(row.toList)
    }
    val affineHillB = ModMatrix(affineHillBList.toList)
    val polyModList = ListBuffer[Term]()
    for ( i <- affineBlockSize to 0 by -1) {
      bis.read(intBuffer)
      polyModList.append(Term(Mod(ByteBuffer.wrap(intBuffer).getInt, M), i))
    }
    val polyMod = ModPoly(polyModList.toList, M)
    val polyAList = ListBuffer[Term]()
    for ( i <- affineBlockSize-1 to 0 by -1) {
      bis.read(intBuffer)
      polyAList.append(Term(Mod(ByteBuffer.wrap(intBuffer).getInt, M), i))
    }
    val polyA = ModPoly(polyAList.toList, M)
    val polyBList = ListBuffer[Term]()
    for ( i <- affineBlockSize-1 to 0 by -1) {
      bis.read(intBuffer)
      polyBList.append(Term(Mod(ByteBuffer.wrap(intBuffer).getInt, M), i))
    }
    val polyB = ModPoly(polyBList.toList, M)
    bis.close()
    return new AffinePolyKey(M, xorKey.toArray, affineBlockSize, affineHillH, affineHillB, polyMod, polyA, polyB)
  }

  def generate(): AffinePolyKey = {
    // mod
    val M = Primes.nextPrime(Key.rand.nextInt(MAXPRIME))

    // xor key
    val xorBlockSize = Key.rand.nextInt(MAXXORBLOCKSIZE) + 10
    val xorKey: Array[Byte] = Array.fill(xorBlockSize)(Key.rand.nextInt(255).toByte)

    // affine hill key
    val matSize = Key.rand.nextInt(MAXMATSIZE) + 2
//    val matSize = 9
    var affineHillH :ModMatrix = null
    do {
      val affineHillHList: List[List[Mod]] = List.fill(matSize)(List.fill(matSize)(Mod(Key.rand.nextInt(M-1), M)))
      affineHillH = ModMatrix(affineHillHList)
    } while (affineHillH.inv().mod == 0)
    val affineHillBList: List[List[Mod]] = List.fill(matSize)(List.fill(1)(Mod(scala.util.Random.nextInt(M-1), M)))
    val affineHillB = ModMatrix(affineHillBList)

    // polynomial key
    val lCPoly = Key.rand.nextInt(M-1) + 1
    val polyModList: List[Term] = (List(Mod(lCPoly, M)) ++ List.fill(matSize)(Mod(Key.rand.nextInt(M-1), M))).
      zip(matSize to 0 by -1).map(t => Term(t._1, t._2))
    val polyMod = ModPoly(polyModList, M)
    var polyA: ModPoly = null
    do {
      val polyAList: List[Term] = (List.fill(matSize)(Mod(Key.rand.nextInt(M-1), M)))
                                    .zip(matSize-1 to 0 by -1).map(t => Term(t._1, t._2))
      polyA = ModPoly(polyAList, M)
    } while(polyA.inverse(polyMod).degree == -1)
    val polyBList: List[Term] = (List.fill(matSize)(Mod(Key.rand.nextInt(M-1), M))).
      zip(matSize-1 to 0 by -1).map(t => Term(t._1, t._2))
    val polyB = ModPoly(polyBList, M)

    return new AffinePolyKey(M, xorKey, matSize, affineHillH, affineHillB, polyMod, polyA, polyB)
  }
}


class CRTKey(list: List[Int], mod: Int) extends Key(mod) {
  val primes = list
  val M = mod

  override def store(filename: String): Boolean = {
    val bos = new BufferedOutputStream(new FileOutputStream(filename))
    bos.write(ByteBuffer.allocate(4).putInt(M).array())
    bos.write(primes.length)
    primes.map(p => bos.write(p))
    bos.close()
    return true
  }
}

object CRTKey {
  def apply(list: List[Int], mod: Int): CRTKey = new CRTKey(list, mod)
  def generate() :CRTKey = {
    val num = Key.rand.nextInt(3) + 2
    val primes: ListBuffer[Int] = ListBuffer()
    var M = 1
    for ( i <- 1 to num) {
      var p = Primes.nextPrime(Key.rand.nextInt(97) + 3)
      while (primes.contains(p)) {
        p = Primes.nextPrime(Key.rand.nextInt(97) + 3)
      }
      M *= p
      primes.append(p)
    }
    CRTKey(primes.toList, M)
  }

  def load(filename: String): CRTKey = {
    val bis = new BufferedInputStream(new FileInputStream(filename))
    val buffer : Array[Byte] = new Array[Byte](4)
    bis.read(buffer)
    val M :Int = ByteBuffer.wrap(buffer).getInt
    val count :Int = bis.read()
    val primes: ListBuffer[Int] = ListBuffer()
    for ( i <- 0 to count-1 ) {
      primes.append(bis.read())
    }
    bis.close()
    CRTKey(primes.toList, M)
  }
}

class RSAKey(N: Long, A: Long) extends Key(N.toInt) {
  val n = N
  val a = A
  override def store(filename: String): Boolean = {
    val bos = new BufferedOutputStream(new FileOutputStream(filename))
    bos.write(ByteBuffer.allocate(8).putLong(n).array())
    bos.write(ByteBuffer.allocate(8).putLong(a).array())
    bos.close()
    return true
  }

  override def toString: String = s"RSAKey{n: $n, a: $a}"
}

object RSAKey {
  private final val MAXPRIME: Int = 45000

  def apply(N: Long, A: Long): RSAKey = new RSAKey(N, A)
  private def getNumberInverse(num :Long, M :Long): Long = {
    def loop(t0: Long, t1: Long, r0: Long, r1: Long): Long =
      if (r1 == 0L) Mod(t0, M).residue else {
        val q = r0 / r1
        loop(t1, t0 - q * t1, r1, r0 - q * r1)
      }
    if (num == 0) {
      return 0
    } else {
      loop(0L, 1L, M, num)
    }
  }

  private def gcd(a: Long,b: Long): Long = {
    if(b ==0) a else gcd(b, a%b)
  }

  def generate(): (RSAKey, RSAKey) = {
    val p = Primes.nextPrime(Key.rand.nextInt(MAXPRIME) + 3)
    var q = Primes.nextPrime(Key.rand.nextInt(MAXPRIME) + 3)
    while ( p == q){
      q = Primes.nextPrime(Key.rand.nextInt(MAXPRIME) + 3)
    }
    val n = p * q
    val phi = (p - 1) * (q - 1)
    var a = Key.rand.nextInt(phi) + 3
    while( gcd(a, phi) != 1 ) {
      a = Key.rand.nextInt(phi) + 3
    }
    var b = getNumberInverse(a, phi)
    while (b == 0) {
      a = Key.rand.nextInt() + 3
      b = getNumberInverse(a, phi)
    }
    val pubKey = RSAKey(n, a)
    val prKey = RSAKey(n, b)
    return (pubKey, prKey)
  }

  def load(filename: String): RSAKey = {
    val bis = new BufferedInputStream(new FileInputStream(filename))
    val buffer : Array[Byte] = new Array[Byte](8)
    bis.read(buffer)
    val n :Long = ByteBuffer.wrap(buffer).getLong
    bis.read(buffer)
    val a :Long = ByteBuffer.wrap(buffer).getLong
    bis.close()
    RSAKey(n, a)
  }
}