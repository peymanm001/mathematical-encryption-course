package encryption

import java.io.{BufferedInputStream, BufferedOutputStream, FileInputStream, FileOutputStream}

/**
  * Created by peyman on 10/19/2017.
  */
class AffineEncryption(fName: String, key: AffineEncryptionKey) extends Encryption {
  val filename = fName
  override def encrypt(enFilename: String): Unit = {
    val bis = new BufferedInputStream((new FileInputStream(filename)))
    val bos = new BufferedOutputStream(new FileOutputStream(enFilename))

    //    var writedData = 0
    var byte : Byte = 0
    var num = 1
    var bit = 0
    val bitLen = getBitLength(key.M)
    var out : String = ""
    //    var total = 0
    while (bis.available() > 0) {
      byte = bis.read().toByte
      for ( i <- 1 to 8) {
        bit = byte >> (8 - i) & 1
        num *= 2
        num += bit
        if (num >= key.M) {
          num -= key.M
          // affine
          num = (key.A * num + key.B) % key.M
          //
          out += toBinary(num, bitLen)
          if (out.length >= 8) {
            bos.write(Integer.parseInt(out.substring(0,8), 2).toByte)
            out = out.substring(8)
          }
          num = 1
        }
      }
    }
    val remainBit = out.length % 8
    if (remainBit != 0) {
      out += ("0" * (8 - remainBit))
      bos.write(Integer.parseInt(out, 2).toByte)
    }
    bos.write(num.toByte)
    bos.write(remainBit.toByte)
    bis.close()
    bos.close()
  }

  override def decrypt(decFilename: String): Unit = {
    val bis = new BufferedInputStream((new FileInputStream(filename)))
    val bos = new BufferedOutputStream(new FileOutputStream(decFilename))

    var byte : Byte = 0
    //    var num = 1
    var bit = 0
    val bitLen = getBitLength(key.M)
    var num = 0
    var out : String = ""
    var tmp : String = ""
    var numberBuilder : String = ""
    val AInverse = getNumberInverse(key.A, key.M)
    while (bis.available() > 3) {
      byte = bis.read().toByte
      for ( i <- 1 to 8) {
        bit = byte >> (8 - i) & 1
        tmp += bit.toString

        if (tmp.length == bitLen) {
          num = Integer.parseInt(tmp, 2)
          num = mod((num - key.B) * AInverse, key.M)
          num += key.M
          numberBuilder = ""
          while (num > 1) {
            numberBuilder = (if (num % 2 == 0) "0" else "1") + numberBuilder
            num /= 2
          }
          out += numberBuilder
          if (out.length >= 8) {
            bos.write(Integer.parseInt(out.substring(0, 8), 2).toByte)
            out = out.substring(8)
          }
          tmp = ""
        }
      }
    }

    val lastByte = bis.read()
    var remainNum = bis.read()
    val remainBits = bis.read()
    val until = if (remainBits == 0) 8 else remainBits
    for ( i <- 1 to until) {
      bit = lastByte >> (8 - i) & 1
      tmp += bit.toString

      if (tmp.length == bitLen) {
        num = Integer.parseInt(tmp, 2)
        num = mod((num - key.B) * AInverse, key.M)
        num += key.M
        numberBuilder = ""
        while (num > 1) {
          numberBuilder = (if (num % 2 == 0) "0" else "1") + numberBuilder
          num /= 2
        }
        out += numberBuilder
        if (out.length >= 8) {
          bos.write(Integer.parseInt(out.substring(0, 8), 2).toByte)
          out = out.substring(8)
        }
        tmp = ""
      }
    }
    numberBuilder = ""
    while(remainNum > 1) {
      numberBuilder = (if (remainNum % 2 == 0) "0" else "1") + numberBuilder
      remainNum /= 2
    }
    if (numberBuilder.length > 0) {
      out += numberBuilder
      bos.write(Integer.parseInt(out, 2).toByte)
    }
    bis.close()
    bos.close()
  }

  override def findKey(enFilename: String): AffineEncryptionKey = {
    val untilMod = 10
    val numberOfBitForCheck = 64
    var out = ""
    var num = 1
    var bit = 0
    var byte : Byte = 0
    var peyman = ""
    for ( m <- 3 to untilMod) {
      val bitLen = getBitLength(m)
      for ( a <- 2 to m-1 ) {
        if (gcd(m, a) == 1) {
          for (b <- 0 to m-1) {
            println("testing in mod " + m + ", a=" + a + ", b=" + b)
            val orgBis = new BufferedInputStream((new FileInputStream(filename)))
            while (out.length < numberOfBitForCheck) {
              byte = orgBis.read().toByte
              for ( i <- 1 to 8) {
                bit = byte >> (8 - i) & 1
                num *= 2
                num += bit
                if (num >= m) {
                  num -= m
                  // affine
                  num = (a * num + b) % m
                  //
                  out += toBinary(num, bitLen)
                  num = 1
                }
              }
            }
            val enBis = new BufferedInputStream((new FileInputStream(enFilename)))
            //        val enIntData = Integer.parseInt(out.substring(0,numberOfBitForCheck), 2)
            //        val enData :Array[Byte] = ByteBuffer.allocate(4).putChar(enIntData.toChar).array()
            val enData = out.substring(0, numberOfBitForCheck)
            val orgEnData: Array[Byte] = new Array[Byte](numberOfBitForCheck / 8)
            enBis.read(orgEnData)
            val tmp = Array[Byte](0) ++ orgEnData
            var orgData = BigInt(tmp).toString(2)
            orgData = repeatChar('0', numberOfBitForCheck - orgData.length) + orgData
            if (orgData.equals(enData)) {
              println("I find the key ;)")
              return new AffineEncryptionKey(m, a, b)
            }
            enBis.close()
            orgBis.close()
            out = ""
            num = 1
          }
        }
      }
    }
    return null
  }

  private def getNumberInverse(num :Int, M :Int) :Int = {
    for ( i <- 1 to M ) {
      if ( (num * i) % M == 1) {
        return i
      }
    }
    return 0
  }

  private def mod(num: Int, m: Int): Int = {
    var res = num % m
    while ( res < 0 ) res += m
    return res
  }

  @annotation.tailrec
  private def gcd(a: Int,b: Int): Int = {
    if (b == 0) a else gcd(b, a%b)
  }

//  private def getInverseList(m: Int) : List[Int] = {
//    val inverseList :List[Int] = List.empty[Int]
//    for ( n <- 0 to m) {
//      for ( i <- 0)
//    }
//  }
}
