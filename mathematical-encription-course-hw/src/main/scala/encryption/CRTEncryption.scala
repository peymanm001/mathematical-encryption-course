package encryption

import java.io.{BufferedInputStream, BufferedOutputStream, FileInputStream, FileOutputStream}
import java.nio.ByteBuffer

import scala.collection.mutable.ListBuffer

/**
  * Created by peyman on 1/23/2018.
  */
class CRTEncryption(fName: String, key: CRTKey) extends Encryption {
  val filename = fName
  override def encrypt(enFilename: String): Unit = {
    val bis = new BufferedInputStream((new FileInputStream(filename)))
    val bos = new BufferedOutputStream(new FileOutputStream(enFilename))


    // write fileSize
    bos.write(ByteBuffer.allocate(4).putInt(bis.available()).array())

    var byte : Byte = 0
    var num = 1
    var bit = 0
//    val bitLen = getBitLength(key.M)
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
          for (prime <- key.primes) {
            out += toBinary(num % prime, getBitLength(prime))
            if (out.length >= 8) {
              bos.write(Integer.parseInt(out.substring(0,8), 2).toByte)
              out = out.substring(8)
            }
          }
          //
          num = 1
        }
      }
      if (bis.available() == 0 && num != 1) {
        num *= 2
        num += 1
        while ( num < key.M) num *= 2
        num -= key.M
        for (prime <- key.primes) {
          out += toBinary(num % prime, getBitLength(prime))
          while (out.length >= 8) {
            bos.write(Integer.parseInt(out.substring(0,8), 2).toByte)
            out = out.substring(8)
          }
        }
      }
    }
    while (out.length % 8 != 0) out+=0
    while (out.length >= 8) {
      bos.write(Integer.parseInt(out.substring(0,8), 2).toByte)
      out = out.substring(8)
    }
    bis.close()
    bos.close()
  }

  override def decrypt(decFilename: String): Unit = {
    val bis = new BufferedInputStream((new FileInputStream(filename)))
    val bos = new BufferedOutputStream(new FileOutputStream(decFilename))



    val fileSizeBuffer : Array[Byte] = new Array[Byte](4)
    bis.read(fileSizeBuffer)
    val fileSize = ByteBuffer.wrap(fileSizeBuffer).getInt
    var writedData = 0

    var byte : Byte = 0
    var bit = 0
    val bitLens = key.primes.map(p => getBitLength(p))
    var num = 0
    var out : String = ""
    var tmp : String = ""
    var numberBuilder : String = ""
    var pIndex = 0
    val nums: ListBuffer[Int] = ListBuffer()
    while (bis.available() > 0) {
      byte = bis.read().toByte
      for ( i <- 1 to 8) {
        bit = byte >> (8 - i) & 1
        tmp += bit.toString
        if (tmp.length == bitLens(pIndex)) {
          pIndex += 1
          num = Integer.parseInt(tmp, 2)
          nums.append(num)
          if (nums.length == key.primes.length) {
            var res = CRT(nums.toList, key.primes)
            res += key.M
            numberBuilder = ""
            while (res > 1) {
              numberBuilder = (if (res % 2 == 0) "0" else "1") + numberBuilder
              res /= 2
            }
            out += numberBuilder
            while (out.length >= 8) {
              bos.write(Integer.parseInt(out.substring(0, 8), 2).toByte)
              out = out.substring(8)
              writedData += 1
              if (writedData == fileSize) {
                bis.close()
                bos.close()
                return
              }
            }
            pIndex = 0
            nums.clear()
          }
          tmp = ""
          num = 1
        }
      }
    }
    bis.close()
    bos.close()
  }

  private def getNumberInverse(num :Int, M :Int) :Int = {
    for ( i <- 1 to M ) {
      if ( (num * i) % M == 1) {
        return i
      }
    }
    return 0
  }

  def CRT(nums: List[Int], primes: List[Int]): Int = {
    var res = 0
    for (i <- 0 to nums.length-1) {
      val p = primes(i)
      if ( nums(i) != 0 ) {
        val tmp = primes.filter(_ != p).reduce(_ * _)
        res += nums(i) * tmp * getNumberInverse(tmp, p)
      }
    }
    res % primes.reduce(_ * _)
  }

  override def findKey(enFilename: String): AffineEncryptionKey = {

    return null
  }
}
