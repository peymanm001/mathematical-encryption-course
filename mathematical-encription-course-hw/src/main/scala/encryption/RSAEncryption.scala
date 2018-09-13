package encryption

import java.io.{BufferedInputStream, BufferedOutputStream, FileInputStream, FileOutputStream}
import java.nio.ByteBuffer

import scala.annotation.tailrec
import java.security.MessageDigest
import java.nio.file.{Files, Paths}

import math.Mod
/**
  * Created by peyman on 1/31/2018.
  */
class RSAEncryption(fName: String, key: RSAKey) extends Encryption {
  val filename = fName
  override def encrypt(enFilename: String): Unit = {
    val bis = new BufferedInputStream((new FileInputStream(filename)))
    val bos = new BufferedOutputStream(new FileOutputStream(enFilename))

    // write fileSize
    bos.write(ByteBuffer.allocate(4).putInt(bis.available()).array())

    var byte : Byte = 0
    var num: Long = 1
    var bit = 0
    while (bis.available() > 0) {
      byte = bis.read().toByte
      for ( i <- 1 to 8) {
        bit = byte >> (8 - i) & 1
        num *= 2
        num += bit
        if (num >= key.n) {
          num -= key.n
          val cipher = powMod(num, key.a.toInt, key.n)
          bos.write(ByteBuffer.allocate(4).putInt(cipher.toInt).array())
          num = 1
        }
      }
      if (bis.available() == 0 && num != 1) {
        num *= 2
        num += 1
        while ( num < key.n) num *= 2
        num -= key.n
        val cipher = powMod(num, key.a.toInt, key.n)
        bos.write(ByteBuffer.allocate(4).putInt(cipher.toInt).array())
        num = 1
      }
    }
    bis.close()
    bos.close()
  }

  def encryptAndSign(enFilename: String, prKey: RSAKey): Unit ={
    encrypt(enFilename)
    val bos = new BufferedOutputStream(new FileOutputStream(enFilename,true))
    var num: Long = 1
    var bit = 0
    val hash = generateHssh(filename).getBytes.map(byte => {
      for ( i <- 1 to 8) {
        bit = byte >> (8 - i) & 1
        num *= 2
        num += bit
        if (num >= prKey.n) {
          num -= prKey.n
          val cipher = powMod(num, prKey.a.toInt, prKey.n)
          bos.write(ByteBuffer.allocate(4).putInt(cipher.toInt).array())
          num = 1
        }
      }
    })
    if (num != 1) {
      num *= 2
      num += 1
      while ( num < prKey.n) num *= 2
      num -= prKey.n
      val cipher = powMod(num, prKey.a.toInt, prKey.n)
      bos.write(ByteBuffer.allocate(4).putInt(cipher.toInt).array())
      num = 1
    }
    bos.close()
  }

  def decryptAndValidate(decFilename: String, pubKey: RSAKey): Boolean = {

    import scala.util.control.Breaks._

    val bis = new BufferedInputStream((new FileInputStream(filename)))
    val bos = new BufferedOutputStream(new FileOutputStream(decFilename))

    val buffer : Array[Byte] = new Array[Byte](4)
    bis.read(buffer)
    val fileSize = ByteBuffer.wrap(buffer).getInt
    var writedData = 0

    var num = 0
    var out : String = ""
    var numberBuilder : String = ""
    var flag = 0
    breakable {
      while (bis.available() > 0) {
        bis.read(buffer)
        num = ByteBuffer.wrap(buffer).getInt
        var plain = powMod(num, key.a.toInt, key.n)
        plain += key.n
        numberBuilder = ""
        while (plain > 1) {
          numberBuilder = (if (plain % 2 == 0) "0" else "1") + numberBuilder
          plain /= 2
        }
        out += numberBuilder
        while (out.length >= 8) {
          bos.write(Integer.parseInt(out.substring(0, 8), 2).toByte)
          out = out.substring(8)
          writedData += 1
          if (writedData == fileSize) {
            bos.close()
            flag = 1
            break
          }
        }
      }
    }
    if (flag == 0) bos.close()
    // validate file

    // read hash
    writedData = 0
    out = ""
    var hash = ""
    breakable {
      while (bis.available() > 0) {
        bis.read(buffer)
        num = ByteBuffer.wrap(buffer).getInt
        var plain = powMod(num, pubKey.a.toInt, pubKey.n)
        plain += pubKey.n
        numberBuilder = ""
        while (plain > 1) {
          numberBuilder = (if (plain % 2 == 0) "0" else "1") + numberBuilder
          plain /= 2
        }
        out += numberBuilder
        while (out.length >= 8) {
          //        bos.write(Integer.parseInt(out.substring(0, 8), 2).toByte)
          hash += Integer.parseInt(out.substring(0, 8), 2).toChar
          out = out.substring(8)
          writedData += 1
          // sha-512 size
          if (writedData == 128) {
            bos.close()
            break()
          }
        }
      }
    }

    // generate hash
    val decHash = generateHssh(decFilename)
    return decHash == hash
  }


  def generateHssh(fileName: String): String = {
    val arr = Files.readAllBytes(Paths.get(fileName))
    val checksum = MessageDigest.getInstance("SHA-512").digest(arr)
    checksum.map("%02X" format _).mkString
  }


  private def powInMod (b: Long, pow: Int, mod: Long) : Long = {
    if (pow == 1) return b % mod
    if (pow % 2 == 0) {
      return powInMod(b, pow / 2, mod) * powInMod(b, pow / 2, mod) % mod
    }
    return b * powInMod(b, pow - 1, mod) % mod
  }


  private def powMod(b: Long, pow: Int, mod: Long): Long = {
    @tailrec
    def loop(b: Long, pow: Int, res: Long): Long = {
      if (pow  == 0) return res
      var newRes = res
      if ( pow % 2 == 1 ) newRes = res * b % mod
      return loop(b * b % mod, pow / 2, newRes)
    }
    return loop(b, pow, 1)
  }

  override def decrypt(decFilename: String): Unit = {
    val bis = new BufferedInputStream((new FileInputStream(filename)))
    val bos = new BufferedOutputStream(new FileOutputStream(decFilename))

    val buffer : Array[Byte] = new Array[Byte](4)
    bis.read(buffer)
    val fileSize = ByteBuffer.wrap(buffer).getInt
    var writedData = 0

    var num = 0
    var out : String = ""
    var numberBuilder : String = ""
    while (bis.available() > 0) {
      bis.read(buffer)
      num = ByteBuffer.wrap(buffer).getInt
      var plain = powMod(num, key.a.toInt, key.n)
      plain += key.n
      numberBuilder = ""
      while (plain > 1) {
        numberBuilder = (if (plain % 2 == 0) "0" else "1") + numberBuilder
        plain /= 2
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
    }
    bis.close()
    bos.close()
  }

  override def findKey(enFilename: String): AffineEncryptionKey = {

    return null
  }

}

object RSAEncryption {
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
  def findPrivateKey(pubkey: RSAKey): RSAKey = {
    var b = Math.sqrt(pubkey.n).toLong + 1
    var tmp = b * b - pubkey.n
    while( !isQuad(tmp)) {
      b += 1
      tmp = b * b - pubkey.n
    }
    val res = Math.sqrt(tmp).toLong
    val p = b + res
    val q = b-res
    val phi = (p-1) * (q-1)
    val a = getNumberInverse(pubkey.a, phi)
    return RSAKey(pubkey.n, a)
  }

  private def isQuad(num: Long): Boolean = {
    val res = Math.sqrt(num).toLong
    return res * res == num
  }
}

