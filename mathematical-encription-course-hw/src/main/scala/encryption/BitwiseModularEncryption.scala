package encryption

import java.io.{BufferedInputStream, BufferedOutputStream, FileInputStream, FileOutputStream}

/**
  * Created by peyman on 10/13/2017.
  */
class BitwiseModularEncryption (fName :String, key :BitwiseModularEncryptionKey) extends Encryption {
  val filename :String = fName
  override def encrypt(enFilename: String): Unit = {
    val bis = new BufferedInputStream((new FileInputStream(filename)))
    val bos = new BufferedOutputStream(new FileOutputStream(enFilename))

    //    var writedData = 0
    var byte : Byte = 0
    var num = 1
    var bit = 0
    var out : String = ""
    val base = key.enTree.size
//    var total = 0
    while (bis.available() > 0) {
      byte = bis.read().toByte
      for ( i <- 1 to 8) {
        bit = byte >> (8 - i) & 1
        num *= 2
        num += bit
        if (num >= base) {
          num -= base
          out += (key.enTree(num))
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

  override def decrypt(decFilename :String): Unit = {
    val bis = new BufferedInputStream((new FileInputStream(filename)))
    val bos = new BufferedOutputStream(new FileOutputStream(decFilename))

    //    var writedData = 0
    var byte : Byte = 0
//    var num = 1
    var bit = 0
    var out : String = ""
    var tmp : String = ""
    val base = key.enTree.size
    var numberBuilder : String = ""
    while (bis.available() > 3) {
      byte = bis.read().toByte
      for ( i <- 1 to 8) {
        bit = byte >> (8 - i) & 1
        tmp += bit.toString

        var res = key.enTree.indexOf(tmp)

        if (res != -1) {
          tmp = ""
          res += base
          numberBuilder = ""
          while(res > 1) {
            numberBuilder = (if (res % 2 == 0) "0" else "1") + numberBuilder
            res /= 2
          }
          out += numberBuilder
          if (out.length >= 8) {
            bos.write(Integer.parseInt(out.substring(0,8), 2).toByte)
            out = out.substring(8)
          }
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

      var res = key.enTree.indexWhere{
        case p => p.equals(tmp)
        case _ => false
      }

      if (res != -1) {
        //          tmp = tmp.substring(enTree(res).length)
        tmp = ""
        res += base
        numberBuilder = ""
        while(res > 1) {
          numberBuilder = (if (res % 2 == 0) "0" else "1") + numberBuilder
          res /= 2
        }
        out += numberBuilder
        if (out.length >= 8) {
          bos.write(Integer.parseInt(out.substring(0,8), 2).toByte)
          out = out.substring(8)
        }
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

  override def findKey(enFilename :String): BitwiseModularEncryptionKey = {
    val numberOfBitForCheck = 32
    val baseBank = Array(3, 5, 6, 7, 9)
    var byte : Byte = 0
    var num = 1
    var bit = 0
    var out : String = ""
    for (base <- baseBank) {
      val trees = listOfTree(base)
      for (tree <- trees) {
        println("testing for key: " + tree)
        val orgBis = new BufferedInputStream((new FileInputStream(filename)))
        while (out.length < numberOfBitForCheck) {
          byte = orgBis.read().toByte
          for ( i <- 1 to 8) {
            if (out.length < numberOfBitForCheck) {
              bit = byte >> (8 - i) & 1
              num *= 2
              num += bit
              if (num >= base) {
                num -= base
                out += (tree(num))
                num = 1

              }
            }
          }
        }
        num = 1
        val enBis = new BufferedInputStream((new FileInputStream(enFilename)))
//        val enIntData = Integer.parseInt(out.substring(0,numberOfBitForCheck), 2)
//        val enData :Array[Byte] = ByteBuffer.allocate(4).putChar(enIntData.toChar).array()
        val enData = out.substring(0, numberOfBitForCheck)
        val orgEnData :Array[Byte] = new Array[Byte](numberOfBitForCheck / 8)
        enBis.read(orgEnData)
        val tmp = Array[Byte](0) ++ orgEnData
        var orgData = BigInt(tmp).toString(2)
        orgData = repeatChar('0', 32 - orgData.length) + orgData
        if (orgData.equals(enData)) {
          println("I find the key ;)")
          return new BitwiseModularEncryptionKey(base, tree.toArray)
        }
        enBis.close()
        orgBis.close()
        out = ""
      }
    }
    return null
  }

  private def listOfTree(base :Int): List[List[String]] = {
    val pureList = base match {
      case 3 => List("00", "01", "1")
      case 5 => List("000", "001", "01", "10", "11")
      case 6 => List("000", "001", "010", "011", "10", "11")
      case 7 => List("000", "001", "010", "011", "100", "101", "11")
      case 9 => List("0000", "0001", "001", "010", "011", "100", "101", "110", "111")
    }
    return pureList.permutations.toList
  }
}
