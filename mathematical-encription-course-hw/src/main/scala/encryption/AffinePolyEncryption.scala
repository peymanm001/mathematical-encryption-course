package encryption

import java.io.{BufferedInputStream, BufferedOutputStream, FileInputStream, FileOutputStream}
import java.nio.ByteBuffer

import math.{Mod, ModMatrix, ModPoly, Term}

import scala.collection.mutable.ListBuffer

/**
  * Created by peyman on 1/23/2018.
  */
class AffinePolyEncryption(fName: String, key: AffinePolyKey) extends Encryption{
  val filename :String = fName

  override def encrypt(enFilename: String): Unit = {
    val bis = new BufferedInputStream((new FileInputStream(filename)))
    val bos = new BufferedOutputStream(new FileOutputStream(enFilename))

    val buff :Array[Byte] = new Array[Byte](key.xorBlockSize)

//     write fileSize
    bos.write(ByteBuffer.allocate(4).putInt(bis.available()).array())

    var byte: Byte = 0
    var num = 1
    var bit = 0
    val affineLst :ListBuffer[List[Mod]] = ListBuffer()
    val bitLen = getBitLength(key.M)
    var out = ""
    var available = bis.available()
    while (bis.read(buff) != -1) {
      for (i <- 0 to Math.min(buff.length - 1, available)) {
        byte = (buff(i) ^ key.xorKey(i)).toByte
        for (j <- 1 to 8) {
          bit = byte >> (8 - j) & 1
          num *= 2
          num += bit
          if (num >= key.M) {
            num -= key.M
            affineLst.append(List(Mod(num, key.M)))
            if (affineLst.length == key.affineBlockSize) {
              val affineCipher = key.affineHillH * ModMatrix(affineLst.toList) + key.affineHillB
              val polyPlain = ModPoly(affineCipher.rows.zip(key.affineBlockSize - 1 to 0 by -1)
                .filter(_._1(0).residue !=0).map(t => Term(t._1(0), t._2)), key.M)
              val polyCipher = (polyPlain * key.polyA + key.polyB) % key.polyMod
              val outData = polyCipher.getCoefList(key.affineBlockSize - 1).map(term => term.residue.toInt)
              for (elm <- outData) {
                out += toBinary(elm, bitLen)
                while (out.length >= 8) {
                  bos.write(Integer.parseInt(out.substring(0, 8), 2).toByte)
                  out = out.substring(8)
                }
              }
              affineLst.clear()
            }
            num = 1
          }
        }
      }
      available = bis.available()
      if (bis.available() == 0 && (affineLst.length != 0 || num != 1 )) {
        num *= 2
        num += 1
        while ( affineLst.length < key.affineBlockSize) {
          while ( num < key.M) {
            num *= 2
          }
          num -= key.M
          affineLst.append(List(Mod(num, key.M)))
          num = 1
        }
        val affineCipher = key.affineHillH * ModMatrix(affineLst.toList) + key.affineHillB
        val polyPlain = ModPoly(affineCipher.rows.zip(key.affineBlockSize - 1 to 0 by -1)
          .filter(_._1(0).residue !=0).map(t => Term(t._1(0), t._2)), key.M)
        val polyCipher = (polyPlain * key.polyA + key.polyB) % key.polyMod
        val outData = polyCipher.getCoefList(key.affineBlockSize - 1).map(term => term.residue.toInt)
        for (elm <- outData) {
          out += toBinary(elm, bitLen)

          while (out.length >= 8) {
            bos.write(Integer.parseInt(out.substring(0, 8), 2).toByte)
            out = out.substring(8)
          }
        }
        affineLst.clear()
      }
    }

    while ( out.length % 8 != 0) out += "0"
    while (out.length >= 8) {
      bos.write(Integer.parseInt(out.substring(0, 8), 2).toByte)
      out = out.substring(8)
    }
//    val remainBit = out.length % 8
//    if (remainBit != 0) {
//      out += ("0" * (8 - remainBit))
//      bos.write(Integer.parseInt(out, 2).toByte)
//    }
//    bos.write(num.toByte)
//    bos.write(remainBit.toByte)
    bis.close()
    bos.close()
  }

  override def decrypt(decFilename :String): Unit = {
    val bis = new BufferedInputStream((new FileInputStream(filename)))
    val bos = new BufferedOutputStream(new FileOutputStream(decFilename))

    val fileSizeBuffer : Array[Byte] = new Array[Byte](4)
    bis.read(fileSizeBuffer)
    val fileSize = ByteBuffer.wrap(fileSizeBuffer).getInt
    var writedData = 0

    val buff :Array[Byte] = new Array[Byte](key.xorBlockSize)
    var byte: Byte = 0
    var num = 1
    var bit = 0
    val polyLst :ListBuffer[Term] = ListBuffer()
    val bitLen = getBitLength(key.M)
    var out = ""
    var tmp = ""
    var numberBuilder = ""
    var xorKeyCounter = 0
    while (bis.available() > 0) {
      byte = bis.read().toByte
      for (i <- 1 to 8) {
        bit = byte >> (8 - i) & 1
        tmp += bit.toString
        if (tmp.length == bitLen) {
          num = Integer.parseInt(tmp, 2)
          polyLst.append(Term(Mod(num, key.M), key.affineBlockSize - polyLst.length - 1))
          if (polyLst.length == key.affineBlockSize) {
            val polyPlain = ((ModPoly(polyLst.toList, key.M) - key.polyB) * key.polyA.inverse(key.polyMod)) % key.polyMod
            val coefLst = polyPlain.getCoefList(key.affineBlockSize - 1)
            val cAffine = ModMatrix(coefLst.map(coef => List(coef)))
            val plainAffine = key.affineHillH.inv() * (cAffine - key.affineHillB)
            val plainLst = plainAffine.rows.map(row => row(0).residue)
            for (num <- plainLst) {
              var res = num + key.M
              numberBuilder = ""
              while (res > 1) {
                numberBuilder = (if (res % 2 == 0) "0" else "1") + numberBuilder
                res /= 2
              }
              out += numberBuilder
              while(out.length >= 8) {
                bos.write(Integer.parseInt(out.substring(0, 8), 2).toByte ^ key.xorKey(xorKeyCounter))
                xorKeyCounter += 1
                xorKeyCounter %= key.xorBlockSize
                out = out.substring(8)
                writedData += 1
                if (writedData == fileSize) {
                  bis.close()
                  bos.close()
                  return
                }
              }
            }
            polyLst.clear()
          }
          tmp = ""
        }
      }
    }
    bis.close()
    bos.close()
  }

  override def findKey(enFilename :String): BitwiseModularEncryptionKey = {
//    val numberOfBitForCheck = 32
//    val baseBank = Array(3, 5, 6, 7, 9)
//    var byte : Byte = 0
//    var num = 1
//    var bit = 0
//    var out : String = ""
//    for (base <- baseBank) {
//      val trees = listOfTree(base)
//      for (tree <- trees) {
//        println("testing for key: " + tree)
//        val orgBis = new BufferedInputStream((new FileInputStream(filename)))
//        while (out.length < numberOfBitForCheck) {
//          byte = orgBis.read().toByte
//          for ( i <- 1 to 8) {
//            if (out.length < numberOfBitForCheck) {
//              bit = byte >> (8 - i) & 1
//              num *= 2
//              num += bit
//              if (num >= base) {
//                num -= base
//                out += (tree(num))
//                num = 1
//
//              }
//            }
//          }
//        }
//        num = 1
//        val enBis = new BufferedInputStream((new FileInputStream(enFilename)))
//        //        val enIntData = Integer.parseInt(out.substring(0,numberOfBitForCheck), 2)
//        //        val enData :Array[Byte] = ByteBuffer.allocate(4).putChar(enIntData.toChar).array()
//        val enData = out.substring(0, numberOfBitForCheck)
//        val orgEnData :Array[Byte] = new Array[Byte](numberOfBitForCheck / 8)
//        enBis.read(orgEnData)
//        val tmp = Array[Byte](0) ++ orgEnData
//        var orgData = BigInt(tmp).toString(2)
//        orgData = repeatChar('0', 32 - orgData.length) + orgData
//        if (orgData.equals(enData)) {
//          println("I find the key ;)")
//          return new BitwiseModularEncryptionKey(base, tree.toArray)
//        }
//        enBis.close()
//        orgBis.close()
//        out = ""
//      }
//    }
    return null
  }

}
