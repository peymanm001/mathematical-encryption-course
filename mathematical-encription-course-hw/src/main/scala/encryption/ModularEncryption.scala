package encryption

import java.io.{BufferedInputStream, BufferedOutputStream, FileInputStream, FileOutputStream}
import java.nio.ByteBuffer

import scala.collection.mutable.HashMap

/**
  * Created by peyman on 10/13/2017.
  */
class ModularEncryption(fName: String, key: ModularEncryptionKey) extends Encryption {
  val filename = fName

  override def encrypt(enFilename:String): Unit = {
    val bis = new BufferedInputStream((new FileInputStream(filename)))
    val bos = new BufferedOutputStream(new FileOutputStream(enFilename))

    val buff :Array[Byte] = new Array[Byte](key.bSize)
    var outData : BigInt = null
    var block = ""

    // write fileSize
    bos.write(ByteBuffer.allocate(4).putInt(bis.available()).array())

    while (bis.read(buff) != -1) {
      outData = BigInt(buff)
      block = outData.toString(key.enTable.size)
      val out = block.map(
        c => key.enTable.getOrElse(c, c)
      )
      if (out.charAt(0) == '-'){
        outData = BigInt( "-1".concat(out.substring(1)), key.enTable.size)
      } else {
        outData = BigInt( "1".concat(out), key.enTable.size)
      }
//      bos.write(outData.toByteArray.length)
      bos.write(ByteBuffer.allocate(4).putInt(outData.toByteArray.length).array())
      bos.write(outData.toByteArray)
    }
    //    println(writedData)
    bis.close()
    bos.close()
  }

  override def decrypt(decFilename:String): Unit = {
    val bis = new BufferedInputStream((new FileInputStream(filename)))
    val bos = new BufferedOutputStream(new FileOutputStream(decFilename))
    val decTable = key.enTable.map(_.swap)

    val fileSizeBuffer : Array[Byte] = new Array[Byte](4)
    bis.read(fileSizeBuffer)
    val fileSize = ByteBuffer.wrap(fileSizeBuffer).getInt
    val bufferSize = key.bSize
    //    bis.read(buff)
    var blockSize = 0
    val blockSizeBuff :Array[Byte] = new Array[Byte](4)

    //    println(blockSize)
    val outData : Array[Byte] = new Array[Byte](bufferSize)
    var block = ""
    var writtedData = 0
    while(bis.available() > 0) {
      bis.read(blockSizeBuff)
      blockSize = ByteBuffer.wrap(blockSizeBuff).getInt
      val buff : Array[Byte] = new Array[Byte](blockSize)
      bis.read(buff)
      block = BigInt(buff).toString(key.enTable.size)

      if (block.charAt(0) == '-'){
        block = "-".concat(block.substring(2))
      } else {
        block = block.substring(1)
      }
      val out = block.map(
        c => decTable.getOrElse(c, c)
      )
      BigIntToByteArray(outData, BigInt(out, key.enTable.size))
      // can be better
      // remove if and put contents of it after while
      if (bis.available() == 0) {
        bos.write(outData,0, fileSize - writtedData)
        writtedData += fileSize - writtedData
      } else {
        bos.write(outData)
        writtedData += bufferSize
      }
    }
    bis.close()
    bos.close()

  }

  private def BigIntToByteArray(byteArray: Array[Byte], num: BigInt): Unit = {
    val byteArrayNum = num.toByteArray
    var sign = 0
    if (num < 0) {
      sign = -1
    }
    for( i <- 0 to (byteArray.length - byteArrayNum.length - 1)) {
      byteArray(i) = sign.toByte
    }
    val diff = byteArray.length - byteArrayNum.length
    for( i <- 0 to (byteArrayNum.length - 1)) {
      byteArray(diff + i) = byteArrayNum(i)
    }
  }

  override def findKey(enFilename :String): ModularEncryptionKey = {
    val enBis = new BufferedInputStream((new FileInputStream(enFilename)))

    val byteBuffer_4 : Array[Byte] = new Array[Byte](4)
    enBis.read(byteBuffer_4)

    enBis.read(byteBuffer_4)
    val firstBlockSize = ByteBuffer.wrap(byteBuffer_4).getInt

    val buff : Array[Byte] = new Array[Byte](firstBlockSize)
    enBis.read(buff)
    val block = BigInt(buff)
    return bruteForce(firstBlockSize, block)

  }

  private def bruteForce(maxBlockSize: Int, block :BigInt): ModularEncryptionKey = {
//    val orgBis = new BufferedInputStream((new FileInputStream(filename)))
    val baseBank = Array(3, 5, 6, 7, 9)
    for (base <- baseBank) {
      val lists = List.range((48 + 0).toChar, (48 + base).toChar).permutations.toList
      for ( table <- lists) {
        println("testing for key:" + table)
        var decBlock = block.toString(base)
        if (decBlock.charAt(0) == '-'){
          decBlock = "-".concat(decBlock.substring(2))
        } else {
          decBlock = decBlock.substring(1)
        }
        val out = decBlock.map(
//          c => if (c == '-') '-' else table(c.toString.toInt).toChar
          c => if (c == '-') '-' else table.indexOf(c).toString.charAt(0)
        )
        List.range(maxBlockSize,1, -1).map(
          blockSize => {
            val outData :Array[Byte] = new Array[Byte](blockSize)
            val orgData :Array[Byte] = new Array[Byte](blockSize)
            if (BigInt(out, base).toByteArray.length <= blockSize ){
              BigIntToByteArray(outData, BigInt(out, base))
              val orgBis = new BufferedInputStream((new FileInputStream(filename)))
              orgBis.read(orgData)
              orgBis.close()
              if (outData.deep == orgData.deep) {
                println("I find the key :)")
                val newTable: HashMap[Char, Char] = new HashMap[Char, Char]
                for ( j <- 0 to (base-1) ) {
                  newTable.put(j.toString.charAt(0), table(j))
                }
                return new ModularEncryptionKey(base, blockSize, newTable)
              }
            }
          }
        )
      }
    }
    return null
  }
}
