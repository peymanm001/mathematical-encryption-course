import encryption._
import math._

object FileEncryptor extends App {
//  val mod = 81
//  val peyman = Mod(63, mod)
////  println(SquareRoot.squareInPrimeMod(peyman, mod))
//  println(SquareRoot.squareInPowPrimeMod(peyman, 3, 4))
//
//  println(SquareRoot.squareInCompositeMod(Mod(134, 175), List((5,2), (7, 1))))
//
//
  println(SquareRoot.squareEquation(3, 5, 13, List((5, 1), (7, 1))))
  println(SquareRoot.squareEquation(3, 4, 13, List((5, 1), (7, 1))))
  println(SquareRoot.squareEquation(3, 4, 5, List((3, 2), (5, 1))))

//  val (pub, pr) = RSAKey.generate()
//  val (pub2, pr2) = RSAKey.generate()
//  pub.store("pubKey.key")
//  pub2.store("pubKey2.key")
//  pr.store("prKey.key")
//  pr2.store("prKey2.key")
//  println("keys generated:")
//  println(pub)
//  println(pr)
//  println(pub2)
//  println(pr2)
//
//  var rsa = new RSAEncryption("test.txt", pub)
//  rsa.encryptAndSign("rsaEn.txt", pr2)
//  println("file encrypted")
//  rsa = new RSAEncryption("rsaEn.txt", pr)
//  println(rsa.decryptAndValidate("rsaDec.txt", pub2))
//  println("file decrypted")

  val (pub, pr) = RSAKey.generate()
  println(pub)
  println(pr)
  println(RSAEncryption.findPrivateKey(pub))
  // start app
//  startApp()

  def startApp() {
    var inp = ""
    while (true) {
      println("-" * 40)
      println("1. Generate an Key")
      println("2. Encrypt a File")
      println("3. Decrypt a File")
      println("4. Break Key ;)")
      println("5. Exit")
      inp = readLine()
      inp.toInt match {
        case 1 => generateKey()
        case 2 => encryptFile()
        case 3 => decryptFile()
        case 4 => breakKey()
        case _ => System.exit(1)
      }
    }
  }

  def breakKey(): Unit = {
    def findKey(enType: Any): Unit = {
      var filename = ""
      var enFilename = ""
      var key :encryption.Key = null
      var enc :Encryption = null
      print("Enter filename to decrypt: ")
      filename = readLine()
      print("Enter encrypted filename: ")
      enFilename = readLine()
      println("Please Wait ... ")

      if (enType == classOf[ModularEncryption]) {
        enc = new ModularEncryption(filename, null)
        key = enc.findKey(enFilename)
      } else if (enType == classOf[BitwiseModularEncryption]) {
        enc = new BitwiseModularEncryption(filename, null)
        key = enc.findKey(enFilename)
      } else if (enType == classOf[AffineEncryption]) {
        enc = new AffineEncryption(filename, null)
        key = enc.findKey(enFilename)
      } else if (enType == classOf[Encryption]) {
        enc = new ModularEncryption(filename, null)
        key = enc.findKey(enFilename)
        if (key == null) {
          enc = new BitwiseModularEncryption(filename, null)
          key = enc.findKey(enFilename)
          if (key == null) {
            enc = new AffineEncryption(filename, null)
            key = enc.findKey(enFilename)
            if (key == null) {
              println("Can't find key! :(")
              return
            } else {
              println("AffineKey found :)")
            }
          } else {
            println("BitwiseModularKey found :)")
          }

        } else {
          println("ModularKey found :)")
        }
      }
      if (key == null) {
        println("can't find")
      } else {
        println(key.toString)
        println("done!")
      }

    }
    var inp = ""
    while (true) {
      println("Choose Encryption method: ")
      println("\t1. Modular Encryption")
      println("\t2. Bitwise Modular Encryption")
      println("\t3. Affine Encryption")
      println("\t4. I don't know :(")
      println("\t5. Back")
      inp = readLine()
      inp.toInt match {
        case 1 => findKey(classOf[ModularEncryption])
        case 2 => findKey(classOf[BitwiseModularEncryption])
        case 3 => findKey(classOf[AffineEncryption])
        case 4 => findKey(classOf[Encryption])
        case _ => return
      }
    }
  }


  def decryptFile(): Unit = {
    def decrypt(enType: Any): Unit = {
      var filename = ""
      var keyFilename = ""
      var key :Key = null
      var enc :Encryption = null
      var out = ""
      print("Enter filename to decrypt: ")
      filename = readLine()
      print("Enter key filename: ")
      keyFilename = readLine()
      print("Enter output filename: ")
      out = readLine()
      println("Please Wait ... ")

      if (enType == classOf[ModularEncryption]) {
        key = ModularEncryptionKey.load(keyFilename)
        enc = new ModularEncryption(filename, key.asInstanceOf[ModularEncryptionKey])
      } else if (enType == classOf[BitwiseModularEncryption]) {
        key = BitwiseModularEncryptionKey.load(keyFilename)
        enc = new BitwiseModularEncryption(filename, key.asInstanceOf[BitwiseModularEncryptionKey])
      } else if (enType == classOf[AffineEncryption]) {
        key = AffineEncryptionKey.load(keyFilename)
        enc = new AffineEncryption(filename, key.asInstanceOf[AffineEncryptionKey])
      } else if (enType == classOf[AffinePolyEncryption]) {
        key = AffinePolyKey.load(keyFilename)
        enc = new AffinePolyEncryption(filename, key.asInstanceOf[AffinePolyKey])
      } else if (enType == classOf[CRTEncryption]) {
        key = CRTKey.load(keyFilename)
        enc = new CRTEncryption(filename, key.asInstanceOf[CRTKey])
      }
      enc.decrypt(out)
      println("done!")

    }
    var inp = ""
    while (true) {
      println("Choose Encryption method: ")
      println("\t1. Modular Encryption")
      println("\t2. Bitwise Modular Encryption")
      println("\t3. Affine Encryption")
      println("\t4. AffinePoly Encryption")
      println("\t5. CRT Encryption")
      println("\t6. Back")
      inp = readLine()
      inp.toInt match {
        case 1 => decrypt(classOf[ModularEncryption])
        case 2 => decrypt(classOf[BitwiseModularEncryption])
        case 3 => decrypt(classOf[AffineEncryption])
        case 4 => decrypt(classOf[AffinePolyEncryption])
        case 5 => decrypt(classOf[CRTEncryption])
        case _ => return
      }
    }
  }

  def encryptFile(): Unit = {
    def encrypt(enType: Any): Unit = {
      var filename = ""
      var keyFilename = ""
      var key :Key = null
      var enc :Encryption = null
      var out = ""
      print("Enter filename to encrypt: ")
      filename = readLine()
      print("Enter key filename: ")
      keyFilename = readLine()
      print("Enter output filename: ")
      out = readLine()
      println("Please Wait ... ")

      if (enType == classOf[ModularEncryption]) {
        key = ModularEncryptionKey.load(keyFilename)
        enc = new ModularEncryption(filename, key.asInstanceOf[ModularEncryptionKey])
      } else if (enType == classOf[BitwiseModularEncryption]) {
        key = BitwiseModularEncryptionKey.load(keyFilename)
        enc = new BitwiseModularEncryption(filename, key.asInstanceOf[BitwiseModularEncryptionKey])
      } else if (enType == classOf[AffineEncryption]) {
        key = AffineEncryptionKey.load(keyFilename)
        enc = new AffineEncryption(filename, key.asInstanceOf[AffineEncryptionKey])
      } else if (enType == classOf[AffinePolyEncryption]) {
        key = AffinePolyKey.load(keyFilename)
        enc = new AffinePolyEncryption(filename, key.asInstanceOf[AffinePolyKey])
      } else if (enType == classOf[CRTKey]) {
        key = CRTKey.load(keyFilename)
        enc = new CRTEncryption(filename, key.asInstanceOf[CRTKey])
      }
      enc.encrypt(out)
      println("done!")

    }
    var inp = ""
    while(true) {
      println("Choose Encryption method: ")
      println("\t1. Modular Encryption")
      println("\t2. Bitwise Modular Encryption")
      println("\t3. Affine Encryption")
      println("\t4. AffinePolynomial Encryption")
      println("\t5. CRT Encryption")
      println("\t6. Back")
      inp = readLine()
      inp.toInt match {
        case 1 => encrypt(classOf[ModularEncryption])
        case 2 => encrypt(classOf[BitwiseModularEncryption])
        case 3 => encrypt(classOf[AffineEncryption])
        case 4 => encrypt(classOf[AffinePolyEncryption])
        case 5 => encrypt(classOf[CRTKey])
        case _ => return
      }
    }
  }

  def generateKey(): Unit = {
    def storeKey(key: Key): Unit = {
      var inp = ""
      print("Enter key filename: ")
      inp = readLine()
      key.store(inp)
      println("done!")
    }
    var inp = ""
    var key : Key= null
    while(true) {
      println("\t1. Modular Encryption Key")
      println("\t2. Bitwise Modular Encryption Key")
      println("\t3. Affine Encryption Key")
      println("\t4. AffinePoly Encryption Key")
      println("\t5. CRT Encryption Key")
      println("\t6. Back")
      inp = readLine()
      key = inp.toInt match {
        case 1 => ModularEncryptionKey.generate()
        case 2 => BitwiseModularEncryptionKey.generate()
        case 3 => AffineEncryptionKey.generate()
        case 4 => AffinePolyKey.generate()
        case 5 => CRTKey.generate()
        case _ => return
      }
      println(key.toString)
      println("1. Store")
      println("2. Generate Again")
      inp = readLine()
      inp.toInt match {
        case 1 => storeKey(key)
        case _ =>
      }
    }
  }
}

