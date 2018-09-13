package encryption

/**
  * Created by peyman on 10/15/2017.
  */
abstract class Encryption {
  def encrypt(filename: String)
  def decrypt(filename: String)
  def findKey(enFilename: String) : Key

  def repeatChar(char:Char, n: Int) = List.fill(n)(char).mkString

  protected def getBitLength(m :Int): Int = {
    var bitLen = 0
    var num = m
    while ( num > 0 ) {
      num /= 2
      bitLen += 1
    }
    return bitLen
  }

  protected def toBinary(n:Int, bin: List[Int] = List.empty[Int]): String = {
    if (n==0) return "0"
    if (n==1) return "1"
    if (n/2 == 1) (1:: (n % 2) :: bin).mkString("")
    else {
      val r = n % 2
      val q = n / 2
      toBinary(q, r::bin)
    }
  }

  protected def toBinary(n:Int, len:Int): String = {
    val res = toBinary(n)
    return  ("0" * (len - res.length)) + res
  }
}
