package math

import scala.collection.mutable.ListBuffer
/**
  * Created by peyman on 1/10/2018.
  */
class ModMatrix (data: List[List[Mod]]) {

  var rows = data
  var mod: Long = 0
  if (rows.isEmpty) {
    mod = 0
  } else {
    mod = rows(0)(0).modulus
  }

  def this(data: List[List[Number]], mod: Int) {
    this(data.map(row => row.map(item => Mod(item.longValue(), mod.toLong))))
  }

  def swapRow(i: Int, j: Int): Unit = {
    val tmp = rows(i)
    rows = rows.updated(i, rows(j)).updated(j, tmp)
  }

  def setItem(i: Int, j: Int, value: Mod): Unit = {
    rows = rows.updated(i, rows(i).updated(j, value))
  }

  def +(other: ModMatrix): ModMatrix = {
    require(this.rows.length == other.rows.length && this.rows(0).length == other.rows(0).length)
    var res = new ListBuffer[List[math.Mod]]()
    for (i <- 0 to rows.length -1) {
      val row = new ListBuffer[math.Mod]()
      for (j <- 0 to rows(0).length - 1) {
        row += rows(i)(j) + other.rows(i)(j)
      }
      res += row.toList
    }
    new ModMatrix(res.toList)
  }

  def unary_-(): ModMatrix = {
    new ModMatrix(rows.map(row => row.map(item => -item)))
  }

  def -(other: ModMatrix): ModMatrix = {
    require(this.rows.length == other.rows.length && this.rows(0).length == other.rows(0).length)
    this + (-other)
  }

  def *(other: ModMatrix): ModMatrix = {
    require(this.rows(0).length == other.rows.length)
    var res = new ListBuffer[List[math.Mod]]()
    for (i <- 0 to rows.length -1) {
      val row = new ListBuffer[math.Mod]()
      for (j <- 0 to other.rows(0).length - 1) {
        var s = Mod.zero(mod)
        for ( k <- 0 to rows(0).length - 1) {
          s += rows(i)(k) * other.rows(k)(j)
        }
        row += s
      }
      res += row.toList
    }
    new ModMatrix(res.toList)
  }
//
  def inv(): ModMatrix = {
    val mcpy = new ModMatrix(rows)
    val size = rows.length
    val imat = ModMatrix(size, mod.toInt)
    for ( i <- size-1 to 1 by -1) {
      if ( rows(i-1)(0) < rows(i)(0)) {
        swapRow(i-1, i)
        imat.swapRow(i-1, i)
      }
    }

    for ( i <- 0 to size-1) {
      for ( j <- 0 to size-1 ) {
        if ( i != j ) {

          if (rows(i)(i) == Mod.zero(mod)) {
            var pivotFlag = 0
            for ( r <- i to size-1) {
              if (rows(i)(r) != Mod.zero(mod)) {
                swapRow(i, r)
                imat.swapRow(i, r)
                pivotFlag = 1
              }
            }
            if (pivotFlag == 0) {
              return ModMatrix(0, mod.toInt)
            }
          }
          val tmp = rows(j)(i) / rows(i)(i)
          for ( k <- 0 to size-1) {
            var value = rows(j)(k) - rows(i)(k) * tmp
            setItem(j, k, value)
            value = imat.rows(j)(k) - imat.rows(i)(k) * tmp
            imat.setItem(j, k, value)

          }
        }
      }
    }

    for ( i <- 0 to size-1 ) {
      val tmp = rows(i)(i)
      for ( j <- 0 to size-1 ) {
        setItem(i, j, rows(i)(j) / tmp)
        imat.setItem(i, j, imat.rows(i)(j) / tmp)
      }
    }
    rows = mcpy.rows
    return imat
  }


  override def toString: String = {
    val str = rows.map(row => row.map( item => item.toString).mkString(" ")).mkString("\n")
    str
  }
}

object ModMatrix {
  def apply(data: List[List[Mod]]): ModMatrix = new ModMatrix(data)

  def apply(size: Int, mod: Int): ModMatrix = {
    var res = new ListBuffer[List[Mod]]
    for ( i <- 0 to size-1 ) {
      val row = new ListBuffer[Mod]
      for (j <- 0 to size-1) {
        if ( i == j){
          row += Mod.one(mod.toLong)
        } else {
          row += Mod.zero(mod.toLong)
        }
      }
      res += row.toList
    }
    new ModMatrix(res.toList)
  }
}