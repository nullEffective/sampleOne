
class SimpleBoard(a: Array[Array[String]]) {
  val rows = a.length
  val cols = a(0).length
  val blobMap: collection.mutable.Map[String, String] = collection.mutable.Map[String, String]()
  private val arr: Array[Array[String]] = a
  var solBoxList: List[SimpleBox] = List.empty

  override def toString: String = {
    solBoxList.mkString("\n")
  }

  def get(row: Int, col: Int): String = {
    arr(row)(col)
  }

  def getSolutionString(): String = {
    solBoxList.mkString("\n")
  }

  def getBoxList(): List[SimpleBox] = {
    val boxMap: collection.mutable.Map[String, SimpleBox] = collection.mutable.Map[String, SimpleBox]()
    for (row <- 0 until rows) {
      for (col <- 0 until cols) {
        var option = getResolved(row, col)
        if option.nonEmpty then
          val box = boxMap.getOrElse(option.get, SimpleBox(option.get))
          box.augment(row, col)
          boxMap(option.get) = box
      }
    }
    boxMap.values.toList
  }

  def getResolved(row: Int, col: Int): Option[String] = {
    val v = resolve(row, col)
    if (v.equalsIgnoreCase("-1")) {
      return None
    }
    Some(v)
  }

  def set(row: Int, col: Int, value: String) = {
    arr(row)(col) = value
  }

  def display(): Unit = {
    for (r <- arr) {
      println(r.mkString)
    }
  }

  def displayResolve(): Unit = {
    for (row <- 0 until rows) {
      for (col <- 0 until cols) {
        val v = resolve(row, col).toInt
        if (v < 0) {
          print("-")
        } else {
          val s = f"$v%01d"
          print(s"$s")
        }
      }
      println(s": $row")
    }
  }

  def resolve(row: Int, col: Int): String = {
    var value: String = arr(row)(col)
    if (!blobMap.contains(value)) {
      return "-1"
    }
    var resolve = blobMap(value)
    while (value != resolve) {
      //println(s"resolving $value:$resolve")
      value = resolve
      resolve = blobMap.getOrElse(value, value + "!")
    }
    resolve.toString
  }

  def blobSize(): Int = {
    blobMap.values.toSet.size
  }
  /*
    def visitorValue(func: (v: String) => Unit): Unit = {
      for (row <- 0 until rows) {
        for (col <- 0 until cols) {
          val value = arr(row)(col)
          func(value)
        }
      }
    }
  
    def visitor(func: (r: Int, c: Int, v: String) => Unit): Unit = {
      for (row <- 0 until rows) {
        for (col <- 0 until cols) {
          val value = arr(row)(col)
          func(row, col, value)
        }
      }
    }
  */
}
