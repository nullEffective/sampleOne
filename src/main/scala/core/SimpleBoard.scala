import java.util.logging.Logger

class SimpleBoard(a: Array[Array[String]] = Array.fill(0, 0)("#")) {

  val logger = Logger.getLogger(getClass.getName)
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

  def getLargestBox(): Int = {
    if (solBoxList.isEmpty) {
      return -1
    }
    solBoxList(0).size()
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

  def resolve(row: Int, col: Int): String = {
    var value: String = arr(row)(col)
    if (!blobMap.contains(value)) {
      return "-1"
    }
    var resolve = blobMap(value)
    while (value != resolve) {
      value = resolve
      resolve = blobMap.getOrElse(value, value + "!")
    }
    resolve
  }

  def set(row: Int, col: Int, value: String) = {
    arr(row)(col) = value
  }

  def displayResolve(): String = {
    val sb: StringBuilder = StringBuilder()
    sb.append("\n")
    val space = " "
    for (row <- 0 until rows) {
      for (col <- 0 until cols) {
        val v = resolve(row, col).toInt
        if (v < 0) {
          sb.append(space)
        } else {
          val display : Char = (v + 'a'.toInt).toChar
          //val s = f"$display%02d"
          sb.append(s"$display")
        }
      }
      sb.append(s": $row\n")
    }
    logger.info(sb.toString())
    sb.toString()
  }

  def display(): Unit = {
    val sb: StringBuilder = StringBuilder()
    sb.append("\n")
    for (r <- arr) {
      sb.append(r.mkString(""))
      sb.append("\n")
    }
    logger.info(sb.toString())
  }

  def blobListSize(): Int = {
    val s = blobMap.values.toSet
    s.size
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
