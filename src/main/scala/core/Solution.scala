
import scala.io.StdIn.readLine

class SimpleBox(val value: String = "N") {

  var x: Int = Int.MaxValue
  var y: Int = Int.MaxValue
  var x2: Int = -1
  var y2: Int = -1

  def size(): Int = {
    (x2 - x) * (y2 - y)
  }

  def intersect(other: SimpleBox): Boolean = {
    if (other == this) {
      return false
    }
    intersect(x, y, other.x, other.y, other.x2, other.y2) &&
      intersect(x2, y2, other.x, other.y, other.x2, other.y2) &&
      intersect(x, y2, other.x, other.y, other.x2, other.y2) &&
      intersect(x2, y, other.x, other.y, other.x2, other.y2)
  }

  def intersect(x: Int, y: Int, otherX: Int, otherY: Int, otherX2: Int, otherY2: Int): Boolean = {
    otherX <= x && x <= otherX2 &&
      y >= otherY &&
      y <= otherY2
  }

  override def toString: String = {
    s"$value=[$x:$y,$x2:$y2]"
  }

  def augment(newX: Int, newY: Int): SimpleBox = {

    if (newX < x) {
      x = newX
    }
    if (newX > x2) {
      x2 = newX
    }
    if (newY < y) {
      y = newY
    }
    if (newY > y2) {
      y2 = newY
    }
    this
  }
}

object SolutionOne {

  def main(args: Array[String]): Unit = {
    val i = readLines()
    val solution = input(i)
    println(solution)
    println("---end trans---")
  }

  def input(input: String): Board = {
    val split = checkInput(input)
    val board = createBoard(split)
    findBlobs(board)
    val boxList = findBoxes(board)
    val solution = solve(boxList)
    solution.isEmpty match {
      case  true => new Throwable("No solution found")
      case _ => board.solution = solution
    }
    board
  }

  def createBoard(split: Array[String]): Board = {
    val rows = split.length
    val cols = split(0).length
    val arr: Array[Array[String]] = Array.fill(rows, cols)("#");

    for (row <- 0 until split.length) {
      val line: String = split(row)
      for (col <- 0 until line.length) {
        val c = line.charAt(col)
        arr(row)(col) = c.toString
      }
    }
    new Board(arr)
  }

  def findBlobs(board: Board): Board = {
    var currentBlob: String = board.blobMap.size.toString
    val blobMap = board.blobMap

    for (row <- 0 until board.rows) {
      for (col <- 0 until board.cols) {
        val value = board.get(row, col)
        var upValue: String = value
        var leftValue: String = value
        if (row > 0) {
          upValue = board.get(row - 1, col)
        }
        if (col > 0) {
          leftValue = board.get(row, col - 1)
        }
        if (value.equals("*")) {
          if (blobMap.contains(leftValue)) {
            blobMap(leftValue) = currentBlob
          }
          if (blobMap.contains(upValue)) {
            blobMap(upValue) = currentBlob
          }

          board.set(row, col, currentBlob)
          println(s"setting $row:$col $currentBlob")
          if (!blobMap.contains(currentBlob)) {
            blobMap(currentBlob) = currentBlob
          }
          //println(s" $row: $col = blobSize ${board.blobMap.size} current=$currentBlob right=[$leftValue up=$upValue]")
          //println("blobMap: "  + board.blobMap.mkString(","))
        } else {
          currentBlob = (row + blobMap.size).toString
        }
      }
      currentBlob = (row + blobMap.size).toString
    }

    println("pre blobbed....")
    board.display()
    println("blobbed....")

    board.displayResolve()
    board
  }

  def findBoxes(board: Board): List[SimpleBox] = {
    val list = board.getBoxList()
    list;
  }

  def solve(boxList: List[SimpleBox]): List[SimpleBox] = {
    var r: Boolean = false
    var copyList: List[SimpleBox] = boxList filterNot  (n => n.intersect(boxList(0)))

    while !r && !copyList.isEmpty do {
      for (box <- copyList) {
        val filterList: List[SimpleBox] = copyList filterNot  (n => n.intersect(boxList(0)))
        val s: Int = filterList.size
        val s2: Int = copyList.size
        r = s == s2
        copyList = filterList
      }
    }
    if (copyList.isEmpty) {
      return copyList
    }
    var result: SimpleBox = copyList(0)
    for (box <- copyList) {
      if (box.size() > result.size()) {
        result = box
      }
    }
    copyList filter (n => n.size() >= result.size())
  }

  def checkInput(rawInput: String): Array[String] = {
    val input = rawInput.trim
    val split = input.split("\n")
    if (split.isEmpty) {
      throw new IllegalArgumentException(s"Bad input, no board found. [$input]")
    }
    val length = split(0).length
    for (line <- split) {
      if (line.length != length) {
        throw new IllegalArgumentException(s"Bad input, size mismatch $length not equal to ${line.length}\nfirstLine $split(0)\nline [$line]")
      }
      for (c <- line) {
        if (c != '-' && c != '*') {
          throw new IllegalArgumentException(s"Bad input, invalid character [$c] in line [$line]")
        }
      }
    }
    split
  }

  private def readLines(): String = {
    val builder: StringBuilder = new StringBuilder()

    var line = "!!!";

    while (!line.trim.equalsIgnoreCase("")) {
      line = readLine()
      if (line.length != 0) {
        checkInput(line)
        builder.append(line)
        builder.append("\n")
      }
    }
    builder.toString()
  }

}

