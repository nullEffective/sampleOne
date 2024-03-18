
import scala.io.StdIn.readLine

case class SimpleBox(x: Int, y: Int, x2: Int, y2: Int)

object Hello {

  var numColumns = -1;
  var rows = -1;
  var cols = -1;

  def main(args: Array[String]) {
    val input = readLines()
    val split = checkBoard(input)
    val board = createBoard(split)
    val boxList = findBoxes(board)
    val solution = solve(boxList)
    println(solution)
    println("end trans.")
  }

  def solve(boxList: List[SimpleBox]): SimpleBox = {
    SimpleBox(-1, -1, -1, -1)
  }

  def findBoxes(board: Board): List[SimpleBox] = {
    val list = List[SimpleBox]()
    list
  }

  private def readLines(): String = {
    val builder: StringBuilder = new StringBuilder()

    var line = "!!!";

    while (!line.trim.equalsIgnoreCase("")) {
      line = readLine()
      if (line.length != 0) {
        builder.append(line)
        builder.append("\n")
      }
    }
    builder.toString()
  }

  def checkBoard(rawInput: String): Array[String] = {
    val split = rawInput.split("\n")
    if (split.isEmpty) {
      throw new IllegalArgumentException(s"Bad input, no board found. [$rawInput]")
    }
    val length = split(0).length
    for (line <- split) {
      println(s"checkboard: $line")
      if (line.length != length) {
        throw new IllegalArgumentException(s"Bad input, size mismatch $length not equal to ${line.length} line [$line]")
      }
      for (c <- line) {
        println(s"checkboard: invalid character $c")

        if (c != '-' && c != '*') {
          throw new IllegalArgumentException(s"Bad input, invalid character [$c] in line [$line]")
        }
      }
    }
    split
  }

  def createBoard(split: Array[String]): Board = {

    val rows = split.length
    val cols = split(0).length
    val arr: Array[Array[Char]] = Array.fill(rows, cols)('!');

    for (row <- 0 until split.length) {
      val line: String = split(row)
      for (col <- 0 until line.length) {
        val c = line.charAt(col)
        arr(row)(col) = c
      }
    }
    new Board(arr)
  }
}

