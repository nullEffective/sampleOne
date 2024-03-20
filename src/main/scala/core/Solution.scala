

import java.nio.file.*
import java.util.logging.Logger
import scala.collection.JavaConverters.*
import scala.collection.mutable.ListBuffer
import scala.io.StdIn.readLine


object SolutionOne {

  val logger = Logger.getLogger(getClass.getName)
  //val board = input(inputString)

  def main(args: Array[String]): Unit = {

    var inputString = ""
    val inputList: ListBuffer[String] = ListBuffer[String]()
    if (args.isEmpty) {
      inputList :+ readLines()
    } else {
      val fileName = args(0)
      readFromFiles(fileName, inputList)
    }

    for (inputString: String <- inputList) {
      val board = input(inputString)
      if (board.solBoxList.isEmpty) {
        System.exit(9)
      }
      println(board.getSolutionString())
      println()
    }
    System.exit(0)
  }

  def readFromFiles(fileName: String, inputList: ListBuffer[String]): Unit = {
    val path: Path = FileSystems.getDefault.getPath(fileName)
    val fileList = ListBuffer[String]()
    if (Files.isDirectory(path)) {
      val files = Files.list(path).iterator().asScala.toList
      for (f <- files) {
        fileList.addOne(f.toString)
      }
      println(fileList.mkString(" "))
    } else {
      fileList += fileName
    }
    for (f: String <- fileList) {
      val path = FileSystems.getDefault.getPath(f)
      val s = scala.io.Source.fromFile(f).mkString
      inputList += s
    }
  }

  def input(input: String): SimpleBoard = {
    val split = checkInput(input)
    val board = createBoard(split)
    findBlobs(board)
    val boxList = findBoxes(board)
    val solution = solve(boxList)
    solution.isEmpty match {
      case true => new Throwable("No solution found")
      case _ => board.solBoxList = solution
    }
    board
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

  def createBoard(split: Array[String]): SimpleBoard = {
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
    new SimpleBoard(arr)
  }

  def findBlobs(board: SimpleBoard): SimpleBoard = {
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
          if (!blobMap.contains(currentBlob)) {
            blobMap(currentBlob) = currentBlob
          }
        } else {
          currentBlob = (row + blobMap.size).toString
        }
      }
      currentBlob = (row + blobMap.size).toString
    }

    board
  }

  def findBoxes(board: SimpleBoard): List[SimpleBox] = {
    val list = board.getBoxList()
    list;
  }

  def solve(boxList: List[SimpleBox]): List[SimpleBox] = {
    var r: Boolean = false

    for (i <- 0 until boxList.length) {
      val box = boxList(i)
      for (j <- i + 1 until boxList.length) {
        val otherBox = boxList(j)
        if (!box.isvalid) {
        } else {
          for (otherBox <- boxList) {
            if (box.intersect(otherBox)) {
              box.setInvalid()
              otherBox.setInvalid()
            }
          }
        }
      }
    }
    val resultList = boxList.filter(n => n.isInvalid())
    if (resultList.isEmpty) {
      return resultList
    }
    var largestBox: SimpleBox = resultList(0)
    for (box <- resultList) {
      if (box.size() > largestBox.size()) {
        largestBox = box
      }
    }
    resultList.filter(n => n.size() >= largestBox.size())
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

