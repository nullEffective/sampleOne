
import org.scalatest.Assertions.{getClass, *}
import org.scalatest.flatspec.AnyFlatSpec

import java.util.logging.*


class SolutionTest extends AnyFlatSpec {

  val logger = Logger.getLogger(getClass.getName)
  {
    logger.setLevel(Level.ALL)
  }

  "Board checks bad characters" should "throw exception" in {
    assertThrows[IllegalArgumentException] {
      SolutionOne.checkInput("x")
    }
  }

  "Board checks good characters" should "not throw exceptions" in {
    SolutionOne.checkInput("-")
    SolutionOne.checkInput("*")
  }

  "Check good solution" should "should equal solutionBox" in {
    val actualBox = SolutionOne.input("-*\n-*")
  }

  "Check blobs O" should "should equal solutionBox" in {
    val inputO =
      """|------
         |-****-
         |**--**
         |-****-"""
        .stripMargin
    validate(inputO, 1, 1, 1)
  }

  "Check blobs F" should "should equal solutionBox" in {
    val input: String =
      """|------
         |----*-
         |----*-
         |---**-
         |---*--
         |---*--
         |------""".stripMargin
    validate(input, 4, 1, 1, 10)
  }
  
  "Check blobs Reverse C" should "should equal solutionBox" in {
    val inputReverseC: String =
      """|------
         |-****-
         |----**
         |--****""".stripMargin
    validate(inputReverseC, 1, 1, 1)
  }

  "Check blobs  n" should "should equal solutionBox" in {
    val inputN =
      """|------
         |-****-
         |-*--**
         |-*--**""".stripMargin
    validate(inputN, 1, 1, 1)
  }

  "Check two blobs" should "should equal solutionBox" in {

    val inputTwoBlobs =
      """|------
         |-****-
         |-*-**-
         |------
         |--**-*
         |--****""".stripMargin
    validate(inputTwoBlobs, 1, 1, 1)
  }

  "Check blob Z" should "should equal Z" in {
    val inputZ: String =
      """|------
         |-****-
         |---**-
         |--*--*
         |*****-
         |------""".stripMargin
    validate(inputZ, 3, 1, 3, 4)
  }

  "Check blob sample2" should "should equal sample2" in {
    val inputSample2 =
      """|**-------***
         |-*--**--***-
         |-----***--**
         |-------***--
      """.stripMargin
    val (result, board) = validate(inputSample2, 5, 1, 3, 4)
    val actualSol: String = board.getSolutionString()
    val expected = "(1,1)(2,2)"

    val v: Boolean = expected.equals(actualSol)
    assert(v, s"$expected does not equal $actualSol")
  }

  def validate(input: String,
               expectedBlobs: Int,
               solLength: Int,
               boxLength: Int,
               largestSize: Int = 4): (Boolean, SimpleBoard) = {
    println(input)
    var board: SimpleBoard = SolutionOne.createBoard("".split("\n"))
    var result = true
    try {

      board = SolutionOne.input(input)
      logger.info("pre blobbed....")
      board.display()
      logger.info("blobbed....")
      board.displayResolve()

      val actualBlobSize = board.blobListSize()
      val actualResultString = board.getSolutionString()
      val actualSolLength = board.solBoxList.size

      equal(actualBlobSize, expectedBlobs, "actual blob length not equal ")
      equal(actualSolLength, solLength, "actual solution length is not ")
      equal(board.getBoxList().size, boxLength, "actual  box length is not equal")
      equal(board.getLargestBox(), largestSize, "actual  box area not equal")

      logger.info(s"solution:")
      logger.info(s" $actualResultString")
      //logger.info(s" ${board.getBoxList().mkString(",")}")

    } catch {
      case e: Throwable => e.printStackTrace()
        result = false
    }
    (result, board)
  }

  def equal(actual: Int, expected: Int, message: String = "is not equal"): Unit = {
    val m = s"-actual=${actual} ${message} expected=$expected"
    assert(actual.equals(expected), m)
  }

  "Test intersection" should "Conflict" in {
    val box1 = SimpleBox("1").augment(0, 0).augment(1, 1)
    val box2 = SimpleBox("2").augment(0, 8).augment(2, 11)
    val box3 = SimpleBox("3").augment(1, 4).augment(3, 9)
    assert( box2.intersect(box3), "box2 should intersect with box3")
    assert(box3.intersect(box2), "box2 should intersect with box3")
    assert(!box1.intersect(box2), "box1 should Not intersect with box2")
    assert(!box1.intersect(box3), "box1 should Not intersect with box3")

    val boxList = List(box1,box2,box3)
    val resultList = SolutionOne.solve(boxList)
    
  }
}

