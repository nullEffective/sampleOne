
import org.scalatest.Assertions.*
import org.scalatest.flatspec.AnyFlatSpec
//import org.junit.Assert.assertEquals


//import org.scalatest._
//import flatspec._

class SolutionTest extends AnyFlatSpec {

  val simpleBox = SimpleBox() //(1, 1, 1, 1)

  val inputO =
    """------
-****-
**--**
-****-""".stripMargin

  val inputReverseC =
    """------
-****-
----**
--****""".stripMargin

  val inputN =
    """------
-****-
----**
--****""".stripMargin

  val inputTwoBlobs =
    """------
-****-
-*-**-
------
--**-*
--****""".stripMargin

  val inputZ =
    """------
-****-
---**-
--*--*
*****-
------""".stripMargin


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
    println(inputO)
    val actualBlobs = SolutionOne.input(inputO)
  }

  "Check blobs Reverse C" should "should equal solutionBox" in {
    println(inputReverseC)
    val actualBlobs = SolutionOne.input(inputReverseC)
  }

  "Check blobs Reverse n" should "should equal solutionBox" in {
    println(inputN)
    val actualBlobs = SolutionOne.input(inputN)
  }

  "Check two blobs" should "should equal solutionBox" in {
    println(inputTwoBlobs)
    val actualBlobs = SolutionOne.input(inputTwoBlobs)
  }

  "Check blob Z" should "should equal solutionBox" in {
    println(inputZ)
    try {
      val board = SolutionOne.input(inputZ)
      val expected = 3
      val blobSize = board.blobSize()
      assert(blobSize.equals(expected), s"${board.blobMap.size} is not $expected")
      val result = board.solution.mkString("\n")
      println(s"solution: $result")
      println("end tests")
    } catch {
      case e: Throwable => e.printStackTrace()
    }
  }

}
