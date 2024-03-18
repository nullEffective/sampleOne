
class Board(a: Array[Array[Char]]) {
  val rows = a.length
  val cols = a(0).length
  private val arr: Array[Array[Char]] = a

  def get(row: Int, col: Int): Char = {
    arr(row)(col)
  }

  def set(row: Int, col: Int, value: Char) {
    arr(row)(col) = value
  }

  def display(): Unit = {
    for (r <- arr) {
      println(r.mkString)
    }
  }
}
