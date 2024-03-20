class SimpleBox(val value: String = "N") {

  private var x: Int = Int.MaxValue
  private var y: Int = Int.MaxValue
  private var x2: Int = -1
  private var y2: Int = -1
  var isvalid = true

  def setInvalid(): Boolean = {
    isvalid = false
    isvalid
  }
  
  def isInvalid() : Boolean = {
    isvalid
  }
  
  def size(): Int = {
    (x2+1 - x) * (y2 + 1 - y)
  }

  def intersect(other: SimpleBox): Boolean = {
    if (other == this) {
      return false
    }

    intersect(x, y, other.x, other.y, other.x2, other.y2) ||
      intersect(x2, y2, other.x, other.y, other.x2, other.y2) ||
      intersect(x, y2, other.x, other.y, other.x2, other.y2) ||
      intersect(x2, y, other.x, other.y, other.x2, other.y2)
  }

  def intersect(
                 x: Int, y: Int,
                 otherX: Int, otherY: Int,
                 otherX2: Int, otherY2: Int): Boolean = {
    val v1 = x >=(otherX)
    val v2 = x <= otherX2
    val v3 = y >= otherY
    val v4 = y <= otherY2
    v1 && v2 && v3 && v4
  }

  override def toString: String = {
    s"(${x+1},${y+1})(${x2+1},${y2+1})"
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
