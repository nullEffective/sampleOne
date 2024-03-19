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
