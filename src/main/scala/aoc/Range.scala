package aoc

case class Range(start: Long, end: Long) {
  def contains(n: Long): Boolean = n >= start && n < end
  def nonEmpty: Boolean = start < end
  def isEmpty: Boolean = !nonEmpty

  def size: Long = end - start

  def intersection(other: Range): Option[Range] = {
    val start = Math.max(this.start, other.start)
    val end = Math.min(this.end, other.end)
    if (start <= end) Some(Range(start, end)) else None
  }

  def relativeComplement(other: Range): Seq[Range] = intersection(other) match {
    case Some(i) => Seq(Range(start, i.start), Range(i.end, end)).filter(_.nonEmpty)
    case None    => Seq(this)
  }
}
