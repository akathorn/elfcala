package elfcala

class Name(val name: String) {
  override def toString: String = name

  def copy: Name = Name.fresh(name)

  override def equals(other: Any): Boolean = other match {
    case n : Name if n.name == this.name =>
      true
    case _ =>
      false
  }
}

object Name {
  private val counters = scala.collection.mutable.HashMap[String,Int]()

  private def freshName(prefix: String): String = {
    val count = counters.getOrElse(prefix, 1)
    counters.put(prefix, count + 1)
    prefix + "_" + count
  }

  def fresh(prefix: String): Name =
    new Name(freshName(prefix))

  def apply(prefix: String) = {
    // Makes sure the counter is at least 1
    val count = counters.getOrElse(prefix, 1)
    counters.put(prefix, count)

    new Name(prefix)
  }

}
