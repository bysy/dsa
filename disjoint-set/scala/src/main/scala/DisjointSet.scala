/**
 * Created by Benjamin on 9/25/15.
 */
class DisjointSet private (
  private val parents : Array[Int]) {
  def size: Int = parents.length
  def numGroups: Int =
    parents.zipWithIndex.count { case (p, i) => p==i }
  def union(a: Int, b: Int) = this
  def areConnected(a: Int, b: Int) = false
}

object DisjointSet {
  def apply(size: Int) =
    new DisjointSet((0 to size-1).toArray)
}
