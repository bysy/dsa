/**
 * Created by Benjamin on 9/25/15.
 */
class DisjointSet private (
  private val parents : Array[Int]) {
  def size: Int = parents.length
  def numGroups: Int =
    parents.zipWithIndex.count { case (p, i) => p==i }
  def isRoot(i: Int): Boolean = i==parents(i)
  def root(i: Int): Int = if (isRoot(i)) i else root(parents(i))
  def union(a: Int, b: Int): DisjointSet = {
    parents(root(a)) = root(b)
    this
  }
  def areConnected(a: Int, b: Int): Boolean = root(a)==root(b)
}

object DisjointSet {
  def apply(size: Int) =
    new DisjointSet((0 to size-1).toArray)
}
