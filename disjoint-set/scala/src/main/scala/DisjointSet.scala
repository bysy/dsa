/**
 * Created by Benjamin on 9/25/15.
 */
class DisjointSet private (
  private val parents : Array[Int]) {
  def size: Int = parents.length
  def numGroups: Int =
    parents.zipWithIndex.count { case (p, i) => p==i }
  def isRoot(i: Int): Boolean = i==parents(i)
  def rootPure(i: Int): Int = if (isRoot(i)) i else rootPure(parents(i))
  def root(i: Int): Int = if (isRoot(i)) i else {
    val ppi = parents(parents(i))
    parents(i) = ppi
    root(ppi)
  }
  def rootAndDepthPure(i: Int): (Int,Int) = {
    def run(j: Int, n: Int): (Int,Int) =
      if (isRoot(j)) (j,n) else run(parents(j), n+1)
    run(i, 0)
  }
  def rootAndDepth(i: Int): (Int,Int) = {
    def run(j: Int, n: Int): (Int,Int) = {
      if (isRoot(j)) (j,n)
      else {
        val pj = parents(j)
        parents(j) = parents(pj)
        run(pj, n+1)
      }
    }
    run(i, 0)
  }
  def union(a: Int, b: Int): DisjointSet = {
    val (rootA, depthA) = rootAndDepth(a)
    val (rootB, depthB) = rootAndDepth(b)
    if (depthA<depthB)
      parents(rootA) = rootB
    else
      parents(rootB) = rootA
    this
  }
  def unionSlow(a: Int, b: Int): DisjointSet = {
    parents(rootPure(a)) = rootPure(b)
    this
  }
  def areConnectedPure(a: Int, b: Int): Boolean = rootPure(a)==rootPure(b)
  def areConnected(a: Int, b: Int): Boolean = root(a)==root(b)
  def getParents = parents.clone()
}

object DisjointSet {
  def apply(size: Int) =
    new DisjointSet((0 to size-1).toArray)
}
