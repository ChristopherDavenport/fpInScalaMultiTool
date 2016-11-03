package fpinscalamulti.Vanilla.datastructures
/**
  * By and Large Caches and Maps are Useful primarily for large amounts of consistent read access.
  * By wrapping this in the State Monad we can capture these effects and keep a purely functional structure with
  * flatmap. As We Start By Apply A LRUCache => Actions => Value
  *
  * This could be handled by delegating to underlying data structures but this gives us quite decent access with
  * full control of the ordering by implementing even just value classes
  *
  * We make it private so that it always need to be initialized empty and so the function f will be taking effect to
  * run the sorting as each value is added. A remove function should be added to have a decent cache.
  *
  */
class LRUCache[K, V, L: Ordering] private (maxSize: Int, cache: Map[K, (L, V)], ordered : Map[L, K], f: () => L ) {

  /**
    * Size of our Cache is The Size of the Actual Cache
    * @return The size of the cache.
    */
  def size = cache.size

  /**
    * This allows the comparison to operate on L which since we already have the map sorted will immediately return
    * the last value to remove. This is purposefully explicit and extra so that we have full control over the value
    * by setting the Ordering of what we are running as L. Which can be custom. We need an ordering for (L,K) for
    * the ordered.min operation on set.
    */
  protected implicit val cmp : Ordering[(L, K)] = new Ordering[(L, K)]{
    override def compare(x: (L, K), y: (L, K)) =  Ordering[L].compare(x._1, y._1)
  }

  /**
    * So the function get utilizes the dual Map system to create an effectively constant return time
    * As Maps give eC time for addition and removal and lookup.  So we work in eC Time for All get Operations with
    * overwriting the ordered value with its new ordered value on get.
    * @param k The Key
    * @return An Optional V and the new State of the LRUCache which was modified because of the transaction.
    */
  def get(k: K): (Option[V], LRUCache[K,V,L]) = {
    val newL = f()
    cache.get(k) match {
      case Some((l, v)) =>
        (Option(v), new LRUCache[K,V,L](maxSize, cache, ordered - l + (newL -> k),f))
      case None => (None, this)
    }
  }

  /**
    * This changes the dynamic of the add since we are putting the values in we are using the n value map min to set.
    * This can be improved by a sorted map but then you dont have direct control over the Sorted Implementation. As
    * Sorted is built in with several controls which create lots of boiler plate to overrride, but if performance
    * is a priority than we can run that SortedMap in and we will have a slightly more performant solution.
    * Mattern matching is just explicit if checking which is then optimized on compile, and names which get outlined
    * by a scala style are for annotation of the case class that each denotes.
    *
    * @param pair The Key Value Pair We Are Adding to The Cache
    * @return The new LRUCache State After We have set the pair.
    */
  def set(pair: (K,V)): LRUCache[K,V,L] = {
    val size = cache.size
    val l = f()
    size match {
      case max if size == maxSize =>
        val orderedMin = ordered.min
        new LRUCache[K,V,L](
          maxSize,
          cache - orderedMin._2 + (pair._1 -> (l, pair._2)),
          ordered - orderedMin._1 + (l -> pair._1),
          f
        )
      case valid if maxSize > 0 =>
        new LRUCache[K,V,L](
          maxSize,
          cache + (pair._1 -> (l, pair._2)),
          ordered + (l -> pair._1),
          f
        )
      case lessThanOrZeroMax if maxSize <= 0 =>
        this
    }
  }

  override def toString: String = s"LRUCache($maxSize, $cache, $ordered)"

}

object LRUCache {
  def apply[K, V, L: Ordering](maxSize: Int)(f: () => L) = new LRUCache[K, V, L](maxSize, Map.empty[K, (L,V)], Map.empty[L, K], f)
}
