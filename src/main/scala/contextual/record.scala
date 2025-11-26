package contextual

import contextual.Keys.KeyTop

import scala.annotation.publicInBinary
import scala.compiletime.ops.any.!=

/**
 * @param representation internal representation of this [[Record]]
 * @tparam V upper bound on the types of the values
 */
final class Record[+V] @publicInBinary private[contextual](
  @publicInBinary protected[contextual] val representation: Map[KeyTop, V],
) extends Map[KeyTop, V]:

  /**
    * Internal representation of this [[Record]]'s type
    *
    * It is sorted by (String) key, without duplicate keys.
    */
  protected[contextual] type Shape <: KVList

  inline def removed(key: KeyTop): Record[V] = new Record(representation.removed(key))

  def updated[W >: V](key: KeyTop, value: W): Record[W] = new Record(representation.updated(key, value))

  inline def get(key: KeyTop): Option[V] = representation.get(key)

  def iterator: Iterator[(KeyTop, V)] = representation.iterator

  override def toString: String = s"Record{${representation.mkString(", ")}}"

  override def equals(o: Any): Boolean =
    o.asInstanceOf[Matchable] match
      case that: Record[?] =>
        given CanEqual[Map[KeyTop, V], Map[KeyTop, ?]] = CanEqual.derived
        this.representation == that.representation
      case _ => false

  override def hashCode(): Int = representation.hashCode()
end Record

object Record:
  import KVList.*
  import Keys.*

  given [V] => CanEqual[V, V] => CanEqual[Record[V], Record[V]] = CanEqual.derived

  /**
    * @return the empty [[Record]]
    */
  lazy val empty: Record[Nothing]{ type Shape = Empty } = new Record(Map.empty[KeyTop, Nothing])
    .asInstanceOf[Record[Nothing]{ type Shape = Empty }]

  /**
    * @return a new [[Record]] containing the given key-value pair
    */
  def apply[K <: String & Singleton, V](kV: (Key[K], V)): Record[V]{ type Shape = Cons[K, V, Empty] } =
    new Record(Map(kV)).asInstanceOf[Record[V]{ type Shape = Cons[K, V, Empty] }]

  /**
    * @return a new [[Record]] containing the two given key-value pairs
    */
  def apply[I1 <: String & Singleton, V1, I2 <: String & Singleton, V2](kV1: (Key[I1], V1), kV2: (Key[I2], V2))
  (using (I1 != I2) =:= true): Record[V1 | V2]{ type Shape = Extended[Cons[I1, V1, Empty], I2, V2] } =
    new Record(Map(kV1, kV2)).asInstanceOf[Record[V1 | V2]{ type Shape = Extended[Cons[I1, V1, Empty], I2, V2] }]

  /**
    * @return a new [[Record]] containing the three given key-value pairs
    */
  def apply[I1 <: String & Singleton, V1, I2 <: String & Singleton, V2, I3 <: String & Singleton, V3]
  (kV1: (Key[I1], V1), kV2: (Key[I2], V2), kV3: (Key[I3], V3))
  (using (I1 != I2) =:= true, (I1 != I3) =:= true, (I2 != I3) =:= true): Record[V1 | V2 | V3]{
    type Shape = Extended[Extended[Cons[I1, V1, Empty], I2, V2], I3, V3]
  } = new Record(Map(kV1, kV2, kV3)).asInstanceOf[Record[V1 | V2 | V3]{
        type Shape = Extended[Extended[Cons[I1, V1, Empty], I2, V2], I3, V3]
      }]

  /**
    * @return a new [[Record]] containing the four given key-value pairs
    */
  def apply[
    I1 <: String & Singleton, V1,
    I2 <: String & Singleton, V2,
    I3 <: String & Singleton, V3,
    I4 <: String & Singleton, V4,
  ](kV1: (Key[I1], V1), kV2: (Key[I2], V2), kV3: (Key[I3], V3), kV4: (Key[I4], V4))
  (using
    (I1 != I2) =:= true, (I1 != I3) =:= true, (I1 != I4) =:= true,
    (I2 != I3) =:= true, (I2 != I4) =:= true, (I3 != I4) =:= true,
  ): Record[V1 | V2 | V3 | V4]{
    type Shape = Extended[Extended[Extended[Cons[I1, V1, Empty], I2, V2], I3, V3], I4, V4]
  } = new Record(Map(kV1, kV2, kV3, kV4)).asInstanceOf[Record[V1 | V2 | V3 | V4]{
        type Shape = Extended[Extended[Extended[Cons[I1, V1, Empty], I2, V2], I3, V3], I4, V4]
      }]

  /**
    * @return a new [[Record]] containing the five given key-value pairs
    */
  def apply[
    I1 <: String & Singleton, V1,
    I2 <: String & Singleton, V2,
    I3 <: String & Singleton, V3,
    I4 <: String & Singleton, V4,
    I5 <: String & Singleton, V5,
  ](kV1: (Key[I1], V1), kV2: (Key[I2], V2), kV3: (Key[I3], V3), kV4: (Key[I4], V4), kV5: (Key[I5], V5))
  (using
    (I1 != I2) =:= true, (I1 != I3) =:= true, (I1 != I4) =:= true, (I1 != I5) =:= true,
    (I2 != I3) =:= true, (I2 != I4) =:= true, (I2 != I5) =:= true,
    (I3 != I4) =:= true, (I3 != I5) =:= true,
    (I4 != I5) =:= true,
  ): Record[V1 | V2 | V3 | V4 | V5]{
    type Shape = Extended[Extended[Extended[Extended[Cons[I1, V1, Empty], I2, V2], I3, V3], I4, V4], I5, V5]
  } = new Record(Map(kV1, kV2, kV3, kV4, kV5)).asInstanceOf[Record[V1 | V2 | V3 | V4 | V5]{
        type Shape = Extended[Extended[Extended[Extended[Cons[I1, V1, Empty], I2, V2], I3, V3], I4, V4], I5, V5]
      }]

  /**
   * @return the union of [[r1]] and [[r2]]
   */
  def merged[V, S1 <: KVList, S2 <: KVList](r1: Record[V]{ type Shape = S1 }, r2: Record[V]{ type Shape = S2 })
  (using AreDisjoint[S1, S2] =:= true): Record[V]{ type Shape = Merged[S1, S2] } =
    (r1 ++ r2).asInstanceOf[Record[V]{ type Shape = Merged[S1, S2] }]

  extension [V, S <: KVList](r: Record[V]{ type Shape = S })

    /**
      * @param key present in this record whose corresponding value is to be retrieved
      * @return the value bound to the given key
      */
    def at[K <: String & Singleton](key: Key[K])(using ContainsKey[S, K] =:= true): ValueIn[S, K] =
      r(key).asInstanceOf[ValueIn[S, K]]

    /**
      * @param kV a key-value binding, whose key is absent from this record
      * @return a new record that additionally contains the given binding
      */
    infix def plus[U, K <: String & Singleton](kV: (Key[K], U))
    (using ContainsKey[S, K] =:= false): Record[U | V]{ type Shape = Extended[S, K, U] } =
      (r + kV).asInstanceOf[Record[U | V]{ type Shape = Extended[S, K, U] }]

    /**
      * @param key present in record, to be removed
      * @return a new record without the given key
      */
    infix def minus[K <: String & Singleton](key: Key[K])
    (using ContainsKey[S, K] =:= true): Record[V]{ type Shape = Without[S, K] } =
      (r - key).asInstanceOf[Record[V]{ type Shape = Without[S, K] }]

  /**
    * Converts a [[KVList]] type to a [[Record]] type.
    */
  type EnvL[L <: KVList] = Record[?]{ type Shape = Sorted[L] }

  /**
    * Converts a [[Tuple]] type to a [[Record]] type.
    */
  type EnvT[T <: Tuple] = Record[?]{ type Shape = FromTuple[T] }

  type Shape[E <: Record[?]] <: KVList = E match
    case GEnv[s] => s

  private type GEnv[S <: KVList] = Record[?]{ type Shape = S }
end Record
