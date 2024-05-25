package main

import java.security.SecureRandom
import java.util.NoSuchElementException
import scala.annotation.targetName
import scala.collection.concurrent
import scala.compiletime.constValue
import scala.compiletime.ops.any.{!=, ToString}

/**
  * Typeful representation of a binding environment/context
  * <br><br>
  * Similarly to how [[TList]] is a much more typeful version of [[collection.LinearSeq]], [[Env]] is a much more
  * typeful version of [[Map]]`[`[[ID]]`, ?]`.
  *
  * [[TList]], by the way, is very similar to [[Tuple]], except that it additionally keeps track of an upper bound,
  * which allows it to extend [[collection.LinearSeq]].
  *
  * A normal list only keeps track of one type argument, while a tuple keeps track of the type of each of its
  * components. For example, compare:
  * {{{
  * List(None, Some(1)): List[Option[String]]
  * (None, Some(1)): (None, Some[String])
  * }}}
  *
  * Similarly, a dictionary only keeps track of one key and one value type argument, while a hypothetical typeful
  * dictionary would have a type that is itself a dictionary type. For example, compare:
  * {{{
  * Map('a' -> None, 'b' -> Some(1)): Map[Char, Option[String]]
  * {'a' -> None, 'b' -> Some(1)}: {'a' -> None, 'b' -> Some[String]}
  * }}}
  * In the example above, `{...}` is a made up notation for typeful dictionaries, analogous to `(...)` for typeful
  * lists (aka tuples).
  * <br><br>
  * [[Env]] represents typeful dictionaries whose keys are [[ID]]s (ideally singleton types) and whose values have a
  * known upper type bound.
  */
sealed trait Env[+V] extends Map[Env.IDTop, V]:
  import Env.{AreDisjoint, ContainsKey, Extended, ID, IDTop, KVList, Merged, ValueIn, Without}

  /**
    * @param key present in this [[Env]] whose corresponding value is to be retrieved
    * @return the value corresponding to the given key
    */
  def at[I <: String & Singleton](key: ID[I])(using ContainsKey[Shape, I] =:= true): ValueIn[Shape, I]

  /**
    * @param key known to be absent from this [[Env]]
    * @param value to associate with the given new key
    * @return a new [[Env]] that additionally contains the given key-value pair
    */
  @targetName("extended")
  def +|[W >: V, U <: W, I <: String & Singleton](key: ID[I], value: U)
  (using ContainsKey[Shape, I] =:= false): Env[W]{ type Shape = Extended[Env.this.Shape, I, U] }

  /**
    * @return a new [[Env]] that additionally contains the given key-value pair
    */
  @throws[Env.AmbiguousKeyError]("if the given key is already associated with a different value")
  @targetName("extendedCanThrow")
  def +![W >: V](key: IDTop, value: W)(using CanEqual[W, W]): Env[W]

  /**
    * @param key present in [[Env]], to be removed
    * @return a new [[Env]] without the given key
    */
  @targetName("without")
  def -|[I <: String & Singleton](key: ID[I])
  (using ContainsKey[Shape, I] =:= true): Env[V]{ type Shape = Without[Env.this.Shape, I] }

  /**
    * @param that another [[Env]], whose keys are disjoint from this one
    * @return the union of this and that [[Env]]
    */
  @targetName("merged")
  def +|+[W >: V, S2 <: KVList](that: Env[W]{ type Shape = S2 })
  (using AreDisjoint[Shape, S2] =:= true): Env[W]{ type Shape = Merged[Env.this.Shape, S2] }

  /**
    * @param that another [[Env]], compatible with this one
    * @return the union of this and that [[Env]]
    */
  @throws[Env.AmbiguousKeyError]("if the any key in the resulting Env is to be associated with two different values")
  @targetName("mergedCanThrow")
  def +!+[W >: V](that: Env[W])(using CanEqual[W, W]): Env[W]

  /**
    * Internal representation of this [[Env]]'s type
    *
    * It is sorted by (String) key, without duplicate keys.
    */
  protected type Shape <: KVList

  /**
    * Internal representation of this [[Env]]
    *
    * It is sorted by (String) key, without duplicate keys.
  */
  protected def representation: Shape
object Env:

  /**
    * List of key-value pairs, where keys are of type [[String]]
    */
  sealed trait KVList
  object KVList:

    /**
      * Empty [[KVList]]
      */
    case object Empty extends KVList

    /**
      * Empty [[KVList]] type
      */
    type Empty = Empty.type

    /**
      * Constructs a new [[KVList]] from the given head and tail.
      *
      * @param k the key of the head of the new list
      * @param v the value of the head of the new list
      * @param t the tail of the new list
      */
    final case class Cons[K <: String & Singleton, +V, +T <: KVList](k: K, v: V, t: T) extends KVList

    given CanEqual[KVList, Empty] = CanEqual.derived
    given CanEqual[Empty, KVList] = CanEqual.derived
    given [H1, H2, T1 <: KVList, T2 <: KVList](using
      CanEqual[H1, H2], CanEqual[T1, T2],
    ): CanEqual[Cons[?, H1, T1], Cons[?, H2, T2]] = CanEqual.derived
  end KVList
  import KVList.{Cons, Empty}

  /**
    * Represents identifiers.
    */
  opaque type ID[+I <: String] = I
  object ID:

    /**
      * @return a new ID
      */
    def newInstance: IDTop =
      def nextAvailable: IDTop =
        newFrom(randomNumberGenerator.nextLong().toString).getOrElse(nextAvailable)
      nextAvailable

    /**
      * @return a new ID from the given [[Long]], if unused
      */
    inline def newFrom(i: Long): Option[ID[ToString[i.type]]] =
      newFrom(constValue[ToString[i.type]])

    /**
      * @return a new ID from the given string, if unused
      */
    def newFrom(i: String): Option[ID[i.type]] =
      used.put(i, ()).isEmpty thenYield i

    /**
      * @return an ID from the given [[Long]]
      */
    inline def from(i: Long): ID[ToString[i.type]] =
      from(constValue[ToString[i.type]])

    /**
      * @return an ID from the given [[String]]
      */
    inline def from(i: String): ID[i.type] =
      used(i) = (); i

    private val randomNumberGenerator = SecureRandom.getInstanceStrong
    private val used = concurrent.TrieMap.empty[String, Unit]
    given CanEqual[IDTop, IDTop] = CanEqual.derived
  end ID

  /**
    * @return the empty [[Env]]
    */
  val empty: Env[Nothing]{ type Shape = Empty } = EnvImpl(Empty)

  /**
    * @return a new [[Env]] containing the given key-value pair
    */
  def from[I <: String & Singleton, V](kV: (ID[I], V)): Env[V]{ type Shape = Cons[I, V, Empty] } =
    EnvImpl(Cons(kV._1, kV._2, Empty))

  /**
    * @return a new [[Env]] containing the two given key-value pairs
    */
  def from[I1 <: String & Singleton, V1, I2 <: String & Singleton, V2](kV1: (ID[I1], V1), kV2: (ID[I2], V2))
  (using (I1 != I2) =:= true): Env[V1 | V2]{ type Shape = Extended[Cons[I1, V1, Empty], I2, V2] } =
    EnvImpl(extended(from(kV1).representation, kV2._1, kV2._2, duplicateKeyThrower))
      .asInstanceOf[Env[V1 | V2]{ type Shape = Extended[Cons[I1, V1, Empty], I2, V2] }]

  /**
    * @return a new [[Env]] containing the three given key-value pairs
    */
  def from[I1 <: String & Singleton, V1, I2 <: String & Singleton, V2, I3 <: String & Singleton, V3]
  (kV1: (ID[I1], V1), kV2: (ID[I2], V2), kV3: (ID[I3], V3))
  (using (I1 != I2) =:= true, (I1 != I3) =:= true, (I2 != I3) =:= true): Env[V1 | V2 | V3]{
    type Shape = Extended[Extended[Cons[I1, V1, Empty], I2, V2], I3, V3]
  } = EnvImpl(extended(from(kV1, kV2).representation, kV3._1, kV3._2, duplicateKeyThrower))
    .asInstanceOf[Env[V1 | V2 | V3]{ type Shape = Extended[Extended[Cons[I1, V1, Empty], I2, V2], I3, V3] }]

  /**
    * @return a new [[Env]] containing the four given key-value pairs
    */
  def from[
    I1 <: String & Singleton, V1,
    I2 <: String & Singleton, V2,
    I3 <: String & Singleton, V3,
    I4 <: String & Singleton, V4,
  ](kV1: (ID[I1], V1), kV2: (ID[I2], V2), kV3: (ID[I3], V3), kV4: (ID[I4], V4))
  (using
    (I1 != I2) =:= true, (I1 != I3) =:= true, (I1 != I4) =:= true,
    (I2 != I3) =:= true, (I2 != I4) =:= true, (I3 != I4) =:= true,
  ): Env[V1 | V2 | V3 | V4]{
    type Shape = Extended[Extended[Extended[Cons[I1, V1, Empty], I2, V2], I3, V3], I4, V4]
  } = EnvImpl(extended(from(kV1, kV2, kV3).representation, kV4._1, kV4._2, duplicateKeyThrower))
    .asInstanceOf[Env[V1 | V2 | V3 | V4]{
      type Shape = Extended[Extended[Extended[Cons[I1, V1, Empty], I2, V2], I3, V3], I4, V4] }
    ]

  /**
    * @return a new [[Env]] containing the five given key-value pairs
    */
  def from[
    I1 <: String & Singleton, V1,
    I2 <: String & Singleton, V2,
    I3 <: String & Singleton, V3,
    I4 <: String & Singleton, V4,
    I5 <: String & Singleton, V5,
  ](kV1: (ID[I1], V1), kV2: (ID[I2], V2), kV3: (ID[I3], V3), kV4: (ID[I4], V4), kV5: (ID[I5], V5))
  (using
    (I1 != I2) =:= true, (I1 != I3) =:= true, (I1 != I4) =:= true, (I1 != I5) =:= true,
    (I2 != I3) =:= true, (I2 != I4) =:= true, (I2 != I5) =:= true,
    (I3 != I4) =:= true, (I3 != I5) =:= true,
    (I4 != I5) =:= true,
  ): Env[V1 | V2 | V3 | V4 | V5]{
    type Shape = Extended[Extended[Extended[Extended[Cons[I1, V1, Empty], I2, V2], I3, V3], I4, V4], I5, V5]
  } = EnvImpl(extended(from(kV1, kV2, kV3, kV4).representation, kV5._1, kV5._2, duplicateKeyThrower))
    .asInstanceOf[Env[V1 | V2 | V3 | V4 | V5]{
      type Shape = Extended[Extended[Extended[Extended[Cons[I1, V1, Empty], I2, V2], I3, V3], I4, V4], I5, V5] }
    ]

  private final class EnvImpl[+V, S <: KVList](protected val representation: S) extends Env[V]:
    protected type Shape = S

    def removed(key: IDTop): Env[V] = EnvImpl(without(representation)(using key))

    def updated[W >: V](key: IDTop, value: W): Env[W] =
      def updated(list: KVList): Cons[?, ?, ?] = list match
        case Empty => Cons(key, value, Empty)
        case Cons(k, v, tail) =>
          if key < k then Cons(key, value, list) else
          if key > k then Cons(k, v, updated(tail))
          else Cons(key, value, tail)
      EnvImpl(updated(representation))

    inline def get(key: IDTop): Option[V] = valueIn(representation)(using key)

    def iterator: Iterator[(IDTop, V)] = new Iterator[(IDTop, V)]:
      var remaining: KVList = representation
      def hasNext: Boolean = remaining == Empty
      def next(): (IDTop, V) = remaining match
        case Empty => throw new NoSuchElementException
        case Cons(key, value, tail) =>
          remaining = tail
          (key, value.asInstanceOf[V])

    def at[I <: String & Singleton](key: ID[I])(using ContainsKey[S, I] =:= true): ValueIn[Shape, I] =
      valueIn(representation)(using key).get.asInstanceOf[ValueIn[S, I]]

    @targetName("extended")
    def +|[W >: V, U <: W, I <: String & Singleton](key: ID[I], value: U)
    (using ContainsKey[S, I] =:= false): Env[W]{ type Shape = Extended[S, I, U] } =
      EnvImpl(extended(representation, key, value, duplicateKeyThrower))
        .asInstanceOf[Env[W]{ type Shape = Extended[S, I, U] }]

    @targetName("extendedCanThrow")
    def +![W >: V](key: IDTop, value: W)(using CanEqual[W, W]): Env[W] =
      EnvImpl(extended(representation, key, value, _ == _))

    @targetName("without")
    def -|[I <: String & Singleton](key: ID[I])
    (using ContainsKey[S, I] =:= true): Env[V]{ type Shape = Without[S, I] } =
      EnvImpl(without(representation)(using key)).asInstanceOf[Env[V]{ type Shape = Without[S, I] }]

    @targetName("merged")
    def +|+[W >: V, S2 <: KVList](that: Env[W]{ type Shape = S2 })
    (using AreDisjoint[S, S2] =:= true): Env[W]{ type Shape = Merged[S, S2] } =
      EnvImpl(merged(representation, that.representation)(using duplicateKeyThrower))
        .asInstanceOf[Env[W]{ type Shape = Merged[S, S2] }]

    @targetName("mergedCanThrow")
    def +!+[W >: V](that: Env[W])(using CanEqual[W, W]): Env[W] =
      given CanEqual[Any, Any] = CanEqual.derived
      EnvImpl(merged(representation, that.representation)(using _ == _))
  end EnvImpl

  private val duplicateKeyThrower: (Any, Any) => Nothing = (_, _) => throw AssertionError("Duplicate key")

  /**
    * @param list a list of key-value pairs, sorted by key, without duplicate keys
    * @param key whose associated value is to be returned
    * @return the value associated to the given key in the given list, if present
    */
  private def valueIn[V](list: KVList)(implicit key: String): Option[V] = list match
    case Empty => None
    case Cons(k, v, tail) => key == k thenYield v.asInstanceOf[V] orElse (valueIn(tail), provided = key < k)

  /**
    * @param list a list of key-value pairs, sorted by key, without duplicate keys
    * @param eq equality function for values
    * @return a new duplicate-free sorted list, additionally containing the new given key-value pair
    */
  private def extended[V](list: KVList, key: IDTop, value: V, eq: (V, V) => Boolean): Cons[?, ?, ?] = list match
    case Empty => Cons(key, value, Empty)
    case Cons(k, v, tail) =>
      if key < k then Cons(key, value, list) else
      if key > k then Cons(k, v, extended(tail, key, value, eq))
      else if eq(value, v.asInstanceOf[V]) then Cons(k, v, tail)
      else throw AmbiguousKeyError(key)

  /**
    * @param list a list of key-value pairs, sorted by key, without duplicate keys
    * @return a new list, without the given key
    */
  private def without[V](list: KVList)(using key: IDTop): KVList = list match
    case Empty => list
    case Cons(k, v, tail) =>
      if key < k then list else
      if key > k then Cons(k, v, without(tail))
      else tail

  /**
    * The usual merge algorithm
    *
    * @param l1 a list of key-value pairs, sorted by key, without duplicate keys
    * @param l2 a list of key-value pairs, sorted by key, without duplicate keys
    * @param eq equality function for values
    * @return a new duplicate-free sorted list representing the union of the two given lists
    */
  private def merged[V](l1: KVList, l2: KVList)(using eq: (V, V) => Boolean): KVList = l1 match
    case Empty => l2
    case Cons(k1, v1, tail1) => l2 match
      case Empty => l1
      case Cons(k2, v2, tail2) =>
        if k1 < k2 then Cons(k1, v1, merged(tail1, l2)) else
        if k1 > k2 then Cons(k2, v2, merged(l1, tail2))
        else if eq(v1.asInstanceOf[V], v2.asInstanceOf[V]) then Cons(k1, v1, merged(tail1, tail2))
        else throw AmbiguousKeyError(k1)

  /**
    * Converts a [[KVList]] type to an [[Env]] type.
    */
  type EnvL[L <: KVList] = Env[?]{ type Shape = Sorted[L] }

  /**
    * Converts a [[Tuple]] type to an [[Env]] type.
    */
  type EnvT[T <: Tuple] = Env[?]{ type Shape = FromTuple[T] }

  /**
    * Converts a [[Tuple]] type to a sorted [[KVList]] type.
    */
  type FromTuple[T <: Tuple] <: KVList = T match
    case EmptyTuple => Empty
    case (k, v) *: tail => Extended[FromTuple[tail], k, v]

  /**
    * Type-level list sort
    */
  type Sorted[L <: KVList] <: KVList = L match
    case Empty => L
    case Cons[k, v, tail] => Extended[Sorted[tail], k, v]

  /**
    * Type-level version of [[valueIn]]
    */
  type ValueIn[L <: KVList, K <: String] = L match
    case Cons[k, v, tail] => IsSmallerThan[K, k] match
      case false => IsSmallerThan[k, K] match
        case true => ValueIn[tail, K]
        case false => v

  /**
    * Type-level version of [[extended]]
    */
  type Extended[L <: KVList, K <: String, V] <: Cons[?, ?, ?] = L match
    case Empty => Cons[K, V, Empty]
    case Cons[k, v, tail] => IsSmallerThan[K, k] match
      case true => Cons[K, V, L]
      case false => IsSmallerThan[k, K] match
        case true => Cons[k, v, Extended[tail, K, V]]

  /**
    * Type-level version of [[without]]
    */
  type Without[L <: KVList, K <: String] <: KVList = L match
    case Cons[k, v, tail] => IsSmallerThan[K, k] match
      case true => L
      case false => IsSmallerThan[k, K] match
        case true => Cons[k, v, Without[tail, K]]
        case false => tail & KVList

  /**
    * Type-level version of [[merged]]
    */
  type Merged[L1 <: KVList, L2 <: KVList] <: KVList = L1 match
    case Empty => L2
    case Cons[k1, v1, tail1] => L2 match
      case Empty => L1
      case Cons[k2, v2, tail2] => IsSmallerThan[k1, k2] match
        case true => Cons[k1, v1, Merged[tail1, L2]]
        case false => IsSmallerThan[k2, k1] match
          case true => Cons[k2, v2, Merged[L1, tail2]]

  /**
    * At the type level, checks whether the given list contains the given key.
    */
  type ContainsKey[L <: KVList, K <: String] <: Boolean = L match
    case Empty => false
    case Cons[k, ?, tail] => IsSmallerThan[K, k] match
      case true => false
      case false => IsSmallerThan[k, K] match
        case true => ContainsKey[tail, K]
        case false => true

  /**
    * At the type level, checks whether the two given lists have disjoint keys.
    */
  type AreDisjoint[L1 <: KVList, L2 <: KVList] <: Boolean = L1 match
    case Empty => true
    case Cons[k1, ?, tail1] => L2 match
      case Empty => true
      case Cons[k2, ?, tail2] => IsSmallerThan[k1, k2] match
        case true => AreDisjoint[tail1, L2]
        case false => IsSmallerThan[k2, k1] match
          case true => AreDisjoint[L1, tail2]
          case false => false

  /**
    * Indicates a situation where the same key is associated with two different values.
    * @param key the ambiguous key
    */
  class AmbiguousKeyError(key: IDTop) extends Error(key)

  /**
    * Supertype of all [[ID]]s
    */
  private type IDTop = ID[String]
end Env

private def envExample(): Unit =
  import Env.ID

  def foo1[T](`a2`: List[T], `a1`: String): String =
    `a2`.map(_.toString).mkString(`a1`)

  println(foo1(`a2` = List(1, 2), `a1` = ", "))

  def foo2[T](args: Env.EnvT[(("a2", List[T]), ("a1", String))]): String =
    val `a1`: String = args.at(ID.from("a1"))
    val `a2`: List[T] = args.at(ID.from("a2"))
    `a2`.map(_.toString).mkString(`a1`)

  val args = Env.from(ID.from("a2") -> List(1, 2), ID.from("a1") -> ", ")
  println(foo2[Int](args))
