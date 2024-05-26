package main

import java.util.NoSuchElementException
import scala.compiletime.constValue
import scala.compiletime.ops.any.{!=, ToString}
import scala.compiletime.ops.int.-
import scala.compiletime.ops.string.+ as ++

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
  import Env.{AreDisjoint, ContainsKey, Extended, ID, KVList, Merged, ValueIn, Without}

  /**
    * @param key present in this [[Env]] whose corresponding value is to be retrieved
    * @return the value bound to the given key
    */
  def at[I <: String & Singleton](key: ID[I])(using ContainsKey[Shape, I] =:= true): ValueIn[Shape, I]

  /**
    * @param kV a key-value binding, whose key is absent from this [[Env]]
    * @return a new [[Env]] that additionally contains the given binding
    */
  infix def plus[W >: V, U <: W, I <: String & Singleton](kV: (ID[I], U))
  (using ContainsKey[Shape, I] =:= false): Env[W]{ type Shape = Extended[Env.this.Shape, I, U] }

  /**
    * @param key present in [[Env]], to be removed
    * @return a new [[Env]] without the given key
    */
  infix def minus[I <: String & Singleton](key: ID[I])
  (using ContainsKey[Shape, I] =:= true): Env[V]{ type Shape = Without[Env.this.Shape, I] }

  /**
    * @param that another [[Env]], whose keys are disjoint from this one
    * @return the union of this and that [[Env]]
    */
  infix def mergedWith[W >: V, S2 <: KVList](that: Env[W]{ type Shape = S2 })
  (using AreDisjoint[Shape, S2] =:= true): Env[W]{ type Shape = Merged[Env.this.Shape, S2] }

  override def toString: String = s"{${representation.elementString}}"

  override def equals(o: Any): Boolean =
    import compiletime.asMatchable
    o.asMatchable match
      case that: Env[?] =>
        given CanEqual[KVList, KVList] = CanEqual.derived
        this.representation == that.representation
      case _ => false

  override def hashCode(): Int = representation.hashCode()

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
  given [V](using CanEqual[V, V]) : CanEqual[Env[V], Env[V]] = CanEqual.derived

  /**
    * List of key-value pairs, where keys are of type [[String]]
    */
  sealed trait KVList:
    override def toString: String = s"KVList($elementString)"
    def elementString: String
  object KVList:

    /**
      * Empty [[KVList]]
      */
    case object Empty extends KVList:
      def elementString: String = ""
    end Empty

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
    final case class Cons[K <: String & Singleton, +V, +T <: KVList](k: K, v: V, t: T) extends KVList:
      def elementString: String = joined(s"$k -> $v", t.elementString, ", ")
    end Cons

    private inline def joined(h: String, t: String, inline s: String): String =
      if t.nonEmpty then s"$h$s$t" else h

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
      * @return an ID from the given [[Long]]
      */
    inline def from(i: Long): ID[ToString[i.type]] =
      apply(constValue[ToString[i.type]])

    /**
      * @return an ID from the given [[String]]
      */
    inline def apply(i: String): ID[i.type] = i

    /**
      * @return an ID from the given [[String]] singleton type
      */
    inline def apply[I <: String & Singleton]: ID[I] = constValue[I]

    given [S <: String & Singleton]: Conversion[S, ID[S]] = s => apply(s)
    given CanEqual[IDTop, IDTop] = CanEqual.derived
  end ID

  /**
    * @return the empty [[Env]]
    */
  val empty: Env[Nothing]{ type Shape = Empty } = EnvImpl(Empty)

  /**
    * @return a new [[Env]] containing the given key-value pair
    */
  def apply[I <: String & Singleton, V](kV: (ID[I], V)): Env[V]{ type Shape = Cons[I, V, Empty] } =
    EnvImpl(Cons(kV._1, kV._2, Empty))

  /**
    * @return a new [[Env]] containing the two given key-value pairs
    */
  def apply[I1 <: String & Singleton, V1, I2 <: String & Singleton, V2](kV1: (ID[I1], V1), kV2: (ID[I2], V2))
  (using (I1 != I2) =:= true): Env[V1 | V2]{ type Shape = Extended[Cons[I1, V1, Empty], I2, V2] } =
    EnvImpl(extended(apply(kV1).representation, kV2._1, kV2._2, duplicateKeyThrower))
      .asInstanceOf[Env[V1 | V2]{ type Shape = Extended[Cons[I1, V1, Empty], I2, V2] }]

  /**
    * @return a new [[Env]] containing the three given key-value pairs
    */
  def apply[I1 <: String & Singleton, V1, I2 <: String & Singleton, V2, I3 <: String & Singleton, V3]
  (kV1: (ID[I1], V1), kV2: (ID[I2], V2), kV3: (ID[I3], V3))
  (using (I1 != I2) =:= true, (I1 != I3) =:= true, (I2 != I3) =:= true): Env[V1 | V2 | V3]{
    type Shape = Extended[Extended[Cons[I1, V1, Empty], I2, V2], I3, V3]
  } = EnvImpl(extended(apply(kV1, kV2).representation, kV3._1, kV3._2, duplicateKeyThrower))
    .asInstanceOf[Env[V1 | V2 | V3]{ type Shape = Extended[Extended[Cons[I1, V1, Empty], I2, V2], I3, V3] }]

  /**
    * @return a new [[Env]] containing the four given key-value pairs
    */
  def apply[
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
  } = EnvImpl(extended(apply(kV1, kV2, kV3).representation, kV4._1, kV4._2, duplicateKeyThrower))
    .asInstanceOf[Env[V1 | V2 | V3 | V4]{
      type Shape = Extended[Extended[Extended[Cons[I1, V1, Empty], I2, V2], I3, V3], I4, V4] }
    ]

  /**
    * @return a new [[Env]] containing the five given key-value pairs
    */
  def apply[
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
  } = EnvImpl(extended(apply(kV1, kV2, kV3, kV4).representation, kV5._1, kV5._2, duplicateKeyThrower))
    .asInstanceOf[Env[V1 | V2 | V3 | V4 | V5]{
      type Shape = Extended[Extended[Extended[Extended[Cons[I1, V1, Empty], I2, V2], I3, V3], I4, V4], I5, V5] }
    ]

  import Trit.*

  private final class EnvImpl[+V, S <: KVList](protected val representation: S) extends Env[V]:
    protected type Shape = S

    def removed(key: IDTop): Env[V] = EnvImpl(without(representation)(using key))

    def updated[W >: V](key: IDTop, value: W): Env[W] =
      def updated(list: KVList): Cons[?, ?, ?] = list match
        case Empty => Cons(key, value, Empty)
        case Cons(k, v, tail) => key <=> k match
          case Negative => Cons(key, value, list)
          case Neutral  => Cons(key, value, tail)
          case Positive => Cons(k, v, updated(tail))
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

    infix def plus[W >: V, U <: W, I <: String & Singleton](kV: (ID[I], U))
    (using ContainsKey[S, I] =:= false): Env[W]{ type Shape = Extended[S, I, U] } =
      EnvImpl(extended(representation, kV._1, kV._2, duplicateKeyThrower))
        .asInstanceOf[Env[W]{ type Shape = Extended[S, I, U] }]

    infix def minus[I <: String & Singleton](key: ID[I])
    (using ContainsKey[S, I] =:= true): Env[V]{ type Shape = Without[S, I] } =
      EnvImpl(without(representation)(using key)).asInstanceOf[Env[V]{ type Shape = Without[S, I] }]

    infix def mergedWith[W >: V, S2 <: KVList](that: Env[W]{ type Shape = S2 })
    (using AreDisjoint[S, S2] =:= true): Env[W]{ type Shape = Merged[S, S2] } =
      EnvImpl(merged(representation, that.representation)(using duplicateKeyThrower))
        .asInstanceOf[Env[W]{ type Shape = Merged[S, S2] }]
  end EnvImpl

  private val duplicateKeyThrower: (Any, Any) => Nothing = (_, _) => throw AssertionError("Duplicate key")

  /**
    * @param list a list of key-value pairs, sorted by key, without duplicate keys
    * @param key whose associated value is to be returned
    * @return the value associated to the given key in the given list, if present
    */
  private def valueIn[V](list: KVList)(implicit key: String): Option[V] = list match
    case Empty => None
    case Cons(k, v, tail) => (key == k)
      .thenYield (v.asInstanceOf[V])
      .orElse (valueIn(tail), provided = key <=> k == Positive)

  /**
    * @param list a list of key-value pairs, sorted by key, without duplicate keys
    * @param eq equality function for values
    * @return a new duplicate-free sorted list, additionally containing the new given key-value pair
    */
  private def extended[V](list: KVList, key: IDTop, value: V, eq: (V, V) => Boolean): Cons[?, ?, ?] = list match
    case Empty => Cons(key, value, Empty)
    case Cons(k, v, tail) => key <=> k match
      case Negative => Cons(key, value, list)
      case Positive => Cons(k, v, extended(tail, key, value, eq))
      case Neutral  => if eq(value, v.asInstanceOf[V])
        then Cons(k, v, tail)
        else throw AmbiguousKeyError(key)

  /**
    * @param list a list of key-value pairs, sorted by key, without duplicate keys
    * @return a new list, without the given key
    */
  private def without[V](list: KVList)(using key: IDTop): KVList = list match
    case Empty => list
    case Cons(k, v, tail) => key <=> k match
      case Negative => list
      case Neutral  => tail
      case Positive => Cons(k, v, without(tail))

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
      case Cons(k2, v2, tail2) => k1 <=> k2 match
        case Negative => Cons(k1, v1, merged(tail1, l2))
        case Positive => Cons(k2, v2, merged(l1, tail2))
        case Neutral  => if eq(v1.asInstanceOf[V], v2.asInstanceOf[V])
          then Cons(k1, v1, merged(tail1, tail2))
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

@main
private def envExample(): Unit =
  import Env.ID
  import ID.given
  import scala.language.implicitConversions

  println(Env.empty)
  println(Env.empty plus (ID("one") -> None))
  println(Env.empty plus (ID("one") -> None) plus (ID("two") -> Some(1)))
  println(Env.empty plus (ID("one") -> None) minus ID("one"))

  def jointString1[T](elements: List[T], sep: String): String =
    elements.map(_.toString).mkString(sep)

  println(exprString(jointString1(sep = ", ", elements = List(1, 2))))

  def jointString2[T](args: Env.EnvT[(("elements", List[T]), ("sep", String))]): String =
    val sep = args.at("sep")
    val elements = args.at("elements")
    elements.map(_.toString).mkString(sep)

  val args = Env(ID["sep"] -> ", ", ID["elements"] -> List(1, 2))
  println(exprString(jointString2[Int](args)))

  import Env.KVList
  type NIntArgs[I <: Int] <: Env.KVList = I match
    case 0 => KVList.Empty
    case _ => KVList.Cons["a" ++ ToString[I] & Singleton, Int, NIntArgs[I - 1]]

  inline def nIntArgs[I <: Int](i: I)(using values: IndexedSeq[Int]): Env[Int]{type Shape = NIntArgs[I]} =
    i match
      case _: 0 => Env.empty.asInstanceOf[Env[Int]{type Shape = NIntArgs[I]}]
      case i =>
        val proof = ???
        nIntArgs(constValue[(I - 1) & Singleton]).plus(ID["a" ++ ToString[I] & Singleton] -> values(i))(using proof)
        .asInstanceOf[Env[Int]{ type Shape = NIntArgs[I] }]
