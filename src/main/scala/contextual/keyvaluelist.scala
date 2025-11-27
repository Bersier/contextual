package contextual

/**
  * Typeful list of key-value pairs, where keys are of type [[String]]
  */
sealed trait KVList:
  override def toString: String = s"KVList(${elements.mkString(", ")})"
  def elements: List[(String & Singleton, ?)]
object KVList:

  /**
    * Empty [[KVList]] type
    */
  type Empty = Empty.type

  /**
    * Empty [[KVList]]
    */
  case object Empty extends KVList:
    def elements: Nil.type = Nil
  end Empty

  /**
    * Constructs a new [[KVList]] from the given head and tail.
    *
    * @param k the key of the head of the new list
    * @param v the value of the head of the new list
    * @param t the tail of the new list
    */
  final case class Cons[K <: String & Singleton, +V, +T <: KVList](k: K, v: V, t: T) extends KVList:
    def elements: List[(String & Singleton, ?)] = (k -> v) :: t.elements
  end Cons

  given CanEqual[KVList, Empty] = CanEqual.derived
  given CanEqual[Empty, KVList] = CanEqual.derived
  given [H1, H2, T1 <: KVList, T2 <: KVList] =>
  CanEqual[H1, H2] => CanEqual[T1, T2] => CanEqual[Cons[?, H1, T1], Cons[?, H2, T2]] = CanEqual.derived

  /**
    * Converts a [[Tuple]] type to a sorted [[KVList]] type.
    */
  type FromTuple[T <: Tuple] <: KVList = T match
    case EmptyTuple => Empty
    case (k, v) *: tail => Extended[FromTuple[tail], k, v]

  /**
    * Sorts the given list.
    */
  type Sorted[L <: KVList] <: KVList = L match
    case Empty => L
    case Cons[k, v, tail] => Extended[Sorted[tail], k, v]

  /**
    * Returns the value associated with the given key in the given sorted list.
    */
  type ValueIn[L <: KVList, K <: String] = L match
    case Cons[k, v, tail] => IsSmallerThan[K, k] match
      case false => IsSmallerThan[k, K] match
        case true => ValueIn[tail, K]
        case false => v

  /**
    * Extends the given sorted list with the new given key, and its associated value.
    */
  type Extended[L <: KVList, K <: String, V] <: Cons[?, ?, ?] = L match
    case Empty => Cons[K, V, Empty]
    case Cons[k, v, tail] => IsSmallerThan[K, k] match
      case true => Cons[K, V, L]
      case false => IsSmallerThan[k, K] match
        case true => Cons[k, v, Extended[tail, K, V]]

  /**
    * If present, removes the given key from the given sorted list.
    */
  type Without[L <: KVList, K <: String] <: KVList = L match
    case Cons[k, v, tail] => IsSmallerThan[K, k] match
      case true => L
      case false => IsSmallerThan[k, K] match
        case true => Cons[k, v, Without[tail, K]]
        case false => tail

  /**
    * Merges the two given disjoint sorted lists.
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
    * Checks whether the given sorted list contains the given key.
    */
  type ContainsKey[L <: KVList, K <: String] <: Boolean = L match
    case Empty => false
    case Cons[k, ?, tail] => IsSmallerThan[K, k] match
      case true => false
      case false => IsSmallerThan[k, K] match
        case true => ContainsKey[tail, K]
        case false => true

  /**
    * Checks whether the two given sorted lists have disjoint keys.
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
end KVList
