package contextual

import scala.compiletime.constValue
import scala.compiletime.ops.any.ToString

object Keys:
  /**
    * Represents identifiers.
    */
  opaque type Key[+K <: String] = K
  object Key:

    /**
      * @return a [[Key]] from the given [[Long]]
      */
    inline def from(i: Long): Key[ToString[i.type]] =
      apply(constValue[ToString[i.type]])

    /**
      * @return a [[Key]] from the given [[String]]
      */
    inline def apply(i: String): Key[i.type] = i

    /**
      * @return a [[Key]] from the given [[String]] singleton type
      */
    inline def apply[K <: String & Singleton]: Key[K] = constValue[K]

    given [S <: String & Singleton] => Conversion[S, Key[S]] = s => apply(s)
    given CanEqual[KeyTop, KeyTop] = CanEqual.derived
  end Key

  /**
    * Supertype of all [[Key]]s
    */
  private[contextual] type KeyTop = Key[String]
end Keys
