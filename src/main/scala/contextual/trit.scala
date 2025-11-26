package contextual

import contextual.Trit.{Negative, Neutral, Positive}

import scala.annotation.targetName

enum Trit:
  case Negative, Neutral, Positive
object Trit:
  def apply(i: Long): Trit = math.signum(i) match
    case -1L => Negative
    case  0L => Neutral
    case  1L => Positive
    case _ => throw AssertionError("Impossible case")

  given CanEqual[Trit, Trit] = CanEqual.derived
end Trit

extension (s: String)

  /**
   * Three-way <b>shortlex</b> comparison operator for strings
   *
   * @return [[Negative]] when s < t, [[Neutral]] when s == t, [[Positive]] when s > t
   */
  @targetName("shortlex")
  def <=>(t: String): Trit =
    math.signum(s.length - t.length) match
      case -1 => Negative
      case 1 => Positive
      case 0 =>
        if s < t then Negative else
        if s > t then Positive
        else Neutral
      case _ => throw AssertionError("Impossible case")
