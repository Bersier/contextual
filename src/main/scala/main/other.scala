package main

import scala.compiletime.codeOf

inline def exprString(inline expression: Any): String = s"${codeOf(expression)} = ${expression.toString}"

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
