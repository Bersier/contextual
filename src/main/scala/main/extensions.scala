package main

import Trit.*
import scala.annotation.targetName

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

extension (b: Boolean)
  inline infix def thenYield[T](inline t: => T): Option[T] = if b then Some(t) else None
  inline infix def thenFlatYield[T](inline o: => Option[T]): Option[T] = if b then o else None

extension [T](o: Option[T])
  inline def orElseIf(inline condition: Boolean)(inline alternative: => Option[T]): Option[T] =
    o orElse (condition thenFlatYield alternative)
  inline infix def orElse(inline alternative: => Option[T], inline provided: Boolean = true): Option[T] =
    o.orElseIf(provided)(alternative)
