import contextual.{KVList, Keys, Record}

import scala.compiletime.ops.any.ToString
import scala.compiletime.ops.int.-
import scala.compiletime.ops.string.+ as ++
import scala.compiletime.{codeOf, constValue}

@main
def envExample(): Unit =
  import Keys.Key
  import Record.*

  import scala.language.implicitConversions

  println(Record.empty)
  println(Record.empty plus (Key("one") -> None))
  println(Record.empty plus (Key("one") -> None) plus (Key("two") -> Some(1)))
  println(Record.empty plus (Key("one") -> None) minus Key("one"))

  def jointString1[T](elements: List[T], sep: String): String =
    elements.map(_.toString).mkString(sep)

  println(exprString(jointString1(sep = ", ", elements = List(1, 2))))

  def jointString2[T](args: Record.EnvT[(("elements", List[T]), ("sep", String))]): String =
    val sep = args.at("sep")
    val elements = args.at("elements")
    elements.map(_.toString).mkString(sep)

  val args = Record(Key["sep"] -> ", ", Key["elements"] -> List(1, 2))
  println(exprString(jointString2[Int](args)))

  type NIntArgs[I <: Int] <: KVList = I match
    case 0 => KVList.Empty
    case _ => KVList.Cons["a" ++ ToString[I] & Singleton, Int, NIntArgs[I - 1]]

  val values: IndexedSeq[Int] = ???

  // todo it seems that, due to https://github.com/scala/scala3/issues/20475,
  //  it is not possible to lift this function to return Record[Int]{type Shape = NIntArgs[I]}.
  //  A workaround might be to have KVList extend Record directly?
  inline def nIntArgs[I <: Int](i: I): NIntArgs[I] = inline i match
    case _: 0 => KVList.Empty
    case _ => KVList.Cons(
      k = constValue["a" ++ ToString[I] & Singleton],
      v = values(i),
      t = nIntArgs(constValue[I - 1]),
    )

  import contextual.KVList.Extended
  type NIntEnv[I <: Int] <: Record[Int] = I match
    case 0 => Record.empty.type
    case _ => Record[Int]{ type Shape = Extended[Record.Shape[NIntEnv[I - 1]], ("a" ++ ToString[I]) & Singleton, Int] }

//  inline def nIntEnv[I <: Int](i: I): NIntEnv[I] = i match
//    case _: 0 => Record.empty
//    case _ =>
//      val dijointnessProof = ???
//      inline val binding: (Key[("a" ++ ToString[I]) & Singleton], Int) =
//        Key[("a" ++ ToString[I]) & Singleton] -> values(i)
//      val r: Record[Int]{ type Shape = Extended[Record.Shape[NIntEnv[I - 1]], ("a" ++ ToString[I]) & Singleton, Int] } =
//        nIntEnv(constValue[I - 1]).plus(binding)(using dijointnessProof) // todo compile-error on this line
//      r

inline def exprString(inline expression: Any): String = s"${codeOf(expression)} = ${expression.toString}"
