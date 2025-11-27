package contextual

import scala.compiletime.ops.int.<
import scala.compiletime.ops.string.{CharAt, Length, Substring}

/**
 * Compares two strings according to the shortlex order at the type level.
 */
type IsSmallerThan[S1 <: String, S2 <: String] <: Boolean = Length[S1] < Length[S2] match
  case true => true
  case false => Length[S2] < Length[S1] match
    case true => false
    case false => Length[S1] match
      case 0 => false
      case _ => IntFromChar[CharAt[S1, 0]] < IntFromChar[CharAt[S2, 0]] match
        case true => true
        case false => IntFromChar[CharAt[S2, 0]] < IntFromChar[CharAt[S1, 0]] match
          case true => false
          case false => IsSmallerThan[Substring[S1, 1, Length[S1]], Substring[S2, 1, Length[S2]]]

/**
 * Converts a [[Char]] to an [[Int]] at the type level.
 *
 * Only handles the 95 printable ASCII characters.
 */
type IntFromChar[C <: Char] <: Int = C match
  case ' ' => 32
  case '!' => 33
  case '"' => 34
  case '#' => 35
  case '$' => 36
  case '%' => 37
  case '&' => 38
  case '\'' => 39
  case '(' => 40
  case ')' => 41
  case '*' => 42
  case '+' => 43
  case ',' => 44
  case '-' => 45
  case '.' => 46
  case '/' => 47
  case '0' => 48
  case '1' => 49
  case '2' => 50
  case '3' => 51
  case '4' => 52
  case '5' => 53
  case '6' => 54
  case '7' => 55
  case '8' => 56
  case '9' => 57
  case ':' => 58
  case ';' => 59
  case '<' => 60
  case '=' => 61
  case '>' => 62
  case '?' => 63
  case '@' => 64
  case 'A' => 65
  case 'B' => 66
  case 'C' => 67
  case 'D' => 68
  case 'E' => 69
  case 'F' => 70
  case 'G' => 71
  case 'H' => 72
  case 'I' => 73
  case 'J' => 74
  case 'K' => 75
  case 'L' => 76
  case 'M' => 77
  case 'N' => 78
  case 'O' => 79
  case 'P' => 80
  case 'Q' => 81
  case 'R' => 82
  case 'S' => 83
  case 'T' => 84
  case 'U' => 85
  case 'V' => 86
  case 'W' => 87
  case 'X' => 88
  case 'Y' => 89
  case 'Z' => 90
  case '[' => 91
  case '\\' => 92
  case ']' => 93
  case '^' => 94
  case '_' => 95
  case '`' => 96
  case 'a' => 97
  case 'b' => 98
  case 'c' => 99
  case 'd' => 100
  case 'e' => 101
  case 'f' => 102
  case 'g' => 103
  case 'h' => 104
  case 'i' => 105
  case 'j' => 106
  case 'k' => 107
  case 'l' => 108
  case 'm' => 109
  case 'n' => 110
  case 'o' => 111
  case 'p' => 112
  case 'q' => 113
  case 'r' => 114
  case 's' => 115
  case 't' => 116
  case 'u' => 117
  case 'v' => 118
  case 'w' => 119
  case 'x' => 120
  case 'y' => 121
  case 'z' => 122
  case '{' => 123
  case '|' => 124
  case '}' => 125
  case '~' => 126
