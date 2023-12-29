// scala -classpath "*.jar:dependencies/*.jar" JSON.scala

import scala.util.parsing.combinator._
import java.io.FileReader

class ArithParser extends JavaTokenParsers {

  def num = floatingPointNumber

  def firstline = num
  def line = ("+" | "-") ~ num
  def result = "=" ~> rep("-") ~> num

  def operations = firstline ~ rep(line) ^^ {
    case (num: String) ~ (ops: List[String~String]) =>
      num.toInt + ops
        .map({ case sign ~ n => if (sign == "+") n.toInt else -1*n.toInt })
        .reduce((acc: Int, n: Int) => acc + n)
    case _ => 0
  }

  def arith = operations ~ result ^^ {
    case (operations: Int) ~ (result: String) => {
      println("Operations result:", operations)
      println("Result:", result.toInt)
      println("Valid:", operations == result.toInt)
      operations == result.toInt
    }
  }
}

object ParseArith extends ArithParser {
  def main(args: Array[String]) =
    println(parseAll(arith, new FileReader(args(0))))
}
