// scala -classpath "../*:dependencies/*.jar" primary_school_arith.scala primary_school_arith1.in primary_school_arith2.in primary_school_arith3.in

import scala.util.parsing.combinator._
import java.io.FileReader

class ArithParser extends JavaTokenParsers {

  def num = floatingPointNumber

  def firstline = num
  def line = ("+" | "-") ~ num
  def result = "=" ~> """[-]+""".r ~> num
  // "=" ~> rep("-") ~> num doesnt work with negative results (the - of the number is parsed by the rep)

  def operations = firstline ~ rep(line) ^^ {
    case (num: String) ~ (ops: List[String~String]) =>
      num.toInt + ops
        .map({ case sign ~ n => if (sign == "+") n.toInt else -1*n.toInt })
        .reduce((acc: Int, n: Int) => acc + n)
    case _ => 0
  }

  def arith = operations ~ result ^^ {
    case (operations: Int) ~ (result: String) => {
      (operations, result.toInt)
    }
  }
}

object ParseArith extends ArithParser {
  def main(args: Array[String]) =
    args.foreach(file => {
      println("---")
      parseAll(arith, new FileReader(file)) match {
        case Success((ops, res), _) =>
          println("Operations result:", ops)
          println("Result:", res)
          println("Valid:", ops == res)
        case x => print(x.toString)
      }
    })
}
