// scala -classpath "../*:dependencies/*.jar" ExpressionSolver.scala "((2 + 7) + ((3 + 9) + 4))" "((1 * 7) + (7 * ((3 + 9) + 5)))" "((5*(7-2))+(((15/3)+2)-((1422*2)-(3500/4))))"

import scala.util.parsing.combinator._

trait Solvable

case class Literal(n: Int) extends Solvable {
  override def toString() = s"${n}"
}

case class Expr(l: Solvable, s: String, r: Solvable) extends Solvable {
  override def toString() = s"(${l.toString()} ${s} ${r.toString()})"
}

class ArithmeticParser extends JavaTokenParsers {

  def sign: Parser[String] = "+" | "-" | "*" | "/"

  def num: Parser[Solvable] = wholeNumber ^^ { n => new Literal(n.toInt) }

  def expr: Parser[Solvable] = num | "(" ~> expr ~ sign ~ expr <~ ")" ^^ {
    case (l ~ s ~ r) => new Expr(l, s, r)
  }
}

object Solve {
  def semplifica(expr: Solvable): Solvable = expr match {
    case Literal(n) => expr
    case Expr(Literal(l), s, Literal(r)) => s match {
      case "+" => Literal(l + r)
      case "-" => Literal(l - r)
      case "*" => Literal(l * r)
      case "/" => Literal(l / r)
    }
    case Expr(l, s, r) => Expr(semplifica(l), s, semplifica(r))
  }

  def solve(expr: Solvable): Unit = {
    println(expr)
    expr match {
      case Literal(n) =>
      case Expr(l, s, r) => solve(semplifica(expr))
    }
  }
}

object Main {
  def main(args: Array[String]) = {
    val p = new ArithmeticParser

    args.foreach(arg => p.parseAll(p.expr, arg) match {
      case p.Success(parsed, _) => Solve.solve(parsed)
      case x => println(x)
    })
  }
}
