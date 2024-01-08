// scala -classpath "../../*:dependencies/*.jar" desk.scala test.desk

import scala.util.parsing.combinator._
import java.io._
import scala.collection.mutable._

class DeskParser extends JavaTokenParsers {

  def start = expr ~ init ^^ {
    case ex ~ map => {
      println(ex.foldLeft(0)((acc: Int, k: Any) => {
        (k match {
          case n: Int => n
          case s: String => map(s)
        }) + acc
      }))
      map.toMap
    }
  }

  def expr = "print" ~> repsep(exprToken, "+")

  def init = "where" ~> repsep(initToken, ",") ^^ {
    list => {
      val map: Map[String, Int] = new HashMap()
      list.foreach({
        case k ~ v => map(k) = v
      })
      map
    }
  }

  def exprToken = varr | num

  def initToken = varr ~ ("=" ~> num)

  def varr: Parser[String] = "[a-z]".r
  def num: Parser[Int] = wholeNumber ^^ { d => d.toInt }
}

object Main {
  def main(args: Array[String]): Unit = {
    val p = new DeskParser

    args.foreach(file => {
      p.parseAll(p.start, new FileReader(file)) match {
        case p.Success(parsed, _) => println(parsed)
        case x => println(x)
      }
    })
  }
}
