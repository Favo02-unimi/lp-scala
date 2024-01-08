// scala -classpath "../../*:dependencies/*.jar" csv.scala books.csv puffo.csv big.csv

import scala.util.parsing.combinator._
import java.io._
import scala.math.max

class CSVParser extends JavaTokenParsers {
  override val whiteSpace = " \t".r

  def start = rep(line) ~ lastLine ^^ {
    case ((l1: List[List[String]]) ~ (l2: List[String])) => l1 :+ l2
  }

  def line = repsep(str, ",") <~ "\r?\n".r
  def lastLine = repsep(str, ",")

  def str = stringLiteral | "[^,\t\n\r]*".r
}

object Main {
  def main(args : Array[String]): Unit = {
    val p = new CSVParser()
    args.foreach(file => {
      p.parseAll(p.start, new FileReader(file)) match {
        case p.Success(parsed: List[List[String]], _) =>
          prettyPrint(parsed, getMaxWidthPerColumn(parsed))
        case x => println(x)
      }
    })
  }

  private def getMaxWidthPerColumn(table: List[List[String]]): Array[Int] = {
    def gmwpc(curIndex: Int, result: Array[Int]): Array[Int] = {
      if (curIndex == table.length) result
      else gmwpc(curIndex+1, result.zipWithIndex.map({
        case (value: Int, index: Int) =>
          max(table(curIndex)(index).length, result(index))
      }))
    }
    gmwpc(0, new Array[Int](table(0).length))
  }

  private def prettyPrint(table: List[List[String]], dim: Array[Int]): Unit = {
    def printRep(n: Int, s: String): Unit = print(s*n)
    def printLine(line: List[String]): Unit = {
      print("|")
      line.zipWithIndex.foreach({
        case (str, index) => {
          print(" ")
          print(str)
          printRep(dim(index) - str.length, " ")
          print(" |")
        }
      })
      println()
    }
    val lineLen = table(0).length*2 + table(0).length+1 + dim.sum

    printRep(lineLen, "-")
    println()
    printLine(table(0))
    printRep(lineLen, "-")
    print("\n")
    table.slice(1, table.length).foreach(l => printLine(l))
    printRep(lineLen, "-")
    print("\n")
  }
}
