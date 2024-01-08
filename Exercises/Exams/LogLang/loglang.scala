// scala -classpath "../../*:dependencies/*.jar" loglang.scala test.ll

import scala.util.parsing.combinator._
import java.io._

class LogLangEvaluator extends JavaTokenParsers {

  def tasks = rep("task" ~> name ~ ("{" ~> rep(routine) <~ "}")) ^^ {
    results => results.foreach({
      case (name ~ res) => {
        println(s"Tast ${name}")
        res.zipWithIndex.foreach({
          case (r, i) => println(s" [op${i+1}] ${r}")
        })
      }
    })
  }

  def name = ident

  def filename = stringLiteral ^^ { s => s.substring(1, s.length -1) }

  def routine = remove | rename | backup | merge

  def remove = "remove" ~> filename ^^ {
    f => new File(f).delete
  }

  def rename = "rename" ~> filename ~ filename ^^ {
    case from ~ to => new File(from).renameTo(new File(to))
  }

  def backup = "backup" ~> filename ~ filename ^^ {
    case old ~ neww => {
      val oldf = new File(old)
      val newf = new File(neww)
      if (!oldf.exists() || newf.exists()) false
      else {
        val reader = new FileReader(old)
        val writer = new FileWriter(neww)
        reader.transferTo(writer)
        reader.close
        writer.close
        true
      }
    }
  }

  def merge = "merge" ~> filename ~ filename ~ filename  ^^ {
    case old1 ~ old2 ~ neww => {
      val old1f = new File(old1)
      val old2f = new File(old2)
      val newf = new File(neww)
      if (!old1f.exists() || !old2f.exists() || newf.exists()) false
      else {
        val reader1 = new FileReader(old1)
        val reader2 = new FileReader(old2)
        val writer = new FileWriter(neww)
        reader1.transferTo(writer)
        reader2.transferTo(writer)
        reader1.close
        reader2.close
        writer.close
        true
      }
    }
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    val p = new LogLangEvaluator

    args.foreach(file =>
      p.parseAll(p.tasks, new FileReader(file)) match {
        case p.Success(parsed, _) =>
        case x => println(x)
      }
    )
  }
}
