import scala.util.parsing.combinator._
import java.io._

class Parser extends JavaTokenParsers {
  // cambiare token ignorati dal parser
  override val whiteSpace = " \t".r
  override val skipWhitespace = false

  // entrypoint
  def start = rep("ciao" ~ name ~ surname) ^^ {
    case n ~ s => prinln("nome: ${name} cognome: ${surname}")
  }

  // utilità della classe JavaTokenParsers:

  // identificatore di Java (stringa che inzia con un carattere e può contenere numeri e simboli)
  def normalString = ident

  // stringa tra virgolette (può contenere qualsiasi simbolo)
  def literalString = stringLiteral

  // numero interno (10)
  def int = wholeNumber

  // numero con virgola (10.5)
  def float = decimalNumber

  // numero in notazione scientifica (10e9)
  def scientificFloat = floatingPointNumber
}

object Main {
  def main(args: Array[String]): Unit = {
    val p = new Parser

    args.foreach(file => p.parseAll(p.start, new FileInput(file)) match {
      case p.Success(parsed, _) =>
      case x => println(x)
    }
  }
}
