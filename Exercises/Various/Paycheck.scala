// scala -classpath "../*:dependencies/*.jar" Paycheck.scala paycheck1.in paycheck2.in paycheck3.in

import scala.language.implicitConversions
import scala.util.parsing.combinator._
import java.io.FileReader
import java.math.{BigDecimal, MathContext, RoundingMode}

class Money(val amount: BigDecimal) {
  def + (m: Money) = Money(amount.add(m.amount))
  def - (m: Money) = Money(amount.subtract(m.amount))
  def * (m: Money) = Money(amount.multiply(m.amount))
  def / (m: Money) = Money(amount.divide(m.amount, Money.scale, Money.roundingMode))
  def < (m: Money) = amount.compareTo(m.amount) < 0
  def <= (m: Money) = amount.compareTo(m.amount) <= 0
  def > (m: Money) = amount.compareTo(m.amount) > 0
  def >= (m: Money) = amount.compareTo(m.amount) >= 0
  override def hashCode = amount.hashCode * 31
  override def toString = String.format("$%.2f", double2Double(amount.doubleValue))
  override def equals (o: Any) = o match {
    case m: Money => amount equals m.amount
    case _ => false
  }
}

object Money {
  def apply(amount: BigDecimal) = new Money(amount)
  def apply(amount: Double) = new Money(scaled(new BigDecimal(amount)))
  def apply(amount: Long) = new Money(scaled(new BigDecimal(amount)))
  def apply(amount: Int) = new Money(scaled(new BigDecimal(amount)))
  def unapply(m: Money) = Some(m.amount)
  protected def scaled(d: BigDecimal) = d.setScale(scale, roundingMode)
  val scale = 4;
  val roundingMode = RoundingMode.HALF_UP
  val context = new MathContext(scale, roundingMode)
}

object Type2Money {
  implicit def bigDecimal2Money(b: BigDecimal) : Money = Money(b)
  implicit def double2Money(d: Double) : Money = Money(d)
  implicit def long2Money(l: Long) : Money = Money(l)
  implicit def int2Money(i: Int) : Money = Money(i)
}

case class Name(first: String, last: String)

case class Employee(name: Name, annualGrossSalary: Money)

case class Paycheck(gross: Money, net: Money, deductions: Money) {
  def plusGross (m: Money) = Paycheck(gross + m, net + m, deductions)
  def plusDeductions (m: Money) = Paycheck(gross, net - m, deductions + m)
}

class UnknownEmployee(name: Name) extends RuntimeException(name.toString)

class PayrollParserCombinators(val employees: Map[Name,Employee]) extends JavaTokenParsers {
  var currentEmployee: Employee = null
  var grossAmount: Money = Money(0)

  /** @return Parser[(Employee, Paycheck)] */
  def paycheck = empl ~ gross ~ deduct ^^ {case em ~ gr ~ de => (em, Paycheck(gr, gr-de, de))}

  /** @return Parser[Employee] */
  def empl = "paycheck" ~> "for" ~> "employee" ~> employeeName ^^ { name =>
    val names = name.substring(1, name.length-1).split(" ")
    val n = Name(names(0), names(1));
    if (!employees.contains(n))
      throw new UnknownEmployee(n)
    currentEmployee = employees(n);
    currentEmployee
  }

  /** @return Parser[Money] */
  def gross = "is" ~> "salary" ~> "for" ~> duration ^^ { dur =>
    grossAmount = salaryForDays(dur);
    grossAmount
  }

  def deduct = "minus" ~> "deductions" ~> "for" ~> "{" ~> deductItems <~ "}"

  /** "stringLiteral" provided by JavaTokenParsers
    * @return Parser[String] */
  def employeeName = stringLiteral

  /** "decimalNumber" provided by JavaTokenParsers
    * @return Parser[Int] */
  def duration = decimalNumber ~ weeksDays ^^ {
    case n ~ factor => n.toInt * factor
  }

  def weeksDays = weeks | days

  def weeks = "weeks?".r ^^ { _ => 5 }

  def days = "days?".r ^^ { _ => 1 }

  /** @return Parser[Money] */
  def deductItems = repsep(deductItem,",") ^^ { items =>
    items.foldLeft(Money(0)){_ + _}
  }

  def deductItem = deductKind ~> deductAmount

  def deductKind = tax | insurance | retirement

  def tax = fedState <~ "income" <~ "tax"

  def fedState = "federal" | "state"

  def insurance = "insurance" ~> "premiums"

  def retirement = "retirement" ~> "fund" ~> "contributions"

  def deductAmount = percentage | amount

  def percentage = toBe ~> doubleNumber <~ "percent" <~ "of" <~ "gross" ^^ { percentage =>
    grossAmount * Type2Money.double2Money(percentage / 100.0)
  }

  def amount = toBe ~> doubleNumber <~ "in" <~ "gross" <~ "currency" ^^ { Money(_) }

  def toBe = "is" | "are"

  def doubleNumber = floatingPointNumber ^^ { _.toDouble }

  def salaryForDays(days: Int) = (currentEmployee.annualGrossSalary / Type2Money.double2Money(260.0)) * Type2Money.int2Money(days)
}

object PayRollBuilder {

  def main(args: Array[String]) = {
    val buck = Employee(Name("Buck", "Trends"), Money(80000))
    val jane = Employee(Name("Jane", "Doe"), Money(90000))
    val employees = Map(buck.name -> buck, jane.name -> jane)

    val p = new PayrollParserCombinators(employees)

    args.foreach(file => {
      p.parseAll(p.paycheck, new FileReader(file)) match {
        case p.Success((employee, paycheck), _) =>
          print("%s %s: %s\n".format(employee.name.first, employee.name.last, paycheck))
        case x => print(x.toString)
      }
    })
  }
}
