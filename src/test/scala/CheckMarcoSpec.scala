import com.softwaremill.quicklens._
import org.specs2.mutable._

case class Street(name: String)
case class Address(street: Option[Street])
case class Person(addresses: List[Address])

class CheckMarco extends Specification {
  val person: Person = Person(List(
    Address(Some(Street("1 Functional Rd."))),
    Address(Some(Street("2 Imperative Dr.")))
  ))

  "modify" >> {
    "works with inlined constant" in {
      val actual = person
        .modify(_.addresses.each.street.eachWhere(_.name.startsWith("1")).name)
        .using(_.toUpperCase)
      val expected =  Person(List(
        Address(Some(Street("1 FUNCTIONAL RD."))),
        Address(Some(Street("2 Imperative Dr.")))
      ))
      actual should beEqualTo(expected)
    }

    "works with variable" in {
      val one = "1"
      val actual = person
        .modify(_.addresses.each.street.eachWhere(_.name.startsWith(one)).name)
        .using(_.toUpperCase)
      val expected =  Person(List(
        Address(Some(Street("1 FUNCTIONAL RD."))),
        Address(Some(Street("2 Imperative Dr.")))
      ))
      actual should beEqualTo(expected)
    }
  }

}
