import org.scalatest.matchers.should.Matchers
import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.OneInstancePerTest

/** @version created manually **/
class GradeSchoolTest extends AnyFunSuite with Matchers with OneInstancePerTest {

  val school = new School

  test ("empty school") {
    school.db should be (Map())
  }

  test ("add student") {
    school.add("Aimee", 2).db should be (Map(2 -> Seq("Aimee")))
  }

  test ("add more students in same class") {
    school.add("James", 2)
    .add("Blair", 2)
    .add("Paul", 2)
    .db should be (Map(2 -> Seq("James", "Blair", "Paul")))
  }

  test ("add students to different grades") {
    school.add("Chelsea", 3)
    .add("Logan", 7)
    .db should be (Map(3 -> Seq("Chelsea"), 7 -> Seq("Logan")))
  }

  test ("get students in a grade") {
    school.add("Franklin", 5)
    .add("Bradley", 5)
    .add("Jeff", 1)
    .grade(5) should be (Seq("Franklin", "Bradley"))
  }

  test ("get students in a non-existent grade") {
    school.grade(1) should be (Seq())
  }

  test ("sort school") {
    val sorted = Map(
      3 -> Seq("Kyle"),
      4 -> Seq("Christopher", "Jennifer"),
      6 -> Seq("Kareem")
    )

    val result = school.add("Jennifer", 4)
    .add("Kareem", 6)
    .add("Christopher", 4)
    .add("Kyle", 3)

    result.sorted should be (sorted)
    result.sorted.keys.toList should be (Seq(3, 4, 6))
  }
}
