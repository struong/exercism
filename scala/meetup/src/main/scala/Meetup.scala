import Schedule.Schedule

import java.time.temporal.TemporalAdjusters.{dayOfWeekInMonth, lastInMonth}
import java.time.{DayOfWeek, LocalDate}

case class Meetup(month: Int, year: Int) {

  private def dayOfMonth(dayOfWeek: Int, schedule: Schedule): Int = {
    schedule match {
      case Schedule.Teenth =>
        (13 to 19)
          .find(LocalDate.of(year, month, _).getDayOfWeek.getValue == dayOfWeek)
          .getOrElse(throw new RuntimeException("No teenth in this month"))
      case Schedule.Last =>
        LocalDate
          .of(year, month, 1)
          .`with`(lastInMonth(DayOfWeek.of(dayOfWeek)))
          .getDayOfMonth
      case schedule: Schedule =>
        LocalDate
          .of(year, month, 1)
          .`with`(dayOfWeekInMonth(schedule.id, DayOfWeek.of(dayOfWeek)))
          .getDayOfMonth
    }
  }

  def day(dayOfWeek: Int, schedule: Schedule): LocalDate = {
    val formattedMonth = f"$month%02d"
    val formattedDay = f"${dayOfMonth(dayOfWeek, schedule)}%02d"

    LocalDate.parse(s"$year-$formattedMonth-$formattedDay")
  }
}

object Schedule extends Enumeration {
  type Schedule = Value
  val Teenth, First, Second, Third, Fourth, Last = Value
}

object Meetup {
  val Mon = DayOfWeek.MONDAY.getValue
  val Tue = DayOfWeek.TUESDAY.getValue
  val Wed = DayOfWeek.WEDNESDAY.getValue
  val Thu = DayOfWeek.THURSDAY.getValue
  val Fri = DayOfWeek.FRIDAY.getValue
  val Sat = DayOfWeek.SATURDAY.getValue
  val Sun = DayOfWeek.SUNDAY.getValue
}
