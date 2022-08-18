import monocle.Lens

import java.time.LocalDate
import monocle.macros.GenLens


object LensPerson {
  case class Person(_name: Name, _born: Born, _address: Address)

  case class Name(_foreNames: String /*Space separated*/ , _surName: String)

  // Value of java.time.LocalDate.toEpochDay
  type EpochDay = Long

  case class Born(_bornAt: Address, _bornOn: EpochDay)

  case class Address(_street: String, _houseNumber: Int,
                     _place: String /*Village / city*/ , _country: String)

  // Valid values of Gregorian are those for which 'java.time.LocalDate.of'
  // returns a valid LocalDate.
  case class Gregorian(_year: Int, _month: Int, _dayOfMonth: Int)

  // Implement these.
  val bornStreetLens: Lens[Born, String] = GenLens[Born](_._bornAt._street)
  val currentStreetLens: Lens[Person, String] = GenLens[Person](_._address._street)

  val bornStreet: Born => String = born => bornStreetLens.get(born)

  val setCurrentStreet: String => Person => Person = currentStreetLens.


    newStreet => {
      person =>
        person.copy(_address = person._address.copy(_street = newStreet))
    }

    val setBirthMonth: Int => Person => Person = newBirthMonth => {
      person => {
        val birthday = LocalDate.ofEpochDay(person._born._bornOn)
        val newBirthday = LocalDate.of(birthday.getYear, newBirthMonth, birthday.getDayOfMonth)

        person.copy(_born = person._born.copy(_bornOn = newBirthday.toEpochDay))
      }
    }

    // Transform both birth and current street names.
    val renameStreets: (String => String) => Person => Person = f => { person =>
      val birthStreet = f(person._born._bornAt._street)
      val currentStreet = f(person._address._street)

      val newBornAt = person._born._bornAt.copy(_street = birthStreet)

      setCurrentStreet(currentStreet)(person).copy(_born = person._born.copy(_bornAt = newBornAt))
    }
}
