import java.util.concurrent.atomic.AtomicInteger

trait BankAccount {

  def closeAccount(): Unit

  def getBalance: Option[Int]

  def incrementBalance(increment: Int): Option[Int]
}

class BankAccountImpl extends BankAccount {
  private var balance: Option[AtomicInteger] = Some(new AtomicInteger(0))
  override def closeAccount(): Unit = balance = None
  override def getBalance: Option[Int] = balance.map(_.intValue())
  override def incrementBalance(increment: Int): Option[Int] =
    balance.map(_.addAndGet(increment))
}

object Bank {
  def openAccount(): BankAccount = new BankAccountImpl
}
