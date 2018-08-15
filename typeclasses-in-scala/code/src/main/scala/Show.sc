trait Show[A] {
  def show(a: A): String
}

object Show {
  def show[A](a: A)(implicit sh: Show[A]) = sh.show(a)
}

implicit class ShowOps[A](a: A)(implicit sh: Show[A]) {
  def show: String = sh.show(a)
}

implicit val intInstance = new Show[Int] {
  override def show(a: Int) = s"Int: $a"
}

println(intInstance.show(123))

println(Show.show(123))

123.show