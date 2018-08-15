
trait Serializer[A] {
  def toJson(a: A): String
}

implicit class SerializerOps[A](a: A)(implicit sh: Serializer[A]) {
  def toJson: String = sh.toJson(a)
}

implicit val intInstance: Serializer[Int] = int => int.toString
implicit val stringInstance: Serializer[String] = str => "\"" + str + "\""

// composition
implicit def mapSerializer[B: Serializer]: Serializer[Map[String, B]] =
  _.toList
    .map {
      case (key, value) => key.toJson + " : " + value.toJson
    }
    .mkString("{\n", "\n", "\n}")


case class Student(name: String, number: Int)

// cloud be improved with macros
implicit val studentInstance: Serializer[Student] = student =>
  Map("name" -> student.name, "number" -> student.number.toJson).toJson


println(Student("Miguel", 1234).toJson)