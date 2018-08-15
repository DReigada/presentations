trait Serializer[A] {
  def toJson(a: A): String
}

implicit class SerializerOps[A](a: A)(implicit sh: Serializer[A]) {
  def toJson: String = sh.toJson(a)
}

implicit val intInstance: Serializer[Int] = int => int.toString
implicit val stringInstance: Serializer[String] = str => "\"" + str + "\""

implicit def mapInstance[A: Serializer]: Serializer[Map[String, A]] =
  _.toList
  .map {
    case (key, value) => key.toJson + " : " + value.toJson
  }
  .mkString("{\n", "\n", "\n}")

println("asdfas".toJson)


println(Map("aa"-> 1, "bb" -> 2).toJson)