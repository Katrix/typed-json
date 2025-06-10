package typedjson.codegen

case class ScalaDialect(
    writeUsing: Boolean,
    varargsSymbol: String
)
object ScalaDialect {
  val scala2: ScalaDialect = ScalaDialect(false, ": _*")
  val scala3: ScalaDialect = ScalaDialect(true, "*")
}
