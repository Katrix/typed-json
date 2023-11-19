# typed-json

Scala.js like facades for your JSON data.

## Deps

```scala
libraryDependencies += "net.katsstuff" %% "typed-json" % "0.1.0"

// Codegen module
libraryDependencies += "net.katsstuff" %% "typed-json-codegen" % "0.1.0"
```

## Usage

typed-json provides facades for a number of different things. Here are how to use them. All of these
have circe codecs defined for them.

### Objects

Make a class extending `JsonObject`, and a companion object implementing `JsonObjectCompanion`. In
the companion object, implement the function `makeRaw`, and any custom factory functions you want.
For the factory functions, you can use the function `makeRawFromFields` for circe like syntax.

In your class, implement your properties. For the accessors, have the implementation
call `selectDynamic[A]`. For `withX` functions, call `objWith` for types like `A` or `Option[A]`,
and call `objWithUndef` for types like `UndefOr[A]` or `JsonOption[A]`.

Example:

```scala
import typedjson._

class Foo(json: Json, startCache: Map[String, Any]) extends JsonObject(json, startCache) {

  def id: Int = selectDynamic[Int]("id")
  def withId(id: Int) = objWith(Foo, "id", id)

  def name: JsonOption[String] = selectDynamic[JsonOption[String]]("name")
  def withName(name: JsonOption[String]) = objWithUndef(Foo, "name", name)

  def tpe: String = selectDynamic[String]("type")
  def withTpe(tpe: String) = objWith(Foo, "type", tpe)
}

object Foo extends JsonObjectCompanion[Foo] {

  override def makeRaw(json: Json, cache: Map[String, Any]): Foo = new Foo(json, cache)

  def make(
    id: Int,
    name: JsonOption[String],
    tpe: String
  ): Foo = makeRawFromFields(
    "id" := id,
    "name" :=? name,
    "type" := tpe
  )
}

```

#### Retyping and extensions

If you want to cast one `JsonObject` to another `JsonObject`, use `retype` instead
of `asInstanceOf`. This can for example be useful when you have a type `A` that has all the fields
of another type `B`, plus a few more. When this happen, we call `A` an extension of `B`.

For extensions, you can use the function `extensionCache` to access a potentially populated cache to
give to an extension.

### Enums

Make a sealed case class extending `JsonEnum` with your desired type. Make a companion object and
extend `JsonEnumCompanion`, passing in your enum type. Next create the named instances for your
enum. Lastly implement the unknown function to deal with unknown enum values.

Example:

```scala
import typedjson._

sealed case class MyEnum(value: String) extends JsonEnum[String]

object MyEnum extends JsonEnumCompanion[String, MyEnum] {
  val MyEnumValue1: MyEnum = MyEnum("1")
  val MyEnumValueFoo: MyEnum = MyEnum("foo")

  override def unknown(s: String): MyEnum = MyEnum(s)
}

```

### Newtypes

Extend `JsonOpaqueCompanion`, specifying the underlying type.

Example:

```scala
import typedjson._

type Mytype = Mytype.OpaqueType

object MyType extends JsonOpaqueCompanion[String]
```

### Video guide

[Impromptu guide filed during Unconference](https://www.youtube.com/watch?v=QND8PxD_MIA)

## Codegen

If you'd rather not deal with the hassle of setting all of that up, a codegen module is also in the
work.
