import $ivy.`com.github.lolgab::mill-crossplatform::0.2.4`
import $ivy.`io.chris-kipp::mill-ci-release::0.1.9`
import com.github.lolgab.mill.crossplatform._
import io.kipp.mill.ci.release.CiReleaseModule
import mill._
import mill.scalajslib._
import mill.scalalib._
import mill.scalalib.publish._

val scalaVersions = Seq("2.12.18", "2.13.12", "3.3.1")

trait TypedJsonCommon extends SbtModule with CiReleaseModule {
  override def scalacOptions = T {
    super.scalacOptions() ++ {
      val majorVersion = scalaVersion().split('.').head.toInt

      if (majorVersion == 3)
        Seq(
          "-deprecation",
          "-feature",
          "-unchecked"
        )
      else
        Seq(
          "-deprecation",
          "-feature",
          "-unchecked",
          "-Xlint",
          "-Ywarn-dead-code",
          "-Xsource:3"
        )
    }
  }

  override def versionScheme: Target[Option[VersionScheme]] = Some(VersionScheme.SemVerSpec)

  def publishDescription: T[String]

  override def pomSettings: T[PomSettings] = T {
    PomSettings(
      description = publishDescription(),
      organization = "net.katsstuff",
      url = "https://github.com/Katrix/typed-json",
      licenses = Seq(License.MIT),
      versionControl = VersionControl.github("Katrix", "typed-json"),
      developers = Seq(
        Developer("Katrix", "Kathryn Frid", "http://katsstuff.net")
      )
    )
  }
}

trait TypedJsonJs extends SbtModule with ScalaJSModule {
  def scalaJSVersion = "1.14.0"
}

object `typed-json` extends Cross[TypedJsonModule](scalaVersions)
trait TypedJsonModule extends CrossPlatform { self =>
  trait Shared extends CrossPlatformCrossScalaModule with CrossSbtModule with TypedJsonCommon {

    override def ivyDeps: T[Agg[Dep]] = super.ivyDeps() ++ Agg(
      ivy"io.circe::circe-core::0.14.2",
      ivy"org.scala-lang.modules::scala-collection-compat::2.11.0"
    )

    override def publishDescription: T[String] = "Typed JSON without decoding it to a case class"
  }
  object jvm extends Shared
  object js  extends Shared with TypedJsonJs
}

object `typed-json-codegen` extends Cross[CodegenModule](scalaVersions)
trait CodegenModule extends CrossSbtModule with TypedJsonCommon {
  override def ivyDeps: T[Agg[Dep]] = super.ivyDeps() ++ Agg(
    ivy"io.circe::circe-yaml::0.14.2"
  )

  override def publishDescription: T[String] = "Codgen for typed-json"
}
