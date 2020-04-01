import io.circe.{Codec, Decoder, Encoder, Json}
import io.circe.generic.semiauto.{deriveEncoder, deriveCodec}
import io.circe.generic.extras.Configuration
import io.circe.yaml.Printer
import shapeless.{ Coproduct, Generic }
import enumeratum._

trait NoDescriminator {
  implicit def encodeAdtNoDiscr[A, Repr <: Coproduct](implicit
                                                      gen: Generic.Aux[A, Repr],
                                                      encodeRepr: Encoder[Repr]
                                                     ): Encoder[A] = encodeRepr.contramap(gen.to)

  implicit def decodeAdtNoDiscr[A, Repr <: Coproduct](implicit
                                                      gen: Generic.Aux[A, Repr],
                                                      decodeRepr: Decoder[Repr]
                                                     ): Decoder[A] = decodeRepr.map(gen.from)
}
object NoDescriminator extends NoDescriminator

sealed trait Event extends EnumEntry
case object Event extends Enum[Event] with CirceEnum[Event] {
  case object push extends Event
  case object release extends Event
  case object pull_request extends Event

  val values = findValues
}


sealed trait BuildStep
object BuildStep {
  implicit val buildStepCodec: Codec[BuildStep] = deriveCodec[BuildStep]
}
case class Uses(
  uses: String,
  `with`: Option[Map[String, String]]
) extends BuildStep

case class Name(
  name: String,
  run: String
)  extends BuildStep

case class Build(
  runsOn: String,
  steps: List[BuildStep]//,
)
object Build {
  import io.circe.generic.extras.semiauto.{deriveEncoder => _, deriveCodec => _, _}
  import NoDescriminator._
  implicit val customConfig: Configuration = Configuration.default.withKebabCaseMemberNames
  implicit val buildCodec: Codec[Build] = deriveConfiguredCodec[Build]
}
case class Job(build: Build)
object Job {
  implicit val jobCodec: Codec[Job] = deriveCodec[Job]
}
case class CI(
   name: String,
   on: List[Event] = List(Event.push),
   jobs: Job
)

object CI {
  implicit val ciCodec: Codec[CI] = deriveCodec[CI]
  def foo: Json = ciCodec(CI(name = "bob", jobs = Job(Build("ubuntu", List(Name("Build", "make install"), Uses("bob", None))))))
}

object Main {
  def main(args: Array[String]) = println(Printer(dropNullKeys = true) pretty CI.foo)
}